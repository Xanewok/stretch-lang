module Lib
    ( interpret
    ) where

import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Text.Show.Functions

import BNFC.AbsStretch
import BNFC.PrintStretch

interpret :: Program -> IO ()
interpret (ProgramEntry stmts) =
    evalStateT (interpretStmts stmts) initialState

interpretStmts :: [Stm] -> StateIO ()
interpretStmts stmts = mapM_ evalStmt stmts

initialState :: MyEnv
initialState = (Map.empty, Map.empty, Map.empty)

toBool :: Boolean -> Bool
toBool Boolean_true = True
toBool Boolean_false = False

litToValue :: Literal -> Value
litToValue lit = case lit of
    LiteralUnit -> Unit ()
    LiteralBoolean bool -> Boolean (toBool bool)
    LiteralInteger int -> Int int
    LiteralString str -> Str str

data Value
    = Unit ()
    | Boolean Bool
    | Int Integer
    | Str String
    | Record (Map Ident Value)
    | Function Env Block
    deriving Show

instance Eq Value where
    Unit () == Unit () = True
    Boolean b1 == Boolean b2 = b1 == b2
    Int i1 == Int i2 = i1 == i2
    Str s1 == Str s2 = s1 == s2
    Record r1 == Record r2 = r1 == r2 -- TODO: Structural equivalence? Need to include type system info


type Loc = Int

type Env = Map Ident Loc
type Store = Map Loc Value

-- TODO: Unify other decls with types
type FuncDecl = Map Ident ([FormalArg], Type)

type MyEnv = (Env, Store, RecordFields) -- TODO: remove and replace with type definitions
-- TODO: Move to Types to be used with TypedIdent Ident Type
type RecordFields = Map Ident (Set Ident)

type ReaderIO a = ReaderT MyEnv IO a
type StateIO a = StateT MyEnv IO a

alloc :: Store -> Loc
alloc m = if Map.null m then 0
          else let (i, w) = Map.findMax m in i+1

allocWithVal :: Ident -> Value -> StateIO Loc
allocWithVal ident value = do
    (_, store, _) <- get

    let newloc = alloc store in do
        modify (\(env, store, fields) ->
            (Map.insert ident newloc env,
                Map.insert newloc value store,
                fields))
        return newloc

evalStmt :: Stm -> StateIO ()

-- TODO
evalStmt (SFunc ident args block) = do
    -- TODO: Introduce new type definition for the function

    -- Crucial for implementing recursion - allocate an identifier in the env
    -- first for ourself, so the stored function value can refer to itself
    loc <- allocWithVal ident (Unit ())

    -- Replace the dummy value with our function, storing previously modified env
    modify(\(env, store, fields) ->
        let func = Function env block in
            let store' = Map.insert loc func store in
                (env, store', fields)
        )

-- TODO: Implement and verify type mismatch
evalStmt (SFuncRet ident args typ block) = evalStmt (SFunc ident args block)
evalStmt stm @ (SStruct ident args) = do
    liftIO $ print stm -- DEBUG

    (_, _, fields) <- get -- DEBUG
    liftIO $ putStrLn $ "Fields before modifying: " ++ (show fields) -- DEBUG

    modify(\(e,s,f) ->
        let fieldIdents = map (\(TypedIdent ident _) -> ident) args in
            (e,s, Map.insert ident (Set.fromList fieldIdents) f))

    (_, _, fields) <- get -- DEBUG
    liftIO $ putStrLn $ "Fields after modifying: " ++ (show fields) -- DEBUG

evalStmt stm @ (SLet ident expr) = do
    liftIO $ print stm -- DEBUG

    value <- evalExp expr

    (env, store, fields) <- get -- DEBUG
    liftIO $ print env -- DEBUG
    liftIO $ print store -- DEBUG

    modify (\(env, store, fields) ->
        let newloc = alloc store in
            (Map.insert ident newloc env,
             Map.insert newloc value store,
             fields)
        )

    (env, store, fields) <- get -- DEBUG
    liftIO $ print env -- DEBUG
    liftIO $ print store -- DEBUG

-- TODO: Check type mismatch
evalStmt (SLetType ident typ expr) = evalStmt (SLet ident expr)

evalStmt (SBlockExp blockExp) = evalBlockExp blockExp >> return ()

evalStmt (SExp expr) = do
    liftIO $ print expr -- DEBUG
    value <- evalExp expr
    liftIO $ print value -- DEBUG

-- TODO: Scopes with STATIC binding (create a fresh copy for nested exprs)
evalBlock :: Block -> StateIO Value
evalBlock (Block1 stmts) = interpretStmts stmts >> (return $ Unit ())
evalBlock (Block2 stmts expr) = interpretStmts stmts >> (evalExp expr)

evalBlockExp :: BlockExp -> StateIO Value
evalBlockExp (EBlock block) = evalBlock block
-- TODO: reset block env after quitting blocks
evalBlockExp (EIf expr block) = do
    value <- evalExp expr
    case value of
        Boolean val -> if val then evalBlock block else return $ Unit ()
        _ -> fail "mismatched types: `if` guard clause not a boolean"
-- TODO: reset block env after quitting blocks
evalBlockExp (EIfElse expr ifBlock elseBlock) = do
    value <- evalExp expr
    case value of
        Boolean val -> evalBlock $ if val then ifBlock else elseBlock
        _ -> fail "mismatched types: `if` guard clause not a boolean"
-- TODO: reset block env after quitting blocks
evalBlockExp while @ (EWhile expr block) = do
    value <- evalExp expr
    case value of
        Boolean val -> if val then evalBlockExp while else return $ Unit ()
        _ -> fail "mismatched types: `if` guard clause not a boolean"

evalExp :: Exp -> StateIO Value
evalExp (EAssign ident expr) = do
    value <- evalExp expr
    liftIO $ print value -- DEBUG

    (env, store, fields) <- get -- DEBUG
    loc <- case Map.lookup ident env of
        Just x -> return x
        Nothing -> fail $ "trying to assign to an unbound variable: " ++ (show ident)

    modify(\(env, store, fields) ->
        (env, Map.insert loc value store, fields))

    (env, store, fields) <- get -- DEBUG
    liftIO $ print env -- DEBUG
    liftIO $ print store -- DEBUG

    return $ Unit ()

evalExp (EOr e1 e2) = do
    value1 <- evalExp e1
    value2 <- evalExp e2

    case (value1, value2) of
        (Boolean bool1, Boolean bool2) -> return $ Boolean (bool1 || bool2)
        _ -> fail "Incompatible types"

evalExp (EAnd e1 e2) = do
    value1 <- evalExp e1
    value2 <- evalExp e2

    case (value1, value2) of
        (Boolean bool1, Boolean bool2) -> return $ Boolean (bool1 && bool2)
        _ -> fail "Incompatible types"

evalExp (EEq e1 e2) = do
    value1 <- evalExp e1
    value2 <- evalExp e2

    val <- case (value1, value2) of
        (Function _ _, Function _ _) -> fail "Can't compare function values"
        (a, b) -> return $ a == b

    return $ Boolean val

evalExp (ENEq e1 e2) = do
    (Boolean val) <- evalExp (EEq e1 e2)
    return $ Boolean (not val)

-- TODO: Refactor binary ops
evalExp (ELess e1 e2) = do
    value1 <- evalExp e1
    value2 <- evalExp e2

    case (value1, value2) of
        (Int int1, Int int2) -> return $ Boolean (int1 < int2)
        _ -> fail "Incompatible types"

evalExp (ELEq e1 e2) = do
    value1 <- evalExp e1
    value2 <- evalExp e2

    case (value1, value2) of
        (Int int1, Int int2) -> return $ Boolean (int1 <= int2)
        _ -> fail "Incompatible types"

evalExp (EGreat e1 e2) = do
    value1 <- evalExp e1
    value2 <- evalExp e2

    case (value1, value2) of
        (Int int1, Int int2) -> return $ Boolean (int1 > int2)
        _ -> fail "Incompatible types"

evalExp (EGEq e1 e2) = do
    value1 <- evalExp e1
    value2 <- evalExp e2

    case (value1, value2) of
        (Int int1, Int int2) -> return $ Boolean (int1 >= int2)
        _ -> fail "Incompatible types"

evalExp (EPlus e1 e2) = do
    value1 <- evalExp e1
    value2 <- evalExp e2

    case (value1, value2) of
        (Int int1, Int int2) -> return $ Int (int1 + int2)
        _ -> fail "Incompatible types"

evalExp (EMinus e1 e2) = do
    value1 <- evalExp e1
    value2 <- evalExp e2

    case (value1, value2) of
        (Int int1, Int int2) -> return $ Int (int1 - int2)
        _ -> fail "Incompatible types"

evalExp (EMul e1 e2) = do
    value1 <- evalExp e1
    value2 <- evalExp e2

    case (value1, value2) of
        (Int int1, Int int2) -> return $ Int (int1 * int2)
        _ -> fail "Incompatible types"

evalExp (ENot e) = do
    value <- evalExp e

    case value of
        Boolean bool -> return $ Boolean (not bool)
        _ -> fail "Incompatible types"

evalExp (ENeg e) = do
    value <- evalExp e

    case value of
        Int int -> return $ Int (-int)
        _ -> fail "Incompatible types"

evalExp (EDiv e1 e2) = do
    value1 <- evalExp e1
    value2 <- evalExp e2

    case (value1, value2) of
        (Int int1, Int 0) -> fail "Division by 0"
        (Int int1, Int int2) -> return $ Int (int1 `div` int2)
        _ -> fail "Incompatible types"

evalExp (ELit lit) = return (litToValue lit)

evalExp (EStruct ident members) = do
    (env, store, fields) <- get
    -- Just make sure the type is okay and structure is declared
    -- TODO: Ensure the types are correct
    _ <- case Map.lookup ident fields of
        Nothing -> fail $ "Struct of type " ++ (show ident) ++ " is not declared"
        Just fields ->
            let passedFields = map (\(MemberExp ident _) -> ident) members in
                let declaredFields = Set.toList fields in
                    if passedFields == declaredFields then return fields
                    else fail $ "Mismatched members in struct definition"

    struct <- sequence $ map (\(MemberExp ident e) -> mapStateT (liftM (\(val,s) -> ((ident,val),s))) $ evalExp e) members
    liftIO $ putStrLn $ "Evaluated struct: " ++ (show struct) -- DEBUG

    return $ Record (Map.fromList struct)

evalExp (ECall funcExp args) = do
    func <- evalExp funcExp
    Function funcEnv block <- case func of
        f @(Function _ _) -> return f
        _ -> fail $ "Expression " ++ (show evalExp) ++ " is not a function"

    -- TODO: Check types of called function and arguments
    evaluatedArgs <- mapM evalExp args
    -- TODO: pass args - needs modifying environment - needs still knowing func type (idents)

    (env, store, fields) <- get

    -- Evaluate function using function's static binding environment
    (result, (_, changedStore, _)) <- liftIO $ runStateT (evalBlock block) (funcEnv, store, fields)

    -- Functions can modify state - store modified state, resulting from evaluating the function call
    modify(\(env, store, fields) -> (env, changedStore, fields))

    return result

evalExp (EIdent ident) = do
    (env, store, fields) <- get -- DEBUG
    liftIO $ print env -- DEBUG
    liftIO $ print store -- DEBUG

    case Map.lookup ident env of
        Nothing -> fail $ "variable not in scope: " ++ (show ident)
        Just loc -> case Map.lookup loc store of
            Nothing -> fail $ "unreachable"
            Just value -> do
                liftIO $ putStrLn $ "Ident `" ++ (show ident) ++ "` point at value: " ++ (show value) -- DEBUG
                return value

evalExp (EPrint expr) = do
    value <- evalExp expr
    liftIO $ print (show value)
    return $ Unit ()

evalExp (EField expr ident) = do
    value <- evalExp expr

    case value of
        Record r -> case Map.lookup ident r of
            Just value -> return value
            Nothing -> fail $ (show ident) ++ " is not a field on structure " ++ (show r)
        _ -> fail $ (show expr) ++ " is not a record"
evalExp (EBlockExp blockExp) = evalBlockExp blockExp

-- TODO Check types, make sure this is okay
evalExp (EAnonFun (AnonEmpty block)) = evalExp (EAnonFun (AnonArgs [] block))
evalExp (EAnonFun (AnonArgs args block)) = do
    (env, _, _) <- get

    return $ Function env block
