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

import Util(debugPutStrLn)
import BNFC.AbsStretch
import BNFC.PrintStretch
import Typeck(typeck)

interpret :: Program -> IO ()
interpret program = do
    typedStmts <- case typeck program of
        Left msg -> fail $ "typeck error: \n" ++ msg
        Right (ProgramEntry stmts) -> return stmts

    evalStateT (interpretStmts typedStmts) initialState

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
    | Function Env [FormalArg] Block

instance Eq Value where
    Unit () == Unit () = True
    Boolean b1 == Boolean b2 = b1 == b2
    Int i1 == Int i2 = i1 == i2
    Str s1 == Str s2 = s1 == s2
    Record r1 == Record r2 = r1 == r2 -- Types are checked during typeck
    _ == _ = False

instance Show Value where
    show (Unit inner) =  show inner
    show (Boolean inner) = show inner
    show (Int inner) = show inner
    show (Str inner) = inner
    show (Record inner) = show inner
    show (Function env args block) = (show env) ++ " " ++ (show args) ++ " " ++ (show block)

type Loc = Int

type Env = Map Ident Loc
type Store = Map Loc Value
type StructDecl = Map Ident [FormalArg]

type MyEnv = (Env, Store, StructDecl)

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

-- Given an initial state, runs a StateT in a local variable binding environment
runInLocalEnv :: StateIO a -> MyEnv -> StateIO a
runInLocalEnv st initial = do
    (result, (_, store', _)) <- liftIO $ runStateT st initial

    modify(\(env, store, fields) -> (env, store', fields))

    return result

evalStmt :: Stm -> StateIO ()

evalStmt (SFunc ident args block) = do
    -- Crucial for implementing recursion - allocate an identifier in the env
    -- first for ourself, so the stored function value can refer to itself
    loc <- allocWithVal ident (Unit ())

    -- Replace the dummy value with our function, storing previously modified env
    modify(\(env, store, fields) ->
        let func = Function env args block in
            let store' = Map.insert loc func store in
                (env, store', fields)
        )

-- Type mismatch detected during typeck
evalStmt (SFuncRet ident args typ block) = evalStmt (SFunc ident args block)
evalStmt stm @ (SStruct ident args) = do
    (_, _, fields) <- get

    liftIO $ debugPutStrLn $ "Fields before modifying: " ++ (show fields)

    modify (\(e,s,f) -> (e,s, Map.insert ident args f))

    (_, _, fields) <- get
    liftIO $ debugPutStrLn $ "Fields after modifying: " ++ (show fields)

evalStmt stm @ (SLet ident expr) = do
    value <- evalExp expr

    (env, store, fields) <- get
    liftIO $ debugPutStrLn $ show env
    liftIO $ debugPutStrLn $ show store

    modify (\(env, store, fields) ->
        let newloc = alloc store in
            (Map.insert ident newloc env,
             Map.insert newloc value store,
             fields)
        )

    (env, store, fields) <- get
    liftIO $ debugPutStrLn $ show env
    liftIO $ debugPutStrLn $ show store

-- Type mismatch detected during typeck
evalStmt (SLetType ident typ expr) = evalStmt (SLet ident expr)

evalStmt (SBlockExp blockExp) = evalBlockExp blockExp >> return ()

evalStmt (SExp expr) = do
    liftIO $ debugPutStrLn $ show expr
    value <- evalExp expr
    liftIO $ debugPutStrLn $ show value

evalBlock :: Block -> StateIO Value
evalBlock (Block1 stmts) = do
    st <- get

    runInLocalEnv (interpretStmts stmts >> (return $ Unit ())) st


evalBlock (Block2 stmts expr) = do
    st <- get

    runInLocalEnv (interpretStmts stmts >> evalExp expr) st


evalBlockExp :: BlockExp -> StateIO Value
evalBlockExp (EBlock block) = evalBlock block
evalBlockExp (EIf expr block) = do
    value <- evalExp expr
    case value of
        Boolean val -> if val then evalBlock block else return $ Unit ()
        _ -> fail "mismatched types: `if` guard clause not a boolean"

evalBlockExp (EIfElse expr ifBlock elseBlock) = do
    value <- evalExp expr
    case value of
        Boolean val -> evalBlock $ if val then ifBlock else elseBlock
        _ -> fail "mismatched types: `if` guard clause not a boolean"
evalBlockExp while @ (EWhile expr block) = do
    value <- evalExp expr
    case value of
        Boolean val ->
            if val then evalBlock block >> evalBlockExp while
                   else return $ Unit ()
        _ -> fail "mismatched types: `if` guard clause not a boolean"

evalExp :: Exp -> StateIO Value
evalExp (EAssign ident expr) = do
    value <- evalExp expr
    liftIO $ debugPutStrLn $ show value

    (env, store, fields) <- get
    loc <- case Map.lookup ident env of
        Just x -> return x
        Nothing -> fail $ "trying to assign to an unbound variable: " ++ (show ident)

    modify(\(env, store, fields) ->
        (env, Map.insert loc value store, fields))

    (env, store, fields) <- get
    liftIO $ debugPutStrLn $ show env
    liftIO $ debugPutStrLn $ show store

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
        (Function _ _ _, Function _ _ _) -> fail "Can't compare function values"
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

evalExp (EIdent ident) = do
    (env, store, fields) <- get
    liftIO $ debugPutStrLn (show env)
    liftIO $ debugPutStrLn (show store)

    case Map.lookup ident env of
        Nothing -> fail $ "variable not in scope: " ++ (show ident)
        Just loc -> case Map.lookup loc store of
            Nothing -> fail $ "unreachable"
            Just value -> do
                liftIO $ debugPutStrLn $ "Ident `" ++ (show ident) ++ "` point at value: " ++ (show value)
                return value

evalExp (EStruct ident members) = do
    (env, store, fields) <- get
    -- Just make sure the type is okay and structure is declared
    -- TODO: Ensure the types are correct
    _ <- case Map.lookup ident fields of
        Nothing -> fail $ "Struct of type " ++ (show ident) ++ " is not declared"
        Just fields ->
            let passedFields = map (\(MemberExp ident _) -> ident) members in
                let declaredFields = map (\(TypedIdent ident typ) -> ident) fields in
                    if passedFields == declaredFields then return declaredFields
                    else fail $ "Mismatched members in struct definition"

    struct <- sequence $ map (\(MemberExp ident e) -> mapStateT (liftM (\(val,s) -> ((ident,val),s))) $ evalExp e) members
    liftIO $ debugPutStrLn $ "Evaluated struct: " ++ (show struct)

    return $ Record (Map.fromList struct)

evalExp (ECall funcExp args) = do
    func <- evalExp funcExp
    Function funcEnv formalArgs block <- case func of
        f @(Function _ _ _) -> return f
        _ -> fail $ "Expression " ++ (show funcExp) ++ " is not a function"

    -- Function argument type checking done during typeck
    evaluatedArgs <- mapM evalExp args

    let args = zip evaluatedArgs formalArgs in
        let allocFuncArgs = (\(value, (TypedIdent ident _)) -> allocWithVal ident value) in
            do
                (env, store, fields) <- get

                liftIO $ debugPutStrLn ""
                liftIO $ debugPutStrLn $ "Env before running:\n" ++ (show env)

                tmp <- runInLocalEnv (mapM_ allocFuncArgs args >> evalBlock block)
                    (funcEnv, store, fields)

                liftIO $ debugPutStrLn ""
                liftIO $ debugPutStrLn $ "Env after running:\n" ++ (show env)

                return tmp

evalExp (EPrint expr) = do
    value <- evalExp expr
    liftIO $ putStrLn (show value)
    return $ Unit ()

evalExp (EField expr ident) = do
    value <- evalExp expr

    case value of
        Record r -> case Map.lookup ident r of
            Just value -> return value
            Nothing -> fail $ (show ident) ++ " is not a field on structure " ++ (show r)
        _ -> fail $ (show expr) ++ " is not a record"
evalExp (EBlockExp blockExp) = evalBlockExp blockExp

evalExp (EAnonFun (AnonEmpty block)) = evalExp (EAnonFun (AnonArgs [] block))
evalExp (EAnonFun (AnonArgs args block)) = do
    (env, _, _) <- get

    return $ Function env args block

evalExp (ETyped expr _) = evalExp expr
