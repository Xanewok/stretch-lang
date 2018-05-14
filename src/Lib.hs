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
    | Function ([Value] -> Value)
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

-- TODO: Move to Types to be used with TypedIdent Ident Type
type RecordFields = Map Ident (Set Ident)

type MyEnv = (Env, Store, RecordFields) -- TODO: reorganize record fields

type ReaderIO a = ReaderT MyEnv IO a
type StateIO a = StateT MyEnv IO a

alloc :: Store -> Loc
alloc m = if Map.null m then 0
          else let (i, w) = Map.findMax m in i+1

-- FIXME: expand stub impls
evalStmt :: Stm -> StateIO ()

-- TODO:
evalStmt stm @ (SStruct ident args) = do
    liftIO $ print stm -- DEBUG

    (_, _, fields) <- get -- DEBUG
    liftIO $ putStrLn $ "Fields before modifying: " ++ (show fields) -- DEBUG

    modify(\(e,s,f) ->
        let fieldIdents = map (\(TypedIdent ident _) -> ident) args in
            (e,s, Map.insert ident (Set.fromList fieldIdents) f))

    (_, _, fields) <- get -- DEBUG
    liftIO $ putStrLn $ "Fields after modifying: " ++ (show fields) -- DEBUG

evalStmt (SBlockExp blockExp) = evalBlockExp blockExp >> return ()

evalStmt (SExp expr) = do
    liftIO $ print expr -- DEBUG
    value <- evalExp expr
    liftIO $ print value -- DEBUG

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
        (Function f, Function g) -> fail "Can't compare function values"
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
-- evalExp(ECall Exp [Exp]) =

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
-- evalExp (EAnonFun AnonFunc) =

-- scopes (manages below?)
-- variable bindings -- holds values of a given type
-- function bindings -- holds function of a given type (no closures for now)

-- expressions with side effects - evaluating expressions yields us another state
-- evaluting from left to right




-- type Var = String

-- infixl 6 :+:, :-:
-- infixl 7 :*:, :/:

-- data Exp
--     = C Int        -- constant
--     | V Var        -- variable
--     | Exp :+: Exp  -- addition
--     | Exp :-: Exp  -- subtraction
--     | Exp :*: Exp  -- multiplication
--     | Exp :/: Exp  -- division

-- infix 1 :=

-- data Stmt
--     = Var := Exp      -- assignment
--     | While Exp Stmt  -- loop
--     | Seq [Stmt]      -- sequence

-- type Prog = Stmt

-- type Val = Int
-- type Store = [(Var, Val)]

-- eval :: Exp -> Store -> Val
-- eval (C n) r       = n
-- eval (V x) r       = case lookup x r of
--                        Nothing -> error ("unbound variable `" ++ x ++ "'")
--                        Just v  -> v
-- eval (e1 :+: e2) r = eval e1 r + eval e2 r
-- eval (e1 :-: e2) r = eval e1 r - eval e2 r
-- eval (e1 :*: e2) r = eval e1 r * eval e2 r
-- eval (e1 :/: e2) r = eval e1 r `div` eval e2 r

-- exec :: Stmt -> Store -> Store
-- exec (x := e) r                    = (x, eval e r) : r
-- exec (While e s) r | eval e r /= 0 = exec (Seq [s, While e s]) r
--                    | otherwise     = r
-- exec (Seq []) r                    = r
-- exec (Seq (s : ss)) r              = exec (Seq ss) (exec s r)

-- run :: Prog -> Store -> Store
-- run p r = nubBy ((==) `on` fst) (exec p r)

-- fib :: Prog
-- fib = Seq
--   [ "x" := C 0
--   , "y" := C 1
--   , While (V "n") $ Seq
--       [ "z" := V "x" :+: V "y"
--       , "x" := V "y"
--       , "y" := V "z"
--       , "n" := V "n" :-: C 1
--       ]
--   ]

-- main = print $ lookup "x" $ run fib [("n", 25)]