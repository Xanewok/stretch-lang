module Lib
    ( interpret
    ) where

import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.State

import Data.Map (Map)
import Data.Typeable(Typeable)
import qualified Data.Map as Map

import Text.Show.Functions

import BNFC.AbsStretch
import BNFC.PrintStretch

interpret :: Program -> IO ()
interpret (ProgramEntry stmts) =
    evalStateT (interpretStmts stmts) initialState

interpretStmts :: [Stm] -> StateIO ()
interpretStmts stmts = mapM_ evalStmt stmts

initialState :: MyEnv
initialState = (Map.empty, Map.empty)

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

type Loc = Int

type Env = Map Ident Loc
type Store = Map Loc Value

type MyEnv = (Env, Store)

type ReaderIO a = ReaderT MyEnv IO a
type StateIO a = StateT MyEnv IO a

alloc :: Store -> Loc
alloc m = if Map.null m then 0
          else let (i, w) = Map.findMax m in i+1

-- FIXME: expand stub impls
evalStmt :: Stm -> StateIO ()

evalStmt (SBlockExp blockExp) = evalBlockExp blockExp >> return ()

evalStmt (SExp expr) = do
    liftIO $ print expr -- DEBUG
    value <- evalExp expr
    liftIO $ print value -- DEBUG

evalStmt stm @ (SLet ident expr) = do
    liftIO $ print stm -- DEBUG

    value <- evalExp expr

    (env, store) <- get -- DEBUG
    liftIO $ print env -- DEBUG
    liftIO $ print store -- DEBUG

    modify (\(env, store) ->
        let newloc = alloc store in
            (Map.insert ident newloc env,
             Map.insert newloc value store)
        )

    (env, store) <- get -- DEBUG
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
evalExp (ELit lit) = return (litToValue lit)

evalExp (EDiv e1 e2) = do
    value1 <- evalExp e1
    value2 <- evalExp e2

    case (value1, value2) of
        (Int int1, Int 0) -> fail "Division by 0"
        (Int int1, Int int2) -> return (Int (int1 `div` int2))
        _ -> fail "Incompatible types"

evalExp (EBlockExp blockExp) = evalBlockExp blockExp

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