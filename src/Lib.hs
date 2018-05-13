module Lib
    ( interpret
    ) where

import Data.Map (Map)
import Data.Typeable(Typeable)
import qualified Data.Map as Map

import BNFC.AbsStretch
import BNFC.PrintStretch

interpret :: Program -> IO ()
interpret (ProgramEntry stmts) =
    mapM_ (`evalStmt` initialState) stmts


toBool :: Boolean -> Bool
toBool Boolean_true = True
toBool Boolean_false = False

litToValue :: Literal -> Value
litToValue lit = case lit of
    LiteralUnit -> Unit ()
    LiteralBoolean bool -> Boolean (toBool bool)
    LiteralInteger int -> Int int
    LiteralString str -> Str str

type Loc = Int

-- FIXME: Loc -> actual value
data Value
    = Unit ()
    | Boolean Bool
    | Int Integer
    | Str String
    deriving Show

type Env = Map String Loc
type Store = Map Loc Value

type State = (Env, Store) -- FIXME: correct?
initialState :: State
initialState = (Map.empty, Map.empty)

alloc :: Store -> Loc
alloc m = if Map.null m then 0
          else let (i, w) = Map.findMax m in i+1

-- FIXME: expand stub impls
evalStmt :: Stm -> State -> IO State
evalStmt (SExp expr) state = do
    print expr -- DEBUG
    (state, value) <- (evalExp expr state)
    print value -- DEBUG
    return state

evalExp :: Exp -> State -> IO (State, Value)
evalExp (ELit lit) state = return (state, litToValue lit)

evalExp (EDiv e1 e2) state0 = do
    (state1, value1) <- evalExp e1 state0
    (state2, value2) <- evalExp e2 state1

    case (value1, value2) of
        (Int int1, Int 0) -> fail "Division by 0"
        (Int int1, Int int2) -> return (state2, Int (int1 `div` int2))
        _ -> fail "Incompatible types"

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