module Main where

import Data.Function
import Data.List

import Lib

type Var = String

infixl 6 :+:, :-:
infixl 7 :*:, :/:

data Exp
    = C Int        -- constant
    | V Var        -- variable
    | Exp :+: Exp  -- addition
    | Exp :-: Exp  -- subtraction
    | Exp :*: Exp  -- multiplication
    | Exp :/: Exp  -- division

infix 1 :=

data Stmt
    = Var := Exp      -- assignment
    | While Exp Stmt  -- loop
    | Seq [Stmt]      -- sequence

type Prog = Stmt

type Val = Int
type Store = [(Var, Val)]

eval :: Exp -> Store -> Val
eval (C n) r       = n
eval (V x) r       = case lookup x r of
                       Nothing -> error ("unbound variable `" ++ x ++ "'")
                       Just v  -> v
eval (e1 :+: e2) r = eval e1 r + eval e2 r
eval (e1 :-: e2) r = eval e1 r - eval e2 r
eval (e1 :*: e2) r = eval e1 r * eval e2 r
eval (e1 :/: e2) r = eval e1 r `div` eval e2 r

exec :: Stmt -> Store -> Store
exec (x := e) r                    = (x, eval e r) : r
exec (While e s) r | eval e r /= 0 = exec (Seq [s, While e s]) r
                   | otherwise     = r
exec (Seq []) r                    = r
exec (Seq (s : ss)) r              = exec (Seq ss) (exec s r)

run :: Prog -> Store -> Store
run p r = nubBy ((==) `on` fst) (exec p r)

fib :: Prog
fib = Seq
  [ "x" := C 0
  , "y" := C 1
  , While (V "n") $ Seq
      [ "z" := V "x" :+: V "y"
      , "x" := V "y"
      , "y" := V "z"
      , "n" := V "n" :-: C 1
      ]
  ]

main :: IO ()
main = print $ lookup "x" $ run fib [("n", 25)]