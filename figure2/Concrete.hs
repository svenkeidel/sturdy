module Concrete where

import           Data.Map (Map)
import qualified Data.Map as M

type Val = Int
type Env = Map String Val
data Expr = Var String | Lit Int | Add Expr Expr | IfZero Expr Expr Expr | TryZero Expr Expr Expr

eval :: Expr -> Env -> Maybe Val
eval e env = case e of
  Var x -> M.lookup x env
  Lit n -> return n
  Add e1 e2 -> do
    v1 <- eval e1 env
    v2 <- eval e2 env
    return (v1 + v2)
  IfZero e1 e2 e3 -> do
    v <- eval e1 env
    if v == 0
    then eval e2 env
    else eval e3 env
  TryZero e1 e2 e3 -> case eval e1 env of
    Just v  | v == 0    -> eval e2 env
            | otherwise -> eval e3 env
    Nothing             -> eval e3 env
