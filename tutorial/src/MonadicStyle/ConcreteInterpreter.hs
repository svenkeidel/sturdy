{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
module MonadicStyle.ConcreteInterpreter where

import           Control.Monad

import           Data.Map (Map)
import qualified Data.Map as Store

import           Syntax

data Val = BoolVal Bool | NumVal Int deriving (Show,Eq)
type Store = Map String Val

newtype M a = M {runM :: Store -> Either String (Store,a)}

-- eval :: Store -> Expr -> Either String Val
eval :: Expr -> M Val
eval e = case e of
  Var x _ -> do
    st <- get
    case Store.lookup x st of
      Just v -> return v
      Nothing -> throw "Variable not in scope"
  NumLit n _ -> return (NumVal n)
  Add e1 e2 _ -> do
    v1 <- eval e1
    v2 <- eval e2
    case (v1,v2) of
      (NumVal n1, NumVal n2) -> return (NumVal (n1 + n2))
      (_,_) -> throw "Expected two numbers as arguments for +"
  BoolLit b _ -> return (BoolVal b)
  And e1 e2 _ -> do
    v1 <- eval e1
    v2 <- eval e2
    case (v1,v2) of
      (BoolVal b1, BoolVal b2) -> return (BoolVal (b1 && b2))
      (_,_) -> throw "Expected two booleans as arguments for &&"
  Lt e1 e2 _ -> do
    v1 <- eval e1
    v2 <- eval e2
    case (v1,v2) of
      (NumVal n1, NumVal n2) -> return (BoolVal (n1 < n2))
      (_,_) -> throw "Expected two numbers as arguments for +"

get :: M Store
get = M (\st -> Right (st,st))

put :: Store -> M ()
put st = M (\_ -> Right (st,()))

throw :: String -> M a
throw er = M (\_ -> Left er)

-- run :: Store -> [Statement] -> Either String Store
run :: [Statement] -> M ()
run stmts = case stmts of
  (Assign x e _ : rest) -> do
    v <- eval e
    st <- get
    put (Store.insert x v st)
    run rest
  (If cond ifBranch elseBranch _ : rest) -> do
    v <- eval cond
    case v of
      BoolVal True -> run ifBranch
      BoolVal False -> run elseBranch
      NumVal _ -> throw "Expected a boolean expression as condition for an if"
    run rest
  (While cond body l : rest) ->
    run (If cond (body ++ [While cond body l]) [] l : rest)
  [] ->
    return ()

deriving instance Functor M


-- do
--  v1 <- eval e1
--  v2 <- eval e2
--  ... 
--
--  eval e1 >>= (\v1 -> eval e2 ....)

-- (>>=) :: M a -> (a -> M b) -> M b
instance Monad M where
  return a = M (\st -> Right (st,a))
  M m >>= k = M $ \st ->
    case m st of
      Left er -> Left er
      Right (st',x) -> runM (k x) st'

instance Applicative M where
  pure = return
  (<*>) = ap
