{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
module MonadicStyle.AbstractInterpreter where

import Prelude hiding (True,False)
import qualified Prelude as P

import           Control.Monad

import           Data.Map (Map)
import qualified Data.Map as Store

import           Syntax

data Val = BoolVal AbsBool | NumVal Interval | TopVal
data AbsBool = True | False | TopBool
data Interval = Interval Int Int
type Store = Map String Val

-- eval :: Store -> Expr -> Either String Val
eval :: Expr -> M Val
newtype M a = M {runM :: Store -> Either String (Store,a)}

eval e = case e of
  Var x _ -> do
    st <- get
    case Store.lookup x st of
      Just v -> return v
      Nothing -> throw "Variable not in scope"
  NumLit n _ -> return (NumVal (Interval n n))
  Add e1 e2 _ -> do
    v1 <- eval e1
    v2 <- eval e2
    case (v1,v2) of
      (NumVal (Interval i1 j1), NumVal (Interval i2 j2)) ->
        return (NumVal (Interval (i1 + i2) (j1 + j2)))
      (_,_) -> throw "Expected two numbers as arguments for +"
  BoolLit b _ -> case b of
    P.True -> return (BoolVal True)
    P.False -> return (BoolVal False)
  And e1 e2 _ -> do
    v1 <- eval e1
    v2 <- eval e2
    case (v1,v2) of
      (BoolVal b1, BoolVal b2) -> return $ BoolVal $ case (b1,b2) of
        (False,_) -> False
        (_,False) -> False
        (True,_) -> b2
        (_,True) -> b1
        (_,_) -> TopBool
      (TopVal, _) -> return TopVal
      (_,TopVal) -> return TopVal
      (_,_) -> throw "Expected two booleans as arguments for &&"
  Lt e1 e2 _ -> do
    v1 <- eval e1
    v2 <- eval e2
    case (v1,v2) of
      (NumVal (Interval i1 j1), NumVal (Interval i2 j2))
        | j1 < i2   -> return (BoolVal True) 
        | j2 < i1   -> return (BoolVal False) 
        | otherwise -> return (BoolVal TopBool) 
      (TopVal, _) -> return TopVal
      (_,TopVal) -> return TopVal
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
      BoolVal TopBool -> run ifBranch ⊔ run elseBranch
      TopVal -> top
      NumVal _ -> throw "Expected a boolean expression as condition for an if"
    run rest
  (While cond body l : rest) ->
    run (If cond (body ++ [While cond body l]) [] l : rest)
  [] ->
    return ()

deriving instance Functor M

instance Monad M where
  return a = M (\st -> Right (st,a))
  M m >>= k = M $ \st ->
    case m st of
      Left er -> Left er
      Right (st',x) -> runM (k x) st'

instance Applicative M where
  pure = return
  (<*>) = ap

(⊔) = undefined
top = undefined
