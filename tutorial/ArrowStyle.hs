{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
module MonadicStyle where

import           Control.Category
import           Control.Arrow

import           Data.Map (Map)
import qualified Data.Map as Store

import           Syntax

data Val = BoolVal Bool | NumVal Int deriving (Show,Eq)
type Store = Map String Val

newtype Arr x y = Arr {runArr :: x -> Store -> Either String (Store,y)}

-- eval :: Store -> Expr -> Either String Val
eval :: Arr Expr Val
eval = proc e -> case e of
  Var x -> do
    st <- get -< ()
    case Store.lookup x st of
      Just v -> returnA -< v
      Nothing -> throw -< "Variable not in scope"
  NumLit n -> returnA -< (NumVal n)
  Add e1 e2 -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    case (v1,v2) of
      (NumVal n1, NumVal n2) -> returnA -< (NumVal (n1 + n2))
      (_,_) -> throw -< "Expected two numbers as arguments for +"
  BoolLit b -> returnA -< (BoolVal b)
  And e1 e2 -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    case (v1,v2) of
      (BoolVal b1, BoolVal b2) -> returnA -< (BoolVal (b1 && b2))
      (_,_) -> throw -< "Expected two booleans as arguments for &&"
  Lt e1 e2 -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    case (v1,v2) of
      (NumVal n1, NumVal n2) -> returnA -< (BoolVal (n1 < n2))
      (_,_) -> throw -< "Expected two numbers as arguments for +"

get :: Arr () Store
get = Arr (\() st -> Right (st,st))

put :: Arr Store ()
put = Arr (\st _ -> Right (st,()))

throw :: Arr String a
throw = Arr (\er _ -> Left er)

-- run :: Store -> [Statement] -> Either String Store
run :: Arr [Statement] ()
run = proc stmts -> case stmts of
  (Assign x e : rest) -> do
    v <- eval -< e
    st <- get -< ()
    put -< (Store.insert x v st)
    run -< rest
  (If cond ifBranch elseBranch : rest) -> do
    v <- eval -< cond
    case v of
      BoolVal True -> run -< ifBranch
      BoolVal False -> run -< elseBranch
      NumVal _ -> throw -< "Expected a boolean expression as condition for an if"
    run -< rest
  (While cond body : rest) ->
    run -< If cond (body ++ [While cond body]) [] : rest
  [] ->
    returnA -< ()

run' :: [Statement] -> Store -> Either String Store
run' stmts st = right fst (runArr run stmts st)

instance Category Arr where
  id = Arr (\x st -> Right (st,x))
  Arr f . Arr g = Arr $ \x st -> case g x st of
    Left er -> Left er
    Right (st',y) -> f y st'

instance Arrow Arr where
  arr f = Arr (\x st -> Right (st,f x))
  first (Arr f) = Arr $ \(x,y) st -> case f x st of
    Left er -> Left er
    Right (st',z) -> Right (st',(z,y))

instance ArrowChoice Arr where
  left (Arr f) = Arr $ \e st -> case e of
    Left x -> case f x st of
      Left er -> Left er
      Right (st',y) -> Right (st',Left y)
    Right z -> Right (st,Right z)
