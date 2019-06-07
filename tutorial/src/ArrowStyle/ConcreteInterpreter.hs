{-# LANGUAGE Arrows #-}
module ArrowStyle.ConcreteInterpreter where

import           Control.Category
import           Control.Arrow

import           Data.Map (Map)
import qualified Data.Map as Store

import           Syntax

data Val = BoolVal Bool | NumVal Int deriving (Show,Eq)
type Store = Map String Val

newtype Arr x y = Arr {runArr :: (Store,x) -> Either String (Store,y)}

-- eval :: Store -> Expr -> Either String Val
eval :: Arr Expr Val
eval = proc e -> case e of
  Var x _ -> do
    st <- get -< ()
    case Store.lookup x st of
      Just v -> returnA -< v
      Nothing -> throw -< "Variable " ++ show x ++ "not in scope"
  NumLit n _ -> returnA -< (NumVal n)
  Add e1 e2 _ -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    case (v1,v2) of
      (NumVal n1, NumVal n2) -> returnA -< (NumVal (n1 + n2))
      (_,_) -> throw -< "Expected two numbers as arguments for +"
  BoolLit b _ -> returnA -< (BoolVal b)
  And e1 e2 _ -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    case (v1,v2) of
      (BoolVal b1, BoolVal b2) -> returnA -< (BoolVal (b1 && b2))
      (_,_) -> throw -< "Expected two booleans as arguments for &&"
  Lt e1 e2 _ -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    case (v1,v2) of
      (NumVal n1, NumVal n2) -> returnA -< (BoolVal (n1 < n2))
      (_,_) -> throw -< "Expected two numbers as arguments for +"

get :: Arr () Store
get = Arr (\(st,()) -> Right (st,st))

put :: Arr Store ()
put = Arr (\(_,st) -> Right (st,()))

throw :: Arr String a
throw = Arr (\(_,er) -> Left er)

-- run :: Store -> [Statement] -> Either String Store
run :: Arr [Statement] ()
run = proc stmts -> case stmts of
  (Assign x e _ : rest) -> do
    v <- eval -< e
    st <- get -< ()
    put -< (Store.insert x v st)
    run -< rest
  (If cond ifBranch elseBranch _ : rest) -> do
    v <- eval -< cond
    case v of
      BoolVal True -> run -< ifBranch
      BoolVal False -> run -< elseBranch
      NumVal _ -> throw -< "Expected a boolean expression as condition for an if"
    run -< rest
  (While cond body l : rest) ->
    run -< If cond (body ++ [While cond body l]) [] l : rest
  [] ->
    returnA -< ()

run' :: [Statement] -> Store -> Either String Store
run' stmts st = right fst (runArr run (st,stmts))

instance Category Arr where
  id = Arr (\(st,x) -> Right (st,x))
  Arr f . Arr g = Arr $ \(st,x) -> case g (st,x) of
    Left er -> Left er
    Right (st',y) -> f (st',y)

instance Arrow Arr where
  arr f = Arr (\(st,x) -> Right (st,f x))
  first (Arr f) = Arr $ \(st,(x,y)) -> case f (st,x) of
    Left er -> Left er
    Right (st',z) -> Right (st',(z,y))

instance ArrowChoice Arr where
  left (Arr f) = Arr $ \(st,e) -> case e of
    Left x -> case f (st,x) of
      Left er -> Left er
      Right (st',y) -> Right (st',Left y)
    Right z -> Right (st,Right z)
