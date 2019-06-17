{-# LANGUAGE Arrows #-}
module ArrowStyle.ConcreteInterpreter where

import           Prelude hiding (lookup)

import           Control.Category
import           Control.Arrow

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Label
import           Data.Maybe

import           Syntax

data Val = BoolVal Bool | NumVal Int deriving (Show,Eq)
type Addr = Label
type Env = Map String Addr
type Store = Map Addr Val

newtype Arr x y = Arr {runArr :: Env -> Store -> x -> Either String (Store,y)}

-- eval :: Store -> Expr -> Either String Val
eval :: Arr Expr Val
eval = proc e -> case e of
  Var x _ -> do
    env <- ask -< ()
    store <- get -< ()
    addr <- lookup -< (x,env)
    lookup -< (addr,store)
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

-- run :: Store -> [Statement] -> Either String Store
run :: Arr [Statement] ()
run = proc stmts -> case stmts of
  (Assign x e l : rest) -> do
    v <- eval -< e
    env <- ask -< ()
    store <- get -< ()
    let addr = fromMaybe l (M.lookup x env)
    put -< (M.insert addr v store)
    local run -< (rest,M.insert x addr env)
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

run' :: [Statement] -> Env -> Store -> Either String Store
run' stmts env st = right fst (runArr run env st stmts)

-- Arrow helper functions ---------------------------------------------
lookup :: Ord a => Arr (a,Map a b) b
lookup = proc (a,m) -> case M.lookup a m of
  Just b -> returnA -< b
  Nothing -> throw -< "Variable not in scope"

ask :: Arr () Env
ask = Arr (\env st () -> Right (st,env))

local :: Arr a b -> Arr (a,Env) b
local (Arr f) = Arr (\_ st (a,env) -> f env st a)

get :: Arr () Store
get = Arr (\_ st () -> Right (st,st))

put :: Arr Store ()
put = Arr (\_ _ st -> Right (st,()))

throw :: Arr String a
throw = Arr (\_ _ er -> Left er)

-- Arrow Instances ------------------------------------------------------
instance Category Arr where
  id = Arr (\_ st x -> Right (st,x))
  Arr f . Arr g = Arr $ \env st x -> case g env st x of
    Left er -> Left er
    Right (st',y) -> f env st' y

instance Arrow Arr where
  arr f = Arr (\_ st x -> Right (st,f x))
  first (Arr f) = Arr $ \env st (x,y) -> case f env st x of
    Left er -> Left er
    Right (st',z) -> Right (st',(z,y))

instance ArrowChoice Arr where
  left (Arr f) = Arr $ \env st e -> case e of
    Left x -> case f env st x of
      Left er -> Left er
      Right (st',y) -> Right (st',Left y)
    Right z -> Right (st,Right z)
