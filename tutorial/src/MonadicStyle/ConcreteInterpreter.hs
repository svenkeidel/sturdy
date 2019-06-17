{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | This is an interpreter in monadic-style. The effects of the
-- programming language such as store passing and error propagation
-- are handled implicitly by the monad `M`.
module MonadicStyle.ConcreteInterpreter where

import           Prelude hiding (lookup)

import           Control.Monad

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Label
import           Data.Maybe

import           Syntax

data Val = BoolVal Bool | NumVal Int deriving (Show,Eq)
type Addr = Label
type Env = Map String Addr
type Store = Map Addr Val

newtype M a = M {runM :: Env -> Store -> Either String (Store,a)}

-- eval :: Store -> Expr -> Either String Val
eval :: Expr -> M Val
eval e = case e of
  Var x _ -> do
    env <- ask
    st <- get
    addr <- lookup x env
    lookup addr st
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

-- run :: Env -> Store -> [Statement] -> Either String Store
run :: [Statement] -> M ()
run stmts = case stmts of
  (Assign x e l : rest) -> do
    v <- eval e
    env <- ask
    store <- get
    let addr = fromMaybe l (M.lookup x env)
    put (M.insert addr v store)
    local (run rest) (M.insert x addr env)
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

-- Monadic helper functions ---------------------------------------------
lookup :: Ord a => a -> Map a b -> M b
lookup a m = case M.lookup a m of
  Just b -> return b
  Nothing -> throw "Variable not in scope"

ask :: M Env
ask = M (\env st -> Right (st,env))

local :: M a -> Env -> M a
local (M f) env = M (\_ st -> f env st)

get :: M Store
get = M (\_ st -> Right (st,st))

put :: Store -> M ()
put st = M (\_ _ -> Right (st,()))

throw :: String -> M a
throw er = M (\_ _ -> Left er)

-- Monad Instances ------------------------------------------------------
deriving instance Functor M

-- do
--  v1 <- eval e1
--  v2 <- eval e2
--  ... 
--
--  eval e1 >>= (\v1 -> eval e2 ....)

-- (>>=) :: M a -> (a -> M b) -> M b
instance Monad M where
  return a = M (\_ st -> Right (st,a))
  M m >>= k = M $ \env st ->
    case m env st of
      Left er -> Left er
      Right (st',x) -> runM (k x) env st'

instance Applicative M where
  pure = return
  (<*>) = ap
