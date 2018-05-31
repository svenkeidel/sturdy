{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
module Concrete where

import           Prelude hiding (id)

import           Control.Arrow
import           Control.Category

import           Data.Map (Map)
import qualified Data.Map as M

import           Shared

type Val = Int
type Env = Map String Val

newtype Interp a b = Interp (Env -> a -> Maybe b)

eval :: Interp Expr Val
eval = eval'

instance Category Interp where
  id = Interp (\_ x -> Just x)
  Interp f . Interp g = Interp $ \env x -> do
    y <- g env x
    f env y

instance Arrow Interp where
  arr f = Interp (\_ x -> Just (f x))
  first (Interp f) = Interp $ \env (x,z) -> do
    y <- f env x
    return (y,z)

instance ArrowChoice Interp where
  left (Interp f) = Interp $ \env e -> case e of
    Left x -> do
      y <- f env x
      return (Left y)
    Right x -> return (Right x)

instance ArrowFix x y Interp where
  fixA f = f (fixA f)

instance IsVal Val Interp where
  lookup = Interp (\env x -> M.lookup x env)
  lit = id
  add = arr (\(x,y) -> x + y)
  ifZero f g = proc (v,(x,y)) ->
    if v == 0
    then f -< x
    else g -< y
  try (Interp f) (Interp g) (Interp h) = Interp $ \env x -> case f env x of
    Just y  -> g env y
    Nothing -> h env x
