{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Abstract where

import           Prelude hiding (Maybe(..),lookup,id)
import qualified Prelude as P

import           Control.Monad(ap)
import           Control.Category
import           Control.Arrow

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Order

import           Shared

data Val = IV (Int,Int)
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
  lookup = Interp $ \env x -> case M.lookup x env of
    P.Just y    -> Just y
    P.Nothing -> Nothing
  lit = arr (\n -> IV (n,n))
  add = arr (\(IV (i1,j1), IV (i2,j2)) -> IV (i1+i2,j1+j2))
  ifZero f g = proc (IV (i,j),(x,y)) ->
    if i == 0 && j == 0
    then f -< x
    else if j < 0 || 0 < i 
         then g -< y
         else (f -< x) ⊔ (g -< y)
  try (Interp f) (Interp g) (Interp h) = Interp $ \env x -> case f env x of
    Just y  -> g env y
    Nothing -> h env x
    JustNothing y  -> g env y ⊔ h env x

instance PreOrd (Interp x y) where
  Interp f ⊑ Interp g = error "forall env x. f env x ⊑ g env x " f g

instance Complete y => Complete (Interp x y) where
  Interp f ⊔ Interp g = Interp $ \env x -> f env x ⊔ g env x

data Maybe a = Just a | Nothing | JustNothing a deriving (Functor)

instance Applicative Maybe where
  pure = return
  (<*>) = ap

instance Monad Maybe where
  return = Just
  Just x >>= k = k x
  Nothing >>= _ = Nothing
  JustNothing x >>= k = case k x of
    Just y -> JustNothing y
    z -> z

instance PreOrd a => PreOrd (Maybe a) where
  Nothing ⊑ Nothing = True
  Just a ⊑ Just b  = a ⊑ b
  Nothing ⊑ JustNothing _ = True
  Just a ⊑ JustNothing b = a ⊑ b
  JustNothing a ⊑ JustNothing b = a ⊑ b
  _ ⊑ _ = False

instance Complete a => Complete (Maybe a) where
  Nothing ⊔ Nothing = Nothing
  Nothing ⊔ Just a = JustNothing a
  Just a ⊔ Nothing = JustNothing a
  Just a ⊔ Just b = Just (a ⊔ b)
  JustNothing a ⊔ Just b = JustNothing (a ⊔ b)
  Just a ⊔ JustNothing b = JustNothing (a ⊔ b)
  JustNothing a ⊔ Nothing = JustNothing a
  Nothing ⊔ JustNothing b = JustNothing b
  JustNothing a ⊔ JustNothing b = JustNothing (a ⊔ b)

instance PreOrd Val where
  IV (i1,j1) ⊑ IV (i2,j2) = i2 <= i1 && j1 <= j2

instance Complete Val where
  IV (i1,j1) ⊔ IV (i2,j2) = IV (min i1 i2, max j1 j2)
