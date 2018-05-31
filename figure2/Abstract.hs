{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Abstract where

import           Prelude hiding (Maybe(..),lookup)
import qualified Prelude as P

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Order
import           Control.Monad(ap)

data Val = IV (Int,Int)
type Env = Map String Val
data Expr = Var String | Lit Int | Add Expr Expr | IfZero Expr Expr Expr | TryZero Expr Expr Expr

eval :: Expr -> Env -> Maybe Val
eval e env = case e of
  Var x -> lookup x env
  Lit n -> return (IV (n,n))
  Add e1 e2 -> do
    IV (i1,j1) <- eval e1 env
    IV (i2,j2) <- eval e2 env
    return (IV (i1 + i2, j1 + j2))
  IfZero e1 e2 e3 -> do
    IV (i1,j1) <- eval e1 env
    if i1 == 0 && j1 == 0
    then eval e2 env
    else if j1 < 0 || 0 < i1
         then eval e3 env
         else eval e2 env ⊔ eval e3 env
  TryZero e1 e2 e3 -> case eval e1 env of
    Just (IV (i1,j1))
      | i1 == 0 && j1 == 0 -> eval e2 env
      | j1 < 0 || 0 < i1   -> eval e3 env
      | otherwise          -> eval e2 env ⊔ eval e3 env
    Nothing                -> eval e3 env
    JustNothing (IV (i1,j1))
      | i1 == 0 && j1 == 0 -> eval e2 env ⊔ eval e3 env
      | j1 < 0 || 0 < i1   -> eval e3 env
      | otherwise          -> eval e2 env ⊔ eval e3 env
  where
    lookup x env' = case M.lookup x env' of
      P.Just y -> Just y
      P.Nothing -> Nothing

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
