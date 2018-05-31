{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Abstract where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Order

data Val = IV (Int,Int)
type Env = Map String Val
data Expr = Var String | Lit Int | Add Expr Expr | IfZero Expr Expr Expr

eval :: Expr -> Env -> Maybe Val
eval e env = case e of
  Var x -> M.lookup x env
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

instance PreOrd a => PreOrd (Maybe a) where
  Nothing ⊑ Nothing = True
  Nothing ⊑ Just _  = True
  Just a ⊑ Just b  = a ⊑ b
  _ ⊑ _ = False

instance Complete a => Complete (Maybe a) where
  Nothing ⊔ Nothing = Nothing
  Nothing ⊔ Just a = Just a
  Just a ⊔ Nothing = Just a
  Just a ⊔ Just b = Just (a ⊔ b)

instance PreOrd Val where
  IV (i1,j1) ⊑ IV (i2,j2) = i2 <= i1 && j1 <= j2

instance Complete Val where
  IV (i1,j1) ⊔ IV (i2,j2) = IV (min i1 i2, max j1 j2)
