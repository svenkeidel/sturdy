{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module Data.Scan where

import Data.Order

data Scan a b = Scan a b
  deriving (Functor,Foldable)

instance (LowerBounded a, Complete a) => Applicative (Scan a) where
  pure = Scan bottom
  Scan a1 f <*> Scan a2 x = Scan (a1 ⊔ a2) (f x)
