{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.Abstract.FreeCompletion where

import Control.Monad
import Control.Applicative

import Data.Order
import Data.Hashable

import GHC.Generics

data FreeCompletion a = Lower a | Top deriving (Eq,Functor,Traversable,Foldable,Generic)

instance Show a => Show (FreeCompletion a) where
  show Top = "⊤"
  show (Lower a) = show a

instance Applicative FreeCompletion where
  pure = return
  (<*>) = ap

instance Monad FreeCompletion where
  return = Lower
  Lower x >>= k = k x
  Top >>= _ = Top

instance PreOrd a => PreOrd (FreeCompletion a) where
  _ ⊑ Top = True
  Top ⊑ _ = False
  Lower a ⊑ Lower b = a ⊑ b

  Top ≈ Top = True
  Lower a ≈ Lower b = a ≈ b
  _ ≈ _ = False

instance PreOrd a => Complete (FreeCompletion a) where
  Lower a ⊔ Lower b
    | a ⊑ b = Lower b
    | b ⊑ a = Lower a
    | otherwise = Top
  Top ⊔ _ = Top
  _ ⊔ Top = Top

instance CoComplete a => CoComplete (FreeCompletion a) where
  Lower a ⊓ Lower b = Lower (a ⊓ b) 
  x ⊓ Top = x
  Top ⊓ y = y

instance PreOrd a => UpperBounded (FreeCompletion a) where
  top = Top

instance LowerBounded a => LowerBounded (FreeCompletion a) where
  bottom = Lower bottom

instance Num a => Num (FreeCompletion a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (FreeCompletion a) where
  (/) = liftA2 (/)
  fromRational = pure . fromRational

instance Hashable a => Hashable (FreeCompletion a)
