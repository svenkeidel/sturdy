{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Abstract.Floored where

import Control.Monad
import Control.Applicative

import Data.Order
import Data.Abstract.Widening

-- Free cocompletion of a type
data Floored a = Bottom | Greater a deriving (Eq,Functor,Traversable,Foldable)

instance Show a => Show (Floored a) where
  show Bottom = "⊥"
  show (Greater a) = show a

instance Applicative Floored where
  pure = return
  (<*>) = ap

instance Monad Floored where
  return = Greater
  Greater x >>= k = k x
  Bottom >>= _ = Bottom

instance PreOrd a => PreOrd (Floored a) where
  Bottom ⊑ _ = True
  _ ⊑ Bottom = False
  Greater a ⊑ Greater b = a ⊑ b

  Bottom ≈ Bottom = True
  Greater a ≈ Greater b = a ≈ b
  _ ≈ _ = False

instance Complete a => Complete (Floored a) where
  Greater a ⊔ Greater b = Greater (a ⊔ b) 
  x ⊔ Bottom = x
  Bottom ⊔ y = y

instance Widening a => Widening (Floored a) where
  Greater a ▽ Greater b = Greater (a ▽ b) 
  x ▽ Bottom = x
  Bottom ▽ y = y

instance PreOrd a => CoComplete (Floored a) where
  Greater a ⊓ Greater b
    | a ⊑ b = Greater a
    | b ⊑ a = Greater b
    | otherwise = Bottom
  Bottom ⊓ _ = Bottom
  _ ⊓ Bottom = Bottom

instance UpperBounded a => UpperBounded (Floored a) where
  top = Greater top

instance PreOrd a => LowerBounded (Floored a) where
  bottom = Bottom

instance Num a => Num (Floored a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Floored a) where
  (/) = liftA2 (/)
  fromRational = pure . fromRational
