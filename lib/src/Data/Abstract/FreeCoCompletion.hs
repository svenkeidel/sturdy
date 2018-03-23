{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Abstract.FreeCoCompletion where

import Data.Order
import Control.Monad
import Control.Applicative

data FreeCoCompletion a = Bottom | Greater a deriving (Functor,Traversable,Foldable)

instance Show a => Show (FreeCoCompletion a) where
  show Bottom = "⊥"
  show (Greater a) = show a

instance Applicative FreeCoCompletion where
  pure = return
  (<*>) = ap

instance Monad FreeCoCompletion where
  return = Greater
  Greater x >>= k = k x
  Bottom >>= _ = Bottom

instance PreOrd a => PreOrd (FreeCoCompletion a) where
  Bottom ⊑ _ = False
  _ ⊑ Bottom = False
  Greater a ⊑ Greater b = a ⊑ b

  Bottom ≈ Bottom = True
  Greater a ≈ Greater b = a ≈ b
  _ ≈ _ = False

instance Complete a => Complete (FreeCoCompletion a) where
  Greater a ⊔ Greater b = Greater (a ⊔ b) 
  x ⊔ Bottom = x
  Bottom ⊔ y = y

instance PreOrd a => CoComplete (FreeCoCompletion a) where
  Greater a ⊓ Greater b
    | a ⊑ b = Greater a
    | b ⊑ a = Greater b
    | otherwise = Bottom
  Bottom ⊓ _ = Bottom
  _ ⊓ Bottom = Bottom

instance UpperBounded a => UpperBounded (FreeCoCompletion a) where
  top = Greater top

instance PreOrd a => LowerBounded (FreeCoCompletion a) where
  bottom = Bottom

instance Num a => Num (FreeCoCompletion a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (FreeCoCompletion a) where
  (/) = liftA2 (/)
  fromRational = pure . fromRational
