{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Abstract.Topped where

import Data.Order
import Control.Monad
import Control.Applicative

data Topped a = Lower a | Top deriving (Functor,Traversable,Foldable)

instance Show a => Show (Topped a) where
  show Top = "⊤"
  show (Lower a) = show a

instance Applicative Topped where
  pure = return
  (<*>) = ap

instance Monad Topped where
  return = Lower
  Lower x >>= k = k x
  Top >>= _ = Top

instance PreOrd a => PreOrd (Topped a) where
  _ ⊑ Top = True
  Top ⊑ _ = False
  Lower a ⊑ Lower b = a ⊑ b

  Top ≈ Top = True
  Lower a ≈ Lower b = a ≈ b
  _ ≈ _ = False

instance PreOrd a => Complete (Topped a) where
  Lower a ⊔ Lower b
    | a ⊑ b = Lower b
    | b ⊑ a = Lower a
    | otherwise = Top
  Top ⊔ _ = Top
  _ ⊔ Top = Top

instance CoComplete a => CoComplete (Topped a) where
  Lower a ⊓ Lower b = Lower (a ⊓ b) 
  x ⊓ Top = x
  Top ⊓ y = y

instance PreOrd a => UpperBounded (Topped a) where
  top = Top

instance LowerBounded a => LowerBounded (Topped a) where
  bottom = Lower bottom

instance Num a => Num (Topped a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Topped a) where
  (/) = liftA2 (/)
  fromRational = pure . fromRational
