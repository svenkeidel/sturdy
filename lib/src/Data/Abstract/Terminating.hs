{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Abstract.Terminating where

import Control.Monad
import Control.Applicative

import Data.Order
import Data.Abstract.Widening

-- Free cocompletion of a type
data Terminating a = NonTerminating | Terminating a deriving (Eq,Functor,Traversable,Foldable)

instance Show a => Show (Terminating a) where
  show NonTerminating = "⊥"
  show (Terminating a) = show a

instance Applicative Terminating where
  pure = return
  (<*>) = ap

instance Monad Terminating where
  return = Terminating
  Terminating x >>= k = k x
  NonTerminating >>= _ = NonTerminating

instance PreOrd a => PreOrd (Terminating a) where
  NonTerminating ⊑ _ = True
  _ ⊑ NonTerminating = False
  Terminating a ⊑ Terminating b = a ⊑ b

  NonTerminating ≈ NonTerminating = True
  Terminating a ≈ Terminating b = a ≈ b
  _ ≈ _ = False

instance Complete a => Complete (Terminating a) where
  Terminating a ⊔ Terminating b = Terminating (a ⊔ b) 
  x ⊔ NonTerminating = x
  NonTerminating ⊔ y = y

instance Widening a => Widening (Terminating a) where
  Terminating a ▽ Terminating b = Terminating (a ▽ b) 
  x ▽ NonTerminating = x
  NonTerminating ▽ y = y

instance PreOrd a => CoComplete (Terminating a) where
  Terminating a ⊓ Terminating b
    | a ⊑ b = Terminating a
    | b ⊑ a = Terminating b
    | otherwise = NonTerminating
  NonTerminating ⊓ _ = NonTerminating
  _ ⊓ NonTerminating = NonTerminating

instance UpperBounded a => UpperBounded (Terminating a) where
  top = Terminating top

instance PreOrd a => LowerBounded (Terminating a) where
  bottom = NonTerminating

instance Num a => Num (Terminating a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Terminating a) where
  (/) = liftA2 (/)
  fromRational = pure . fromRational
