{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Abstract.FreeCompletion where

import Control.Applicative
import Control.Monad
import Data.Hashable
import Data.Order

data FreeCompletion a = Lower a | Top deriving (Eq,Functor,Traversable,Foldable)

instance Show a => Show (FreeCompletion a) where
  show Top = "⊤"
  show (Lower a) = show a

instance Hashable a => Hashable (FreeCompletion a) where
  hashWithSalt s (Lower a) = s `hashWithSalt` a
  hashWithSalt s Top = s `hashWithSalt` (2::Int)

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

instance (PreOrd a, Complete (FreeCompletion a),
          PreOrd b, Complete (FreeCompletion b)) => Complete (FreeCompletion (a,b)) where
  Lower (a1,b1) ⊔ Lower (a2,b2) = case (Lower a1 ⊔ Lower a2, Lower b1 ⊔ Lower b2) of
    (Lower a, Lower b) -> Lower (a,b)
    (_, _) -> Top
  _ ⊔ _ = Top

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

instance Complete (FreeCompletion ()) where
  Lower _ ⊔ Lower _ = Lower ()
  _ ⊔ _ = Top
