{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Abstract.Terminating where

import Control.Arrow hiding (ArrowMonad)
import Control.Arrow.Monad
import Control.Monad
import Control.Applicative
import Control.DeepSeq
import Control.Arrow(second)

import Data.Profunctor
import Data.Order
import Data.Hashable
import Data.Abstract.Stable
import Data.Abstract.Widening

import GHC.Generics

-- Free cocompletion of a type
data Terminating a = NonTerminating | Terminating a deriving (Eq,Functor,Traversable,Foldable,Generic)
instance NFData a => NFData (Terminating a)

instance Show a => Show (Terminating a) where
  show NonTerminating = "NonTerminating"
  show (Terminating a) = show a

instance Hashable a => Hashable (Terminating a)

instance Applicative Terminating where
  pure = return
  (<*>) = ap
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad Terminating where
  return = Terminating
  Terminating x >>= k = k x
  NonTerminating >>= _ = NonTerminating
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}

instance (ArrowChoice c, Profunctor c) => ArrowFunctor Terminating c where
  mapA f = lmap toEither (arr (\_ -> NonTerminating) ||| rmap Terminating f)
  {-# INLINE mapA #-}

instance (ArrowChoice c, Profunctor c) => ArrowMonad Terminating c where
  mapJoinA f = lmap toEither (arr (\_ -> NonTerminating) ||| f)
  {-# INLINE mapJoinA #-}

toEither :: Terminating a -> Either () a
toEither (Terminating a) = Right a
toEither NonTerminating = Left ()
{-# INLINE toEither #-}

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

instance CoComplete a => CoComplete (Terminating a) where
  Terminating a ⊓ Terminating b = Terminating (a ⊓ b) 
  NonTerminating ⊓ _ = NonTerminating
  _ ⊓ NonTerminating = NonTerminating

instance UpperBounded a => UpperBounded (Terminating a) where
  top = Terminating top

instance PreOrd a => LowerBounded (Terminating a) where
  bottom = NonTerminating

widening :: Widening a -> Widening (Terminating a)
widening _ NonTerminating NonTerminating = (Stable,NonTerminating)
widening _ NonTerminating (Terminating b) = (Unstable,Terminating b)
widening _ (Terminating a) NonTerminating = (Unstable,Terminating a)
widening w (Terminating a) (Terminating b) = second Terminating (w a b)

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

fromTerminating :: a -> Terminating a -> a
fromTerminating _ (Terminating a) = a
fromTerminating a NonTerminating = a
{-# INLINE fromTerminating #-}

toMaybe :: Terminating a -> Maybe a
toMaybe (Terminating a) = Just a
toMaybe NonTerminating = Nothing
{-# INLINE toMaybe #-}
