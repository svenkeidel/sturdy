{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Abstract.FreeCocompletion where

import Control.Arrow hiding (ArrowMonad)
import Control.Arrow.Monad
import Control.Applicative
import Control.Monad
import Control.DeepSeq

import Data.Abstract.Widening
import Data.Abstract.Stable

import Data.Text.Prettyprint.Doc
import Data.Profunctor
import Data.Hashable
import Data.Order
import Data.Text(Text)

import GHC.Generics(Generic)
import GHC.Exts

data FreeCocompletion a = Upper a | Bottom deriving (Eq,Functor,Traversable,Foldable,Generic)

instance Show a => Show (FreeCocompletion a) where
  show Bottom = "⊥"
  show (Upper a) = show a

instance Pretty a => Pretty (FreeCocompletion a) where
  pretty Bottom = "⊥"
  pretty (Upper a) = pretty a

instance NFData a => NFData (FreeCocompletion a)

instance Hashable a => Hashable (FreeCocompletion a) where
  hashWithSalt s (Upper a) = s `hashWithSalt` (1::Int) `hashWithSalt` a
  hashWithSalt s Bottom = s `hashWithSalt` (2::Int)
  {-# INLINE hashWithSalt #-}

instance Applicative FreeCocompletion where
  pure = return
  (<*>) = ap
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad FreeCocompletion where
  return = Upper
  Upper x >>= k = k x
  Bottom >>= _ = Bottom
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}

instance (ArrowChoice c, Profunctor c) => ArrowFunctor FreeCocompletion c where
  mapA f = lmap toEither (arr (const Bottom) ||| rmap Upper f)
  {-# INLINE mapA #-}

instance (ArrowChoice c, Profunctor c) => ArrowMonad FreeCocompletion c where
  mapJoinA f = lmap toEither (arr (const Bottom) ||| f)
  {-# INLINE mapJoinA #-}

instance PreOrd a => PreOrd (FreeCocompletion a) where
  Bottom ⊑ _ = True
  _ ⊑ Bottom = False
  Upper a ⊑ Upper b = a ⊑ b

  Bottom ≈ Bottom = True
  Upper a ≈ Upper b = a ≈ b
  _ ≈ _ = False
  {-# INLINE (⊑) #-}
  {-# INLINE (≈) #-}

instance PreOrd a => LowerBounded (FreeCocompletion a) where
  bottom = bottom

instance Num a => Num (FreeCocompletion a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (FreeCocompletion a) where
  (/) = liftA2 (/)
  fromRational = pure . fromRational

instance IsString s => IsString (FreeCocompletion s) where
  fromString = Upper . fromString

instance Complete a => Complete (FreeCocompletion a) where
  a ⊔ Bottom = a
  Bottom ⊔ b = b
  Upper a ⊔ Upper b = Upper (a ⊔ b)

toEither :: FreeCocompletion a -> Either () a
toEither Bottom = Left ()
toEither (Upper a) = Right a
{-# INLINE toEither #-}

widening :: Widening a -> Widening (FreeCocompletion a)
widening wa (Upper a) (Upper a') = second Upper (a `wa` a')
widening _   Bottom    Bottom    = (Stable,Bottom)
widening _  (Upper a)  Bottom    = (Unstable,Upper a)
widening _   Bottom   (Upper b)  = (Unstable,Upper b)
{-# INLINABLE widening #-}

fromCompletion :: a -> FreeCocompletion a -> a
fromCompletion a Bottom = a
fromCompletion _ (Upper a) = a
{-# INLINE fromCompletion #-}
