{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Abstract.Failure where

import Control.Arrow hiding (ArrowMonad)
import Control.Arrow.Monad
import Control.Monad
import Control.Monad.Except
import Control.DeepSeq

import Data.Abstract.FreeCompletion(FreeCompletion(..))
import Data.Abstract.Widening
import Data.Abstract.Stable

import Data.Bifunctor (Bifunctor(bimap))
import Data.Profunctor
import Data.Hashable
import Data.Order

import GHC.Generics (Generic, Generic1)

-- | Failure is an Either-like type with the special ordering Failure ⊑ Success.
-- Left and Right of the regular Either type, on the other hand are incomparable.
data Failure e a = Fail e | Success a
  deriving (Eq, Functor, Generic, Generic1, NFData, NFData1)

instance (Show e,Show a) => Show (Failure e a) where
  show (Fail e) = "Failure " ++ show e
  show (Success a) = show a

instance (Hashable e, Hashable a) => Hashable (Failure e a) where
  hashWithSalt s (Fail e)  = s `hashWithSalt` (0::Int) `hashWithSalt` e
  hashWithSalt s (Success a) = s `hashWithSalt` (1::Int) `hashWithSalt`  a

instance PreOrd a => PreOrd (Failure e a) where
  Fail _ ⊑ Success _ = True
  Fail _ ⊑ Fail _ = True
  Success x ⊑ Success y = x ⊑ y
  _ ⊑ _ = False

  Fail _ ≈ Fail _ = True
  Success x ≈ Success y = x ≈ y
  _ ≈ _ = False

instance Complete a => Complete (Failure e a) where
  Fail _ ⊔ b = b
  a ⊔ Fail _ = a
  Success x ⊔ Success y = Success (x ⊔ y)

instance UpperBounded a => UpperBounded (Failure e a) where
  top = Success top

widening :: Widening a -> Widening (Failure e a)
widening _ (Fail _) (Fail b) = (Stable,Fail b)
widening _ (Fail _) (Success y) = (Unstable ,Success y)
widening _ (Success x) (Fail _) = (Unstable ,Success x)
widening w (Success x) (Success y) = second Success (x `w` y)

instance (PreOrd a, Complete (FreeCompletion a)) => Complete (FreeCompletion (Failure e a)) where
  Lower m1 ⊔ Lower m2 = case (bimap Lower Lower m1 ⊔ bimap Lower Lower m2) of
    Fail (Lower e) -> Lower (Fail e)
    Success (Lower a) -> Lower (Success a)
    _ -> Top
  _ ⊔ _ = Top

instance Bifunctor Failure where
  bimap f g x = case x of
    Fail e -> Fail (f e)
    Success a -> Success (g a)

instance MonadError e (Failure e) where
  throwError = Fail
  catchError (Fail e) f = f e
  catchError (Success a) _ = Success a

instance Applicative (Failure e) where
  pure = return
  (<*>) = ap

instance Monad (Failure e) where
  return = Success
  Fail e >>= _ = Fail e
  Success a >>= k = k a

instance (ArrowChoice c, Profunctor c) => ArrowFunctor (Failure e) c where
  mapA f = lmap toEither (arr Fail ||| rmap Success f)
  {-# INLINE mapA #-}

instance (ArrowChoice c, Profunctor c) => ArrowMonad (Failure e) c where
  mapJoinA f = lmap toEither (arr Fail ||| f) 
  {-# INLINE mapJoinA #-}

fromFailure :: a -> Failure e a -> a
fromFailure _ (Success a) = a
fromFailure a (Fail _) = a

fromEither :: Either e a -> Failure e a
fromEither (Left e) = Fail e
fromEither (Right a) = Success a

toEither :: Failure e a -> Either e a
toEither (Fail e) = Left e
toEither (Success a) = Right a

fromMaybe :: Maybe a -> Failure () a
fromMaybe Nothing = Fail ()
fromMaybe (Just a) = Success a

toMaybe :: Failure e a -> Maybe a
toMaybe (Fail _) = Nothing
toMaybe (Success a) = Just a
