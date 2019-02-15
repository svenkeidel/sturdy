{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Abstract.Error where

import Control.Monad
import Control.Monad.Except
import Data.Abstract.FreeCompletion (FreeCompletion(..))
import Data.Abstract.Widening
import Data.Bifunctor
import Data.Hashable
import Data.Order

import Data.Monoidal

import GHC.Generics (Generic, Generic1)
import Control.DeepSeq

-- | Failure is an Either-like type with the special ordering Failure ⊑ Success.
-- Left and Right of the regular Either type, on the other hand are incomparable.
data Error e a = Fail e | Success a
  deriving (Eq, Functor, Generic, Generic1, NFData, NFData1)

instance (Show e,Show a) => Show (Error e a) where
  show (Fail e) = "Error " ++ show e
  show (Success a) = show a

instance (Hashable e, Hashable a) => Hashable (Error e a) where
  hashWithSalt s (Fail e)  = s `hashWithSalt` (0::Int) `hashWithSalt` e
  hashWithSalt s (Success a) = s `hashWithSalt` (1::Int) `hashWithSalt`  a

instance (PreOrd e, PreOrd a) => PreOrd (Error e a) where
  Success x ⊑ Success y = x ⊑ y
  Success _ ⊑ Fail _  = True
  Fail x ⊑ Fail y = x ⊑ y
  _ ⊑ _ = False

  Fail x ≈ Fail y = x ≈ y
  Success x ≈ Success y = x ≈ y
  _ ≈ _ = False

instance (Complete e,Complete a) => Complete (Error e a) where
  (⊔) = toJoin2 widening (⊔) (⊔)

instance (PreOrd a, UpperBounded e) => UpperBounded (Error e a) where
  top = Fail top

widening :: Widening e -> Widening a -> Widening (Error e a)
widening we _ (Fail a) (Fail b) = second Fail (a `we` b)
widening _ wa (Success a) (Success b) = second Success (a `wa` b)
widening _ _ (Fail a) (Success _) = (Instable ,Fail a)
widening _ _ (Success _) (Fail b) = (Instable ,Fail b)

instance (PreOrd e, PreOrd a, Complete (FreeCompletion e), Complete (FreeCompletion a)) => Complete (FreeCompletion (Error e a)) where
  Lower m1 ⊔ Lower m2 = case (bimap Lower Lower m1 ⊔ bimap Lower Lower m2) of
    Fail (Lower e) -> Lower (Fail e)
    Success (Lower a) -> Lower (Success a)
    _ -> Top
  _ ⊔ _ = Top

instance Bifunctor Error where
  bimap f g x = case x of
    Fail e -> Fail (f e)
    Success a -> Success (g a)

instance MonadError e (Error e) where
  throwError = Fail
  catchError (Fail e) f = f e
  catchError (Success a) _ = Success a

instance Applicative (Error e) where
  pure = return
  (<*>) = ap

instance Monad (Error e) where
  return = Success
  Fail e >>= _ = Fail e
  Success a >>= k = k a

fromError :: a -> Error e a -> a
fromError _ (Success a) = a
fromError a (Fail _) = a

fromEither :: Either e a -> Error e a
fromEither (Left e) = Fail e
fromEither (Right a) = Success a

toEither :: Error e a -> Either e a
toEither (Fail e) = Left e
toEither (Success a) = Right a

fromMaybe :: Maybe a -> Error () a
fromMaybe Nothing = Fail ()
fromMaybe (Just a) = Success a

toMaybe :: Error e a -> Maybe a
toMaybe (Fail _) = Nothing
toMaybe (Success a) = Just a

instance Monoidal Error where
  mmap f _ (Fail x) = Fail (f x)
  mmap _ g (Success y) = Success (g y)

  assoc1 (Fail a) = Fail (Fail a)
  assoc1 (Success (Fail b)) = Fail (Success b)
  assoc1 (Success (Success c)) = Success c

  assoc2 (Fail (Fail a)) = Fail a
  assoc2 (Fail (Success b)) = Success (Fail b)
  assoc2 (Success c) = Success (Success c)

instance Symmetric Error where
  commute (Fail a) = Success a
  commute (Success a) = Fail a

instance Applicative f => Strong f Error where
  strength1 (Success a) = pure $ Success a
  strength1 (Fail a) = Fail <$> a

  strength2 (Success a) = Success <$> a
  strength2 (Fail a) = pure $ Fail a
