{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Error where

import Control.Monad
import Control.Monad.Except
import Data.Order

-- | Error is an Either-like type with the special ordering Error ⊑ Success.
-- Left and Right of the regular Either type, on the other hand are incomparable.
data Error e a = Error e | Success a
  deriving (Eq, Functor, Show)

instance PreOrd a => PreOrd (Error e a) where
  Error _ ⊑ Success _ = True
  Error _ ⊑ Error _ = True
  Success x ⊑ Success y = x ⊑ y
  _ ⊑ _ = False

  Error _ ≈ Error _ = True
  Success x ≈ Success y = x ≈ y
  _ ≈ _ = False

instance Complete a => Complete (Error e a) where
  Error _ ⊔ b = b
  a ⊔ Error _ = a
  Success x ⊔ Success y = Success (x ⊔ y)

instance UpperBounded a => UpperBounded (Error e a) where
  top = Success top

instance MonadError e (Error e) where
  throwError = Error
  catchError (Error e) f = f e
  catchError (Success a) _ = Success a

instance Applicative (Error e) where
  pure = return
  (<*>) = ap

instance Monad (Error e) where
  return = Success
  Error e >>= _ = Error e
  Success a >>= k = k a

fromEither :: Either e a -> Error e a
fromEither (Left e) = Error e
fromEither (Right a) = Success a

toEither :: Error e a -> Either e a
toEither (Error e) = Left e
toEither (Success a) = Right a
