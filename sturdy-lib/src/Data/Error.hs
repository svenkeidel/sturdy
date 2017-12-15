{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Error where

import Control.Monad

-- | Error is an Either-like type with the special ordering Error ⊑ Success.
-- Left and Right of the regular Either type, on the other hand are incomparable.
data Error e a = Error e | Success a
  deriving (Functor)

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
