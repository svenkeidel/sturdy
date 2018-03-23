{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Abstract.Error where

import Control.Monad
import Control.Monad.Except
import Data.Order
import Data.Abstract.Widening

-- | Error is an Either-like type with the special ordering Error ⊑ Success.
-- Left and Right of the regular Either type, on the other hand are incomparable.
data Error e a = Bot | Fail e | Success a
  deriving (Eq, Functor)

instance (Show e,Show a) => Show (Error e a) where
  show Bot = "⊥"
  show (Fail e) = "Error " ++ show e
  show (Success a) = show a

instance PreOrd a => PreOrd (Error e a) where
  Bot ⊑ _ = True
  Fail _ ⊑ Success _ = True
  Fail _ ⊑ Fail _ = True
  Success x ⊑ Success y = x ⊑ y
  _ ⊑ _ = False

  Bot ≈ Bot = True
  Fail _ ≈ Fail _ = True
  Success x ≈ Success y = x ≈ y
  _ ≈ _ = False

instance Complete a => Complete (Error e a) where
  Bot ⊔ b = b
  a ⊔ Bot = a
  Fail _ ⊔ b = b
  a ⊔ Fail _ = a
  Success x ⊔ Success y = Success (x ⊔ y)

instance (PreOrd b) => LowerBounded (Error a b) where
  bottom = Bot

instance UpperBounded a => UpperBounded (Error e a) where
  top = Success top

instance Widening a => Widening (Error e a) where
  Bot ▽ b = b
  a ▽ Bot = a
  Fail _ ▽ b = b
  a ▽ Fail _ = a
  Success x ▽ Success y = Success (x ▽ y)

instance MonadError e (Error e) where
  throwError = Fail
  catchError Bot _ = Bot
  catchError (Fail e) f = f e
  catchError (Success a) _ = Success a

instance Applicative (Error e) where
  pure = return
  (<*>) = ap

instance Monad (Error e) where
  return = Success
  Bot >>= _ = Bot
  Fail e >>= _ = Fail e
  Success a >>= k = k a

fromError :: a -> Error e a -> a
fromError _ (Success a) = a
fromError a (Fail _) = a
fromError a Bot = a

fromEither :: Either e a -> Error e a
fromEither (Left e) = Fail e
fromEither (Right a) = Success a

-- toEither :: Error e a -> Either e a
-- toEither (Error e) = Left e
-- toEither (Success a) = Right a

fromMaybe :: Maybe a -> Error () a
fromMaybe Nothing = Fail ()
fromMaybe (Just a) = Success a

-- toMaybe :: Error e a -> Maybe a
-- toMaybe (Error _) = Nothing
-- toMaybe (Success a) = Just a

unzipError :: Error e (a1,a2) -> (Error e a1, Error e a2)
unzipError Bot = (Bot,Bot)
unzipError (Fail e) = (Fail e, Fail e)
unzipError (Success (a1, a2)) = (Success a1, Success a2)

zipError :: Eq e => (Error e a1, Error e a2) -> Error e (a1,a2)
zipError (Bot, Bot) = Bot
zipError (Fail e1, Fail e2) | e1 == e2 = Fail e1
zipError (Success a1, Success a2) = Success (a1, a2)
zipError _ = error "cannot zip these error values"

mapError :: (a1 -> b1) -> (a2 -> b2) -> Error e (a1, a2) -> Error e (b1, b2)
mapError _ _ Bot = Bot
mapError _ _ (Fail e) = Fail e
mapError f1 f2 (Success (a1, a2)) = Success (f1 a1, f2 a2)

