{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Error where

import Control.Monad
import Control.Monad.Except
import Data.Order
import Data.Widening

-- | Error is an Either-like type with the special ordering Error ⊑ Success.
-- Left and Right of the regular Either type, on the other hand are incomparable.
data Error e a = Bot | Error e | Success a
  deriving (Eq, Functor)

instance (Show e,Show a) => Show (Error e a) where
  show Bot = "⊥"
  show (Error e) = "Error " ++ show e
  show (Success a) = show a

instance PreOrd a => PreOrd (Error e a) where
  Bot ⊑ _ = True
  Error _ ⊑ Success _ = True
  Error _ ⊑ Error _ = True
  Success x ⊑ Success y = x ⊑ y
  _ ⊑ _ = False

  Bot ≈ Bot = True
  Error _ ≈ Error _ = True
  Success x ≈ Success y = x ≈ y
  _ ≈ _ = False

instance Complete a => Complete (Error e a) where
  Bot ⊔ b = b
  a ⊔ Bot = a
  Error _ ⊔ b = b
  a ⊔ Error _ = a
  Success x ⊔ Success y = Success (x ⊔ y)

instance (PreOrd b) => LowerBounded (Error a b) where
  bottom = Bot

instance UpperBounded a => UpperBounded (Error e a) where
  top = Success top

instance Widening a => Widening (Error e a) where
  Bot ▽ b = b
  a ▽ Bot = a
  Error _ ▽ b = b
  a ▽ Error _ = a
  Success x ▽ Success y = Success (x ▽ y)

instance MonadError e (Error e) where
  throwError = Error
  catchError Bot _ = Bot
  catchError (Error e) f = f e
  catchError (Success a) _ = Success a

instance Applicative (Error e) where
  pure = return
  (<*>) = ap

instance Monad (Error e) where
  return = Success
  Bot >>= _ = Bot
  Error e >>= _ = Error e
  Success a >>= k = k a

fromEither :: Either e a -> Error e a
fromEither (Left e) = Error e
fromEither (Right a) = Success a

-- toEither :: Error e a -> Either e a
-- toEither (Error e) = Left e
-- toEither (Success a) = Right a

fromMaybe :: Maybe a -> Error () a
fromMaybe Nothing = Error ()
fromMaybe (Just a) = Success a

-- toMaybe :: Error e a -> Maybe a
-- toMaybe (Error _) = Nothing
-- toMaybe (Success a) = Just a

unzipError :: Error e (a1,a2) -> (Error e a1, Error e a2)
unzipError Bot = (Bot,Bot)
unzipError (Error e) = (Error e, Error e)
unzipError (Success (a1, a2)) = (Success a1, Success a2)

zipError :: Eq e => (Error e a1, Error e a2) -> Error e (a1,a2)
zipError (Bot, Bot) = Bot
zipError (Error e1, Error e2) | e1 == e2 = Error e1
zipError (Success a1, Success a2) = Success (a1, a2)

mapError :: (a1 -> b1) -> (a2 -> b2) -> Error e (a1, a2) -> Error e (b1, b2)
mapError _ _ Bot = Bot
mapError _ _ (Error e) = Error e
mapError f1 f2 (Success (a1, a2)) = Success (f1 a1, f2 a2)

-- | The type Error has the correct ordering for our use case compared to the either type
-- instance (PreOrd (m (Error e a)), Functor m) => PreOrd (ExceptT e m a) where
--   ExceptT f ⊑ ExceptT g = fmap fromEither f ⊑ fmap fromEither g

-- instance (Complete (m (Error e a)), Functor m) => Complete (ExceptT e m a) where
--   ExceptT f ⊔ ExceptT g = ExceptT $ fmap toEither (fmap fromEither f ⊔ fmap fromEither g)

