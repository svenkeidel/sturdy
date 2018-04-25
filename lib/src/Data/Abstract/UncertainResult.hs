{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
module Data.Abstract.UncertainResult where

import Control.Arrow
import Control.Monad
import Control.Applicative

import Data.Abstract.FreeCompletion
import Data.Order
import Data.Traversable

data UncertainResult a
  = Success a
  | Fail
  | SuccessOrFail a
  deriving (Eq, Show)

instance Functor UncertainResult where
  fmap f = proc r -> case r of
    Success a -> Success ^<< f -< a
    Fail -> returnA -< Fail
    SuccessOrFail a -> SuccessOrFail ^<< f -< a

instance Applicative UncertainResult where
  pure = return
  (<*>) = ap

instance Monad UncertainResult where
  return = arr Success
  f >>= k = mu (fmap k f)
    where
      mu r = case r of
        Success (Success x) -> Success x
        Success (SuccessOrFail x) -> SuccessOrFail x
        SuccessOrFail (SuccessOrFail x) -> SuccessOrFail x
        SuccessOrFail (Success x) -> SuccessOrFail x
        _ -> Fail

instance Alternative UncertainResult where
  empty = mzero
  (<|>) = mplus 

instance MonadPlus UncertainResult where
  mzero = Fail
  mplus f g = case (f,g) of
    (Success x, Success _) -> Success x
    (Success x, Fail) -> SuccessOrFail x
    (Fail, Success y) -> SuccessOrFail y
    (Fail, Fail) -> Fail
    (SuccessOrFail x, Success _) -> SuccessOrFail x
    (Success x, SuccessOrFail _) -> SuccessOrFail x
    (SuccessOrFail x, Fail) -> SuccessOrFail x
    (Fail, SuccessOrFail y) -> SuccessOrFail y
    (SuccessOrFail x, SuccessOrFail _) -> SuccessOrFail x

instance Foldable UncertainResult where
  foldMap = foldMapDefault

instance Traversable UncertainResult where
  traverse f (Success x) = Success <$> f x
  traverse _ Fail = pure Fail
  traverse f (SuccessOrFail x) = SuccessOrFail <$> f x

instance PreOrd a => PreOrd (UncertainResult a) where
  m1 ⊑ m2 = case (m1,m2) of
    (Fail, Fail) -> True
    (Success a, Success b) -> a ⊑ b
    (Success a, SuccessOrFail b) -> a ⊑ b
    (Fail, SuccessOrFail _) -> True
    (_, _) -> False

instance Complete a => Complete (UncertainResult a) where
  m1 ⊔ m2 = case (m1,m2) of
    (Success x, Success y) -> Success (x ⊔ y)
    (Success x, Fail) -> SuccessOrFail x
    (Fail, Success y) -> SuccessOrFail y
    (Fail, Fail) -> Fail
    (SuccessOrFail x, Success y) -> SuccessOrFail (x ⊔ y)
    (Success x, SuccessOrFail y) -> SuccessOrFail (x ⊔ y)
    (SuccessOrFail x, Fail) -> SuccessOrFail x
    (Fail, SuccessOrFail y) -> SuccessOrFail y
    (SuccessOrFail x, SuccessOrFail y) -> SuccessOrFail (x ⊔ y)

instance PreOrd a => LowerBounded (UncertainResult a) where
  bottom = Fail

instance (PreOrd a, Complete (FreeCompletion a)) => Complete (FreeCompletion (UncertainResult a)) where
  Lower m1 ⊔ Lower m2 = sequenceA (fmap Lower m1 ⊔ fmap Lower m2)
  _ ⊔ _ = Top

fromMaybe :: Maybe a -> UncertainResult a
fromMaybe m = case m of
  Just a -> Success a
  Nothing -> Fail
