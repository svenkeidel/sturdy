{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
module Data.Abstract.UncertainResult where

import Control.Arrow
import Control.Monad
import Control.Applicative
import Data.Order

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

fromMaybe :: Maybe a -> UncertainResult a
fromMaybe m = case m of
  Just a -> Success a
  Nothing -> Fail
