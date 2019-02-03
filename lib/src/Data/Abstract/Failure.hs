{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Abstract.Failure where

import Control.Monad
import Control.Monad.Except
import Data.Abstract.FreeCompletion
import Data.Abstract.Widening
import Data.Bifunctor
import Data.Hashable
import Data.Order

import Data.Monoidal

import GHC.Generics (Generic, Generic1)
import Control.DeepSeq

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
widening _ (Fail _) b = b
widening _ a (Fail _) = a
widening w (Success x) (Success y) = Success (x `w` y)

instance (PreOrd e, PreOrd a, Complete (FreeCompletion a)) => Complete (FreeCompletion (Failure e a)) where
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

instance Monoidal Failure where
  mmap f _ (Fail x) = Fail (f x)
  mmap _ g (Success y) = Success (g y)

  assoc1 (Fail a) = Fail (Fail a)
  assoc1 (Success (Fail b)) = Fail (Success b)
  assoc1 (Success (Success c)) = Success c

  assoc2 (Fail (Fail a)) = Fail a
  assoc2 (Fail (Success b)) = Success (Fail b)
  assoc2 (Success c) = Success (Success c)

instance Symmetric Failure where
  commute (Fail a) = Success a
  commute (Success a) = Fail a

instance Applicative f => Strong f Failure where
  strength1 (Success a) = pure $ Success a
  strength1 (Fail a) = Fail <$> a

  strength2 (Success a) = Success <$> a
  strength2 (Fail a) = pure $ Fail a

-- instance Applicative f => StrongMonad f Failure where
--   mstrength (Success a) = fmap Success a
--   mstrength (Fail a) = fmap Fail a

-- instance StrongMonad (Failure e) (,) where
--   mstrength (Success a,Success b) = Success (a,b)
--   mstrength (Fail e,_) = Fail e
--   mstrength (_,Fail e) = Fail e

-- instance Distributive (,) Failure where
--   distribute = Iso distTo distFrom
--     where
--       distTo :: (a,Failure b c) -> Failure (a,b) (a,c)
--       distTo (a,Fail b) = Fail (a,b)
--       distTo (a,Success c) = Success (a,c)

--       distFrom :: Failure (a,b) (a,c) -> (a,Failure b c)
--       distFrom (Fail (a,b)) = (a,Fail b)
--       distFrom (Success (a,c)) = (a,Success c)

-- instance Distributive Either Failure where
--   distribute = Iso distTo distFrom
--     where
--       distTo :: Either a (Failure b c) -> Failure (Either a b) (Either a c)
--       distTo (Left a) = Fail (Left a)
--       distTo (Right (Fail b)) = Fail (Right b)
--       distTo (Right (Success c)) = Success (Right c)
  
--       distFrom :: Failure (Either a b) (Either a c) -> Either a (Failure b c)
--       distFrom (Fail (Left a)) = Left a
--       distFrom (Fail (Right b)) = Right (Fail b)
--       distFrom (Success (Left a)) = Left a
--       distFrom (Success (Right c)) = Right (Success c)

-- instance Distributive Failure Either where
--   distribute = Iso distTo distFrom
--     where
--       distTo :: Failure a (Either b c) -> Either (Failure a b) (Failure a c)
--       distTo (Fail a) = Right (Fail a)
--       distTo (Success (Left b)) = Left (Success b)
--       distTo (Success (Right c)) = Right (Success c)
  
--       distFrom :: Either (Failure a b) (Failure a c) -> Failure a (Either b c)
--       distFrom (Left (Fail a)) = Fail a
--       distFrom (Left (Success b)) = Success (Left b)
--       distFrom (Right (Fail a)) = Fail a
--       distFrom (Right (Success c)) = Success (Right c)
