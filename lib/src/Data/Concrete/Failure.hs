{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Concrete.Failure where

import Data.Hashable
import Data.Monoidal

import Control.Monad

data Failure e x = Fail e | Success x
  deriving (Eq, Functor)

instance (Show e,Show a) => Show (Failure e a) where
  show (Fail e) = "Failure " ++ show e
  show (Success a) = show a

instance (Hashable e, Hashable a) => Hashable (Failure e a) where
  hashWithSalt s (Fail e) = s `hashWithSalt` (1 :: Int) `hashWithSalt` e
  hashWithSalt s (Success a) = s `hashWithSalt` (2 :: Int) `hashWithSalt` a

instance Applicative (Failure e) where
  pure = return
  (<*>) = ap

instance Monad (Failure e) where
  return = Success
  Fail e >>= _ = Fail e
  Success a >>= k = k a

toEither :: Failure e x -> Either e x
toEither (Fail e) = Left e
toEither (Success e) = Right e

instance Monoidal Failure where
  mmap f _ (Fail x) = Fail (f x)
  mmap _ g (Success y) = Success (g y)

  assoc = Iso assocTo assocFrom
    where
      assocTo :: Failure a (Failure b c) -> Failure (Failure a b) c
      assocTo (Fail a) = Fail (Fail a)
      assocTo (Success (Fail b)) = Fail (Success b)
      assocTo (Success (Success c)) = Success c

      assocFrom :: Failure (Failure a b) c -> Failure a (Failure b c)
      assocFrom (Fail (Fail a)) = Fail a
      assocFrom (Fail (Success b)) = Success (Fail b)
      assocFrom (Success c) = Success (Success c)

instance Symmetric Failure where
  commute (Fail a) = Success a
  commute (Success a) = Fail a

instance Distributive (,) Failure where
  distribute = Iso distTo distFrom
    where
    distTo :: (a,Failure b c) -> Failure (a,b) (a,c)
    distTo (a,Fail b) = Fail (a,b)
    distTo (a,Success c) = Success (a,c)

    distFrom :: Failure (a,b) (a,c) -> (a,Failure b c)
    distFrom (Fail (a,b)) = (a,Fail b)
    distFrom (Success (a,c)) = (a,Success c)

instance Distributive Either Failure where
  distribute = Iso distTo distFrom
    where
      distTo :: Either a (Failure b c) -> Failure (Either a b) (Either a c)
      distTo (Left a) = Fail (Left a)
      distTo (Right (Fail b)) = Fail (Right b)
      distTo (Right (Success c)) = Success (Right c)

      distFrom :: Failure (Either a b) (Either a c) -> Either a (Failure b c)
      distFrom (Fail (Left a)) = Left a
      distFrom (Fail (Right b)) = Right (Fail b)
      distFrom (Success (Left a)) = Left a
      distFrom (Success (Right c)) = Right (Success c)

instance Distributive Failure Either where
  distribute = Iso distTo distFrom
    where
      distTo :: Failure a (Either b c) -> Either (Failure a b) (Failure a c)
      distTo (Fail a) = Right (Fail a)
      distTo (Success (Left b)) = Left (Success b)
      distTo (Success (Right c)) = Right (Success c)

      distFrom :: Either (Failure a b) (Failure a c) -> Failure a (Either b c)
      distFrom (Left (Fail a)) = Fail a
      distFrom (Left (Success b)) = Success (Left b)
      distFrom (Right (Fail a)) = Fail a
      distFrom (Right (Success c)) = Success (Right c)
