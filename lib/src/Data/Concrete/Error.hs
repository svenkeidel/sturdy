{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Concrete.Error where

import Data.Hashable
import Data.Profunctor

import Control.Monad
import Control.Arrow hiding (ArrowMonad)
import Control.Arrow.Monad

data Error e x = Fail e | Success x
  deriving (Eq, Functor)

instance Applicative (Error e) where
  pure = return
  (<*>) = ap

instance Monad (Error e) where
  return = Success
  Fail e >>= _ = Fail e
  Success a >>= k = k a

instance (ArrowChoice c, Profunctor c) => ArrowFunctor (Error e) c where
  mapA f = lmap toEither (arr Fail ||| rmap Success f)
  {-# INLINE mapA #-}

instance (ArrowChoice c, Profunctor c) => ArrowMonad (Error e) c where
  mapJoinA f = lmap toEither (arr Fail ||| f)
  {-# INLINE mapJoinA #-}

instance (Show e,Show a) => Show (Error e a) where
  show (Fail e) = "Error " ++ show e
  show (Success a) = show a

instance (Hashable e, Hashable a) => Hashable (Error e a) where
  hashWithSalt s (Fail e) = s `hashWithSalt` (1 :: Int) `hashWithSalt` e
  hashWithSalt s (Success a) = s `hashWithSalt` (2 :: Int) `hashWithSalt` a

toEither :: Error e x -> Either e x
toEither (Fail e) = Left e
toEither (Success e) = Right e

-- instance Monoidal Error where
--   mmap f _ (Fail x) = Fail (f x)
--   mmap _ g (Success y) = Success (g y)

--   assoc1 (Fail a) = Fail (Fail a)
--   assoc1 (Success (Fail b)) = Fail (Success b)
--   assoc1 (Success (Success c)) = Success c

--   assoc2 (Fail (Fail a)) = Fail a
--   assoc2 (Fail (Success b)) = Success (Fail b)
--   assoc2 (Success c) = Success (Success c)

-- instance Symmetric Error where
--   commute (Fail a) = Success a
--   commute (Success a) = Fail a

-- instance Distributive (,) Error where
--   distribute1 (a,Fail b) = Fail (a,b)
--   distribute1 (a,Success c) = Success (a,c)

--   distribute2 (Fail (a,b)) = (a,Fail b)
--   distribute2 (Success (a,c)) = (a,Success c)

-- instance Distributive Either Error where
--   distribute1 (Left a) = Fail (Left a)
--   distribute1 (Right (Fail b)) = Fail (Right b)
--   distribute1 (Right (Success c)) = Success (Right c)

--   distribute2 (Fail (Left a)) = Left a
--   distribute2 (Fail (Right b)) = Right (Fail b)
--   distribute2 (Success (Left a)) = Left a
--   distribute2 (Success (Right c)) = Right (Success c)

-- instance Distributive Error Either where
--   distribute1 (Fail a) = Right (Fail a)
--   distribute1 (Success (Left b)) = Left (Success b)
--   distribute1 (Success (Right c)) = Right (Success c)

--   distribute2 (Left (Fail a)) = Fail a
--   distribute2 (Left (Success b)) = Success (Left b)
--   distribute2 (Right (Fail a)) = Fail a
--   distribute2 (Right (Success c)) = Success (Right c)
