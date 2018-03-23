{-# LANGUAGE DeriveFunctor #-}
module Data.Concrete.Error where

import Data.Hashable

data Error e x = Fail e | Success x
  deriving (Eq, Functor)

instance (Show e,Show a) => Show (Error e a) where
  show (Fail e) = "Error " ++ show e
  show (Success a) = show a

instance (Hashable e, Hashable a) => Hashable (Error e a) where
  hashWithSalt s (Fail e) = s `hashWithSalt` (1 :: Int) `hashWithSalt` e
  hashWithSalt s (Success a) = s `hashWithSalt` (2 :: Int) `hashWithSalt` a
