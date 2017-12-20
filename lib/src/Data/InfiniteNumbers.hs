{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.InfiniteNumbers where

import Data.Order
import Data.FreeCoCompletion

data InfiniteNumber a = NegInfinity | Number a | Infinity deriving (Eq,Ord,Show)

isNegative :: (Eq a,Num a) => a -> Bool
isNegative x = signum x == -1

mult :: (Num a, Ord a) => a -> InfiniteNumber a -> InfiniteNumber a
mult a e
  | a < 0 = case e of
      NegInfinity -> Infinity
      Infinity -> NegInfinity
      Number b -> Number (a * b)
  | a == 0 = Number 0
  | a > 0 = case e of
      NegInfinity -> NegInfinity
      Infinity -> Infinity
      Number b -> Number (a * b)

instance (Num a, Ord a) => Num (InfiniteNumber a) where
  NegInfinity + Infinity = error "Addition of positive and negative infinity is undefined"
  Infinity + NegInfinity = error "Addition of positive and negative infinity is undefined"
  Infinity + _ = Infinity
  _ + Infinity = Infinity
  NegInfinity + _ = NegInfinity
  _ + NegInfinity = NegInfinity
  Number n + Number m = Number (n + m)

  Number n * e = mult n e
  e * Number m = mult m e
  Infinity * Infinity = Infinity
  Infinity * NegInfinity = NegInfinity
  NegInfinity * Infinity = NegInfinity
  NegInfinity * NegInfinity = Infinity

  abs (Number n) = Number (abs n)
  abs _ = Infinity

  signum Infinity = Number 1
  signum NegInfinity = Number (-1)
  signum (Number n) = Number (signum n)

  fromInteger = Number . fromInteger

  negate Infinity = NegInfinity
  negate NegInfinity = Infinity
  negate (Number n) = Number (negate n)

instance PreOrd a => PreOrd (InfiniteNumber a) where
  NegInfinity ⊑ _ = True
  _ ⊑ NegInfinity = False
  _ ⊑ Infinity = True
  Infinity ⊑ _ = False
  Number n ⊑ Number m = n ⊑ m

instance (PreOrd a, Complete a) => Complete (InfiniteNumber a) where
  NegInfinity ⊔ x = x
  x ⊔ NegInfinity = x
  Infinity ⊔ _ = Infinity
  _ ⊔ Infinity = Infinity
  Number n ⊔ Number m = Number (n ⊔ m)

instance (PreOrd a, CoComplete a) => CoComplete (InfiniteNumber a) where
  NegInfinity ⊓ _ = NegInfinity
  _ ⊓ NegInfinity = NegInfinity
  Infinity ⊓ x = x
  x ⊓ Infinity = x
  Number n ⊓ Number m = Number (n ⊓ m)
