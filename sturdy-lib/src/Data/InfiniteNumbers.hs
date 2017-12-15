{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.InfiniteNumbers where

import Data.Order
import Data.FreeCoCompletion

data InfiniteNumber a = NegInfinity | Number a | Infinity deriving (Eq,Ord)

isNegative :: (Eq a,Num a) => a -> Bool
isNegative x = signum x == -1

instance (Eq a,Num a) => Num (FreeCoCompletion (InfiniteNumber a)) where
  Bottom + _ = Bottom
  _ + Bottom = Bottom
  Greater Infinity + Greater NegInfinity = Bottom
  Greater NegInfinity + Greater Infinity = Bottom
  Greater (Number a) + Greater (Number b) = Greater (Number (a+b))
  Greater Infinity + _ = Greater Infinity
  _ + Greater Infinity = Greater Infinity
  Greater NegInfinity + _ = Greater NegInfinity
  _ + Greater NegInfinity = Greater NegInfinity

  Bottom * _ = Bottom
  _ * Bottom = Bottom
  Greater (Number a) * Greater (Number b) = Greater (Number (a*b))
  Greater (Number 0) * _ = Greater (Number 0)
  Greater Infinity * (Greater (Number (isNegative -> True))) = Greater NegInfinity
  Greater Infinity * (Greater NegInfinity) = Greater NegInfinity
  Greater Infinity * _ = Greater Infinity
  Greater NegInfinity * (Greater (Number (isNegative -> True))) = Greater Infinity
  Greater NegInfinity * (Greater NegInfinity) = Greater Infinity
  Greater NegInfinity * _ = Greater NegInfinity
  a * b = b * a
  
  abs (Greater (Number a)) = Greater (Number (abs a))
  abs (Greater NegInfinity) = Greater Infinity
  abs (Greater Infinity) = Greater Infinity
  abs Bottom = Bottom

  negate (Greater (Number a)) = Greater (Number (negate a))
  negate (Greater Infinity) = Greater NegInfinity
  negate (Greater NegInfinity) = Greater Infinity
  negate Bottom = Bottom

  signum (Greater (Number a)) = Greater (Number (signum a))
  signum (Greater Infinity) = Greater (Number 1)
  signum (Greater NegInfinity) = Greater (Number (-1))
  signum Bottom = Bottom

  fromInteger = Greater . Number . fromInteger

instance (Eq a,Fractional a) => Fractional (FreeCoCompletion (InfiniteNumber a)) where
  Bottom / _ = Bottom
  _ / Bottom = Bottom
  Greater Infinity / Greater (Number a)
    | a == 0 = Bottom
    | isNegative a = Greater NegInfinity
    | otherwise = Greater Infinity
  Greater Infinity / _ = Bottom
  Greater NegInfinity / Greater (Number a)
    | a == 0 = Bottom
    | isNegative a = Greater Infinity
    | otherwise = Greater NegInfinity
  Greater NegInfinity / _ = Bottom
  Greater (Number a) / Greater (Number b)
    | b == 0 = Bottom
    | otherwise = Greater (Number (a/b))
  Greater (Number _) / _ = 0

  fromRational = Greater . Number . fromRational
