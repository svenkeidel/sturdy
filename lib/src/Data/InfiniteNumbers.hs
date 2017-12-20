{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.InfiniteNumbers where

import Data.Order
import Data.FreeCoCompletion

data InfiniteNumber a = NegInfinity | Number a | Infinity deriving (Eq,Ord)

isNegative :: (Eq a,Num a) => a -> Bool
isNegative x = signum x == -1
