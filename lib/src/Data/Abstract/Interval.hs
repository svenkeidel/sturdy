{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Abstract.Interval where

import Prelude hiding (div,Bool(..),(==),(/),(<),Ordering)
import qualified Prelude as P
import Data.Hashable
import Data.Order
import Data.Numeric

import Data.Abstract.Boolean
import Data.Abstract.Equality
import Data.Abstract.Ordering
import Data.Abstract.Failure
import Data.Abstract.InfiniteNumbers
import Data.Abstract.Widening

import GHC.Generics

-- | Intervals represent ranges of numbers. Bot represents the empty interval
data Interval n = Interval n n
  deriving (Eq,Generic)

instance Show n => Show (Interval n) where
  show (Interval n m) = "["++ show n ++ "," ++ show m ++"]"

instance Ord x => PreOrd (Interval x) where
  Interval i1 i2 ⊑ Interval j1 j2 = j1 <= i1 && i2 <= j2

instance Ord x => Complete (Interval x) where
  Interval i1 i2 ⊔  Interval j1 j2 = Interval (min i1 j1) (max i2 j2)

instance (Num n, Ord n) => Num (Interval n) where
  Interval i1 i2 + Interval j1 j2 = Interval (i1 + j1) (i2 + j2)
  (*) = withBounds2 (*)
  negate (Interval i1 i2) = Interval (negate i2) (negate i1)
  abs (Interval i j)
    | 0 <= i = Interval i j
    | otherwise = Interval 0 (max i j)
  signum = withBounds1 signum
  fromInteger = constant . fromInteger

instance (Integral n, Num n, Ord n) => Numeric (Interval (InfiniteNumber n)) (Failure String) where
  Interval i1 i2 / Interval j1 j2
    | j1 P.== 0 && j2 P.== 0 = Fail "divided by 0 error"
    | j1 P.== 0 && 0  P.< j2 = Fail "divided by 0 error" ⊔ Interval i1 i2 / Interval (j1+1) j2
    | j1 P.<  0 && j2 P.== 0 = Fail "divided by 0 error" ⊔ Interval i1 i2 / Interval j1 (j2-1)
    | j1 P.<  0 && 0  P.< j2 = Fail "divided by 0 error" ⊔ Success (withBounds2 divInf (Interval i1 i2) (Interval j1 j2))
    | otherwise = Success (withBounds2 divInf (Interval i1 i2) (Interval j1 j2))

instance Ord n => Equality (Interval n) where
  Interval i1 i2 == Interval j1 j2
    | i1 P.== i2 && j1 P.== j2 && i1 P.== j1 = True
    | j2 P.< i1 || i2 P.< j1 = False
    | otherwise = Top

instance Ord n => Ordering (Interval n) where
  Interval i1 i2 < Interval j1 j2
    | i2 P.< j1 = True
    | j2 P.<= i1 = False
    | otherwise = Top

instance Hashable n => Hashable (Interval n)

constant :: n -> Interval n
constant x = Interval x x

withBounds1 :: Ord n => (n -> n) -> Interval n -> Interval n
withBounds1 f (Interval i1 i2) = Interval (min (f i1) (f i2)) (max (f i1) (f i2))

withBounds2 :: Ord n => (n -> n -> n) -> Interval n -> Interval n -> Interval n
withBounds2 f (Interval i1 i2) (Interval j1 j2) =
    Interval (minimum [ f x y | x <- [i1,i2], y <- [j1,j2]]) 
             (maximum [ f x y | x <- [i1,i2], y <- [j1,j2]])

instance (Ord n, Bounded n) => UpperBounded (Interval n) where
  top = Interval minBound maxBound

widening :: Ord n => Widening (Interval (InfiniteNumber n))
widening (Interval i1 i2) (Interval j1 j2) =
  (if j1 P.< i1 || j2 P.> i2 then Instable else Stable,
    Interval (if j1 P.< i1 then NegInfinity else j1)
             (if j2 P.> i2 then Infinity else i2))
