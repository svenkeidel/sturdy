{-# LANGUAGE DeriveGeneric #-}
module Data.Interval where

import Data.Hashable
import Data.Order
import GHC.Generics

-- | Intervals represent ranges of numbers. Bot represents the empty interval
data Interval n = Bot | Interval n n
  deriving (Eq,Show,Generic)

instance PreOrd x => PreOrd (Interval x) where
  Bot ⊑ _ = True
  _ ⊑ Bot = False
  Interval i1 i2 ⊑ Interval j1 j2 = j1 ⊑ i1 && i2 ⊑ j2

instance (Complete x, CoComplete x) => Complete (Interval x) where
  Bot ⊔ x = x
  x ⊔ Bot = x
  Interval i1 i2 ⊔  Interval j1 j2 = Interval (i1 ⊓ j1) (i2 ⊔ j2)

instance (Num n, Complete n, LowerBounded n, CoComplete n, UpperBounded n) => Num (Interval n) where
  Bot + _ = Bot
  _ + Bot = Bot
  Interval i1 i2 + Interval j1 j2 = Interval (i1+ j1) (i2+ j2)
  (*) = withBounds2 (*)
  negate Bot = Bot
  negate (Interval i1 i2) = Interval (negate i2) (negate i1)
  abs = withBounds1 abs
  signum = withBounds1 signum
  fromInteger = constant . fromInteger

instance (Fractional n, Complete n, LowerBounded n, CoComplete n, UpperBounded n) => Fractional (Interval n) where
  Bot / _ = Bot
  _ / Bot = Bot
  Interval i1 i2 / Interval j1 j2
    | j1 ⊑ 0 && 0 ⊑ j2 = Interval bottom top
    | otherwise = withBounds2 (/) (Interval i1 i2) (Interval j1 j2)
  fromRational = constant . fromRational

instance Hashable n => Hashable (Interval n)

constant :: n -> Interval n
constant x = Interval x x

withBounds1 :: (Complete n, CoComplete n) => (n -> n) -> Interval n -> Interval n
withBounds1 f (Interval i1 i2) = Interval (f i1 ⊓ f i2) (f i1 ⊔ f i2)
withBounds1 _ Bot = Bot

withBounds2 :: (Complete n, LowerBounded n, CoComplete n, UpperBounded n) =>
               (n -> n -> n) -> Interval n -> Interval n -> Interval n
withBounds2 f (Interval i1 i2) (Interval j1 j2) =
    Interval (glb [ f x y | x <- [i1,i2], y <- [j1,j2]]) 
             (lub [ f x y | x <- [i1,i2], y <- [j1,j2]])
withBounds2 _ Bot _ = Bot
withBounds2 _ _ Bot = Bot

instance PreOrd n => LowerBounded (Interval n) where
  bottom = Bot

instance (LowerBounded n, UpperBounded n) => UpperBounded (Interval n) where
  top = Interval bottom top
