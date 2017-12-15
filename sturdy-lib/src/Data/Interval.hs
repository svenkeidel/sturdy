module Data.Interval where

import Data.Order

newtype Interval n = IV (n,n)
  deriving (Eq,Show)

instance PreOrd x => PreOrd (Interval x) where
  IV (i1,i2) ⊑ IV (j1,j2) = j1 ⊑ i1 && i2 ⊑ j2

instance (Complete x, CoComplete x) => Complete (Interval x) where
  IV (i1,i2) ⊔  IV (j1,j2) = IV (i1 ⊓ j1, i2 ⊔ j2)

instance (Num n, Complete n, LowerBounded n, CoComplete n, UpperBounded n) => Num (Interval n) where
  IV (i1,i2) + IV (j1,j2) = IV (i1+j1,i2+j2)
  (*) = withBounds2 (*)
  negate (IV (i1,i2)) = IV (negate i2,negate i1)
  abs = withBounds1 abs
  signum = withBounds1 signum
  fromInteger = constant . fromInteger

instance (Fractional n, Complete n, LowerBounded n, CoComplete n, UpperBounded n) => Fractional (Interval n) where
  IV (i1,i2) / IV (j1,j2)
    | j1 ⊑ 0 && 0 ⊑ j2 = IV (bottom,top)
    | otherwise = withBounds2 (/) (IV (i1,i2)) (IV (j1,j2))
  fromRational = constant . fromRational

constant :: n -> Interval n
constant x = IV (x,x)

withBounds1 :: (Complete n, CoComplete n) => (n -> n) -> Interval n -> Interval n
withBounds1 f (IV (i1,i2)) = IV (f i1 ⊓ f i2, f i1 ⊔ f i2)

withBounds2 :: (Complete n, LowerBounded n, CoComplete n, UpperBounded n) => (n -> n -> n) -> Interval n -> Interval n -> Interval n
withBounds2 f (IV (i1,i2)) (IV (j1,j2)) =
    IV (glb [ f x y | x <- [i1,i2], y <- [j1,j2]],
        lub [ f x y | x <- [i1,i2], y <- [j1,j2]])

