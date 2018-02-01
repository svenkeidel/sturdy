{-# LANGUAGE DeriveGeneric #-}
module Data.Bounded where

import Prelude hiding (Bounded)
    
import Data.Order
import Data.Widening
import Data.Hashable

import GHC.Generics

-- |Bounded invokes the least upper bound operator until the element reaches a given limit.
data Bounded a = Bounded a a deriving (Eq,Generic)

instance Show a => Show (Bounded a) where
  show (Bounded a b) = "min {" ++ show a ++ ", " ++ show b ++ "}"

instance Hashable a => Hashable (Bounded a)

instance PreOrd a => PreOrd (Bounded a) where
  Bounded b1 e1 ⊑ Bounded b2 e2 = b1 ≈ b2 && e1 ⊑ e2

instance Complete a => Complete (Bounded a) where
  Bounded b1 e1 ⊔ Bounded b2 e2 = Bounded (b1 ⊔ b2) (e1 ⊔ e2)

instance (Complete a, UpperBounded a) => Widening (Bounded a) where
  Bounded b1 e1 ▽ Bounded b2 e2
    | e1 ⊑ b || e2 ⊑ e1 = Bounded b (e1 ⊔ e2)
    | otherwise = Bounded b top
    where
      b = b1 ⊔ b2

-- | Arithmetic operations are lifted to the elements and bounds are joined. This makes all operations associative.
instance (Num a, LowerBounded a, Complete a) => Num (Bounded a) where
  (+) = lift2 (+)
  (*) = lift2 (*)
  abs = lift abs
  signum = lift signum
  negate = lift negate
  fromInteger a = Bounded bottom (fromInteger a)

lift :: (a -> a) -> Bounded a -> Bounded a
lift f (Bounded b a) = Bounded b (f a)

lift2 :: Complete a => (a -> a -> a) -> Bounded a -> Bounded a -> Bounded a
lift2 f (Bounded b1 a1) (Bounded b2 a2) = Bounded (b1 ⊔ b2) (f a1 a2)
