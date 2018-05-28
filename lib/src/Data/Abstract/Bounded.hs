{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Abstract.Bounded where

import Prelude hiding (Bounded,(==),(/),(<),Ordering)
import qualified Prelude as P
    
import Data.Order
import Data.Hashable
import Data.Numeric

import Data.Abstract.Equality
import Data.Abstract.Ordering
import Data.Abstract.Widening
import Data.Abstract.PropagateError

-- |Bounded invokes the least upper bound operator until the element reaches a given limit.
data Bounded a = Bounded a a

bounded :: (?bound :: a) => a -> Bounded a
bounded = Bounded ?bound

instance Eq a => Eq (Bounded a) where
  Bounded _ x == Bounded _ y = x P.== y

instance Show a => Show (Bounded a) where
  show (Bounded _ b) = show b

instance Hashable a => Hashable (Bounded a) where
  hashWithSalt s (Bounded _ b) = hashWithSalt s b

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

instance LowerBounded a => LowerBounded (Bounded a) where
  bottom = Bounded bottom bottom

-- | Arithmetic operations are lifted to the elements and bounds are joined. This makes all operations associative.
instance (Num a, Complete a, UpperBounded a) => Num (Bounded a) where
  (+) = lift2 (+)
  (*) = lift2 (*)
  abs = lift abs
  signum = lift signum
  negate = lift negate
  fromInteger = error "use the constructor Bounded instead of fromInteger"

instance (UpperBounded a, Complete a, Numeric a (Error e)) => Numeric (Bounded a) (Error e) where
  Bounded b1 a1 / Bounded b2 a2
    | (a1 / a2) ⊑ Success (b1 ⊔ b2) = Bounded (b1 ⊔ b2) <$> a1 / a2 
    | otherwise = Success (Bounded (b1 ⊔ b2) top)

instance Equality a => Equality (Bounded a) where
  Bounded _ a == Bounded _ b = a == b

instance Ordering a => Ordering (Bounded a) where
  Bounded _ a < Bounded _ b = a < b

lift :: (UpperBounded a) => (a -> a) -> Bounded a -> Bounded a
lift f (Bounded b a)
  | f a ⊑ b = Bounded b (f a)
  | otherwise = Bounded b top

lift2 :: (Complete a, UpperBounded a) => (a -> a -> a) -> Bounded a -> Bounded a -> Bounded a
lift2 f (Bounded b1 a1) (Bounded b2 a2)
  | f a1 a2 ⊑ (b1 ⊔ b2) = Bounded (b1 ⊔ b2) (f a1 a2)
  | otherwise = Bounded (b1 ⊔ b2) top
