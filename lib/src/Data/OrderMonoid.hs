{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.OrderMonoid where

import Data.Order
import Data.Empty

newtype OrderMonoid m = OrderMonoid m deriving (PreOrd,Complete,LowerBounded)

instance Complete m => Semigroup (OrderMonoid m) where
  OrderMonoid m1 <> OrderMonoid m2 = OrderMonoid (m1 ⊔ m2)

instance (LowerBounded m, Complete m) => Monoid (OrderMonoid m) where
  mempty = OrderMonoid bottom
  mappend = (<>)

instance (LowerBounded m, Complete m) => IsEmpty (OrderMonoid m) where
  empty = mempty

instance Show m => Show (OrderMonoid m) where
  show (OrderMonoid m) = show m
