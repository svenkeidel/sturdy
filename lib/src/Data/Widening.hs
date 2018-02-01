module Data.Widening where

import Prelude hiding (Bounded)

import Data.Order

class PreOrd a => Widening a where
  -- ▽ has to be an upper bound operator, i.e. x ⊑ x ▽ y and y ⊑ x ▽ y.
  -- Furthermore, iterating ▽ on an ascending chain has to stabilize:
  -- Let x1, x2, ... xn be an infinite ascending chain, then x1, x1 ▽ x2, (x1 ▽ x2) ▽ x3, ... has a limit.
  (▽) :: a -> a -> a

-- it holds that for all f, lfp f ⊑ widenedLfp f
widenedLfp :: (Widening a, LowerBounded a) => (a -> a) -> a
widenedLfp f = go bottom
  where
    go x | f x ⊑ x = x
         | otherwise = x ▽ f (go x)

class PreOrd a => Narrowing a where
  -- For △ holds that, i.e. if x ⊑ y then y ⊑ x ▽ y ⊑ x.
  -- Furthermore, iterating ▽ on an descending chain has to stabilize:
  -- Let x1, x2, ... xn be an infinite descending chain, then x1, x1 ▽ x2, (x1 ▽ x2) ▽ x3, ... has a limit.
  (△) :: a -> a -> a

-- Notice that widening and narrowing are *not* dual.

-- We define here some simple widening operators.

-- |Invokes the least upper bound operator until the element reaches a given limit.
data Bounded a = Bounded a a

instance PreOrd a => PreOrd (Bounded a) where
  Bounded _ e1 ⊑ Bounded _ e2 = e1 ⊑ e2

instance (Complete a, UpperBounded a) => Widening (Bounded a) where
  Bounded b1 e1 ▽ Bounded _ e2
    | e1 ⊑ b1 || e2 ⊑ e1 = Bounded b1 (e1 ⊔ e2)
    | otherwise = Bounded b1 top

-- |Invokes the least upper bound operator a certain number of times.
data Fueled a = Fueled Int a

instance PreOrd a => PreOrd (Fueled a) where
  Fueled f1 e1 ⊑ Fueled f2 e2 = f1 <= f2 && e1 ⊑ e2

instance (Complete a, UpperBounded a) => Widening (Fueled a) where
  Fueled f1 e1 ▽ Fueled _ e2
    | f1 > 0 || e2 ⊑ e1 = Fueled (f1-1) (e1 ⊔ e2)
    | otherwise = Fueled 0 top



