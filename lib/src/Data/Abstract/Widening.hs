{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.Abstract.Widening where

import Control.Arrow
import Data.Functor.Identity

import Data.Order

class PreOrd a => Widening a where
  -- ▽ has to be an upper bound operator, i.e. x ⊑ x ▽ y and y ⊑ x ▽ y.
  -- Furthermore, iterating ▽ on an ascending chain has to stabilize:
  -- Let x1, x2, ... xn be an infinite ascending chain, then x1, x1 ▽ x2, (x1 ▽ x2) ▽ x3, ... (similar to left fold) has a limit.
  (▽) :: a -> a -> a

  default (▽) :: Complete a => a -> a -> a
  (▽) = (⊔)

instance (Widening a,Widening b) => Widening (a,b) where
  (a1,b1) ▽ (a2,b2) = (a1 ▽ a2, b1 ▽ b2) 

instance (Widening a,Widening b,Widening c) => Widening (a,b,c) where
  (a1,b1,c1) ▽ (a2,b2,c2) = (a1 ▽ a2, b1 ▽ b2,  c1 ▽ c2) 

instance Widening ()

instance Widening b => Widening (a -> b) where
  f ▽ g = \x -> f x ▽ g x

-- it holds that for all f, lfp f ⊑ widenedLfp f
widenedLfp :: (Widening a, LowerBounded a) => (a -> a) -> a
widenedLfp f = go bottom
  where
    go x | f x ⊑ x = x
         | otherwise = x ▽ f (go x)

-- |Invokes the least upper bound operator a certain number of times.
data Fueled a = Fueled Int a

instance PreOrd a => PreOrd (Fueled a) where
  Fueled f1 e1 ⊑ Fueled f2 e2 = f1 <= f2 && e1 ⊑ e2

instance (Complete a, UpperBounded a) => Widening (Fueled a) where
  Fueled f1 e1 ▽ Fueled _ e2
    | f1 > 0 || e2 ⊑ e1 = Fueled (f1-1) (e1 ⊔ e2)
    | otherwise = Fueled 0 top

instance Widening (m b) => Widening (Kleisli m a b) where
  Kleisli f ▽ Kleisli g = Kleisli $ f ▽ g

instance Widening a => Widening (Identity a) where
  Identity a ▽ Identity b = Identity $ a ▽ b
