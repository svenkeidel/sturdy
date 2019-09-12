{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Fix.Reuse where

import Control.Arrow
import Control.Arrow.Fix

import Data.Abstract.Stable

import Data.Metric
import Data.Profunctor
import Data.Monoid

import Text.Printf

class (Arrow c, Profunctor c) => ArrowReuse a b c where
  type Dom c :: *

  -- | Reuse cached results at the cost of precision.
  reuse :: (Monoid m) => (Dom c -> Dom c -> Stable -> b -> m) -> c (a,Stable) m

reuseFirst :: (ArrowChoice c, ArrowReuse a b c) => IterationStrategy c a b
reuseFirst f = proc a -> do
  m <- reuse (\_ _ _ b -> First (Just b)) -< (a,Stable)
  case getFirst m of
    Just b -> returnA -< b
    Nothing -> f -< a
{-# INLINE reuseFirst #-}

reuseExact :: (Eq (Dom c), ArrowChoice c, ArrowReuse a b c) => IterationStrategy c a b
reuseExact = reuseByMetric discrete
{-# INLINE reuseExact #-}

reuseByMetric :: (Ord n, ArrowChoice c, ArrowReuse a b c) => Metric (Dom c) n -> IterationStrategy c a b
reuseByMetric metric f = proc a -> do
  m <- reuse (\a a' _ b -> Just (Measured { measured = metric a a', argument = b })) -< (a,Stable)
  case m of
    Just n -> returnA -< argument n
    Nothing -> f -< a
{-# INLINE reuseByMetric #-}

data Measured a n = Measured { argument :: a, measured :: n }

instance (Show a, Show n) => Show (Measured a n) where
  show m = printf "%s@%s" (show (argument m)) (show (measured m))

instance Ord n => Semigroup (Measured a n) where
  m1 <> m2
    | measured m1 <= measured m2 = m1
    | otherwise                  = m2
  {-# INLINE (<>) #-}
