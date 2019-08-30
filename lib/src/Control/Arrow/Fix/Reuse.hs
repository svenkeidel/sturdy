{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Fix.Reuse where

import Control.Arrow
import Control.Arrow.Fix

import Data.Measure
import Data.Metric

class ArrowReuse a b c where
  -- | Reuse cached results at the cost of precision.
  reuseStable :: (Show m, Monoid m) => (a -> a -> b -> m) -> c a m

reuseStableByMetric :: (Show b, Show n, Ord n, ArrowChoice c, ArrowReuse a b c) => Metric a n -> IterationStrategy c a b
reuseStableByMetric metric f = proc a -> do
  m <- reuseStable (\a a' b -> Just (Measured { measured = metric a a', argument = b })) -< a
  case m of
    Just n -> returnA -< argument n
    Nothing -> f -< a
{-# INLINE reuseStableByMetric #-}
