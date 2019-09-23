{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Fix.Reuse
  ( ArrowReuse(..)
  , reuseFirst
  , reuseExact
  , reuseByMetric
  , reuseStableByMetric
  )
where

import Prelude hiding (lookup)
import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Fix.Cache

import Data.Abstract.Stable

import Data.Metric
import Data.Profunctor
import Data.Monoid (First(..))

import Text.Printf

class (Arrow c, Profunctor c) => ArrowReuse a b c where

  -- | Reuse cached results at the cost of precision.
  reuse :: (Show m, Monoid m) => (a -> a -> Stable -> b -> m) -> c (a,Stable) m

reuseFirst :: (ArrowChoice c, ArrowReuse a b c, Show a, Show b) => Stable -> IterationStrategy c a b
reuseFirst st f = proc a -> do
  m <- reuse (\_ a' s' b' -> First (Just (a',b',s'))) -< (a,st)
  case getFirst m of
    Just (_,b,Stable) -> returnA -< b
    Just (a',_,Unstable) -> f -< a'
    Nothing -> f -< a
{-# INLINE reuseFirst #-}

reuseExact :: (ArrowChoice c, ArrowCache a b c) => IterationStrategy c a b
reuseExact f = proc a -> do
  m <- lookup -< a
  case m of
    Just (Stable,b) -> returnA -< b
    _ -> f -< a
{-# INLINE reuseExact #-}

reuseByMetric :: (Show a, Show b, Show n, Ord n, ArrowChoice c, ArrowReuse a b c) => Metric a n -> IterationStrategy c a b
reuseByMetric metric = reuseByMetric_ (\s a a' -> Product s (metric a a')) Unstable
{-# INLINE reuseByMetric #-}

reuseStableByMetric :: (Show a, Show b, Show n, Ord n, ArrowChoice c, ArrowReuse a b c) => Metric a n -> IterationStrategy c a b
reuseStableByMetric metric = reuseByMetric_ (const metric) Stable
{-# INLINE reuseStableByMetric #-}

reuseByMetric_ :: (Show a, Show b, Show n, Ord n, ArrowChoice c, ArrowReuse a b c) => (Stable -> Metric a n) -> Stable -> IterationStrategy c a b
reuseByMetric_ metric st f = proc a -> do
  m <- reuse (\a a' s' b' -> Just (Measured { input = a', output = b', stable = s', measured = metric s' a a' })) -< (a,st)
  case m of
    Just Measured { stable = Stable, output = b } -> returnA -< b
    Just Measured { stable = Unstable, input = a' } -> f -< a'
    Nothing -> f -< a
{-# INLINE reuseByMetric_ #-}

data Measured a b n = Measured { input :: a, output :: b, stable :: Stable, measured :: n }

instance (Show a, Show b, Show n) => Show (Measured a b n) where
  show m = printf "%s@%s" (show (output m)) (show (measured m))

instance Ord n => Semigroup (Measured a b n) where
  m1 <> m2
    | measured m1 <= measured m2 = m1
    | otherwise                  = m2
  {-# INLINE (<>) #-}
