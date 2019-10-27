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

import Data.Order
import Data.Metric
import Data.Profunctor
import Data.Monoid (First(..))

import Text.Printf

class (Arrow c, Profunctor c) => ArrowReuse a b c where

  -- | Reuse cached results at the cost of precision.
  reuse :: (Monoid m) => Stable -> (a -> a -> Stable -> b -> m -> m) -> c a m

reuseFirst :: (PreOrd a, ArrowChoice c, ArrowReuse a b c) => Stable -> IterationStrategy c a b
reuseFirst s f = proc a -> do
  m <- reuse s (\a a' s' b' m -> case m of
                 First (Just _) -> m
                 First Nothing
                   | a ⊑ a' -> First (Just (a',b',s'))
                   | otherwise -> m) -< a
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

reuseByMetric :: (PreOrd a, Ord n, ArrowChoice c, ArrowReuse a b c) => Metric a n -> IterationStrategy c a b
reuseByMetric metric = reuseByMetric_ (\s a a' -> Product s (metric a a')) Unstable
{-# INLINE reuseByMetric #-}

reuseStableByMetric :: (PreOrd a, Ord n, ArrowChoice c, ArrowReuse a b c) => Metric a n -> IterationStrategy c a b
reuseStableByMetric metric = reuseByMetric_ (const metric) Stable
{-# INLINE reuseStableByMetric #-}

reuseByMetric_ :: (PreOrd a, Ord n, ArrowChoice c, ArrowReuse a b c) => (Stable -> Metric a n) -> Stable -> IterationStrategy c a b
reuseByMetric_ metric s f = proc a -> do
  m <- reuse s (\a a' s' b' m ->
                if a ⊑ a'
                then m <> Just (Measured { input = a', output = b', stable = s', measured = metric s' a a' })
                else m) -< a
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
