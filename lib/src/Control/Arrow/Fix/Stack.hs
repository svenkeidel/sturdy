{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Fix.Stack where

import Prelude hiding (elem)
import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Trans

import Data.Profunctor
import Data.Order
import Data.Metric
import Data.Abstract.Widening
import Data.Maybe

import Text.Printf

class (Arrow c, Profunctor c) => ArrowStack a c | c -> a where
  push :: c a b -> c a b
  elem :: c a Bool

  default elem :: (c ~ t c', ArrowLift t, ArrowStack a c') => c a Bool
  elem = lift' elem
  {-# INLINE elem #-}

class (Arrow c, Profunctor c) => ArrowStackDepth c where
  depth :: c () Int
  default depth :: (c ~ t c', ArrowLift t, ArrowStackDepth c') => c () Int
  depth = lift' depth
  {-# INLINE depth #-}


class (Arrow c, Profunctor c) => ArrowStackElements a c where
  elems :: c () [a]
  peek :: c () (Maybe a)

  default elems :: (c ~ t c', ArrowLift t, ArrowStackElements a c') => c () [a]
  default peek :: (c ~ t c', ArrowLift t, ArrowStackElements a c') => c () (Maybe a)

  elems = lift' elems
  peek = lift' peek

  {-# INLINE elems #-}
  {-# INLINE peek #-}

class (Arrow c, Profunctor c) => ArrowTopLevel c where
  topLevel :: FixpointCombinator c a b -> FixpointCombinator c a b

  default topLevel :: (Underlying c a b ~ c' a' b', ArrowTrans c, ArrowTopLevel c') => FixpointCombinator c a b -> FixpointCombinator c a b
  topLevel strat f = lift $ topLevel (unlift . strat . lift) (unlift f)

maxDepth :: (ArrowChoice c, ArrowStackDepth c) => Int -> FixpointCombinator c a b -> FixpointCombinator c a b
maxDepth limit strat f = proc a -> do
  n <- depth -< ()
  if n < limit
  then f -< a
  else strat f -< a
{-# INLINABLE maxDepth #-}

widenInput :: (Complete a, ArrowStackElements a c) => Widening a -> FixpointCombinator c a b
widenInput widen f = proc a -> do
  m <- peek -< ()
  f -< case m of
    Nothing -> a
    Just x  -> snd $ x `widen` (x ⊔ a)
{-# INLINE widenInput #-}

reuse :: (ArrowChoice c, ArrowStackElements a c) => (a -> [a] -> Maybe a) -> FixpointCombinator c a b
reuse select f = proc a -> do
  xs <- elems -< ()
  f -< fromMaybe a (select a xs)
{-# INLINE reuse #-}

reuseFirst :: (PreOrd a, ArrowChoice c, ArrowStackElements a c) => FixpointCombinator c a b
reuseFirst = reuse find
  where
    find a (x:xs)
      | a ⊑ x     = Just x
      | otherwise = find a xs
    find _ []     = Nothing
{-# INLINE reuseFirst #-}

reuseByMetric :: (PreOrd a, Ord n, ArrowChoice c, ArrowStackElements a c) => Metric a n -> FixpointCombinator c a b
reuseByMetric metric = reuse find
  where
    find a xs = element <$> foldMap (\a' -> if a ⊑ a' then Just (Measured a' (metric a a')) else Nothing) xs
{-# INLINE reuseByMetric #-}

data Measured a n = Measured { element :: a, measured :: n }

instance (Show a, Show n) => Show (Measured a n) where
  show m = printf "%s@%s" (show (element m)) (show (measured m))

instance Ord n => Semigroup (Measured a n) where
  m1 <> m2
    | measured m1 <= measured m2 = m1
    | otherwise                  = m2
  {-# INLINE (<>) #-}
