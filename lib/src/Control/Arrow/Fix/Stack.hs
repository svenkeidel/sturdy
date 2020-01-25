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
  elems :: c () [a]
  peek :: c () (Maybe a)
  size :: c () Int

  default elems :: (c ~ t c', ArrowLift t, ArrowStack a c') => c () [a]
  default peek :: (c ~ t c', ArrowLift t, ArrowStack a c') => c () (Maybe a)
  default size :: (c ~ t c', ArrowLift t, ArrowStack a c') => c () Int

  elems = lift' elems
  peek = lift' peek
  size = lift' size

  {-# INLINE elems #-}
  {-# INLINE peek #-}
  {-# INLINE size #-}

maxSize :: (ArrowChoice c, ArrowStack a c) => Int -> FixpointCombinator c a b -> FixpointCombinator c a b
maxSize limit strat f = proc a -> do
  n <- size -< ()
  if n < limit
  then f -< a
  else strat f -< a
{-# INLINE maxSize #-}

widenInput :: (Complete a, ArrowStack a c) => Widening a -> FixpointCombinator c a b
widenInput widen f = proc a -> do
  m <- peek -< ()
  f -< case m of
    Nothing -> a
    Just x  -> snd $ x `widen` (x ⊔ a)
{-# INLINE widenInput #-}

reuse :: (ArrowChoice c, ArrowStack a c) => (a -> [a] -> Maybe a) -> FixpointCombinator c a b
reuse select f = proc a -> do
  xs <- elems -< ()
  f -< fromMaybe a (select a xs)
{-# INLINE reuse #-}

reuseFirst :: (PreOrd a, ArrowChoice c, ArrowStack a c) => FixpointCombinator c a b
reuseFirst = reuse find
  where
    find a (x:xs)
      | a ⊑ x     = Just x
      | otherwise = find a xs
    find _ []     = Nothing
{-# INLINE reuseFirst #-}

reuseByMetric :: (PreOrd a, Ord n, ArrowChoice c, ArrowStack a c) => Metric a n -> FixpointCombinator c a b
reuseByMetric metric = reuse find
  where
    find a xs = elem <$> foldMap (\a' -> if a ⊑ a' then Just (Measured a' (metric a a')) else Nothing) xs
{-# INLINE reuseByMetric #-}

data Measured a n = Measured { elem :: a, measured :: n }

instance (Show a, Show n) => Show (Measured a n) where
  show m = printf "%s@%s" (show (elem m)) (show (measured m))

instance Ord n => Semigroup (Measured a n) where
  m1 <> m2
    | measured m1 <= measured m2 = m1
    | otherwise                  = m2
  {-# INLINE (<>) #-}
