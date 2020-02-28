{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- | Parallel fixpoint iteration.
module Control.Arrow.Fix.Parallel(parallel,parallelADI) where

import           Prelude hiding (iterate)

import           Control.Arrow hiding (loop)
import           Control.Arrow.Fix
import           Control.Arrow.Fix.Iterate
import           Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Fix.Cache (ArrowCache)
import qualified Control.Arrow.Fix.Cache as Cache

import           Data.Abstract.Stable

iterate :: forall a b c. (ArrowIterate c, ArrowChoice c) => FixpointCombinator c a b
iterate f = proc a -> do
  b <- f -< a
  s <- isStable -< ()
  case s of
    Stable -> returnA -< b
    Unstable -> do
      nextIteration -< ()
      iterate f -< a

parallel :: forall a b c. (ArrowChoice c, ArrowTopLevel c, ArrowIterate c, ArrowStack a c, ArrowCache a b c) => FixpointCombinator c a b
parallel = topLevel iterate . update
  where
    update f = proc a -> do
      loop <- Stack.elem -< a
      if loop
      then do
        m <- Cache.lookup -< a
        case m of
          Just (_,b) -> returnA -< b
          Nothing -> do
            Cache.initialize -< a
            b <- Stack.push f -< a
            (_,b') <- Cache.update -< (a,b)
            returnA -< b'
      else
        Stack.push f -< a
{-# INLINABLE parallel #-}

-- Implements the fixpoint algorithm in the ADI paper.
parallelADI :: forall a b c. (ArrowChoice c, ArrowTopLevel c, ArrowStack a c, ArrowIterate c, ArrowCache a b c) => FixpointCombinator c a b
parallelADI = topLevel iterate . update
  where
    update f = proc a -> do
      m <- Cache.lookup -< a
      case m of
        Just (_,b) -> returnA -< b
        Nothing -> do
          Cache.initialize -< a
          b <- Stack.push f -< a
          (_,b') <- Cache.update -< (a,b)
          returnA -< b'
{-# INLINE parallelADI #-}
