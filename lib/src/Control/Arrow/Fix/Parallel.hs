{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- | Parallel fixpoint iteration.
module Control.Arrow.Fix.Parallel(parallel,adi) where

import           Prelude hiding (map,iterate,lookup)

import           Control.Arrow hiding (loop)
import           Control.Arrow.Fix
import           Control.Arrow.Fix.Stack (ArrowStack)
import qualified Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Fix.Cache (ArrowParallelCache,ArrowCache)
import qualified Control.Arrow.Fix.Cache as Cache

import           Data.Abstract.Stable
import qualified Data.Function as Function

parallel :: forall a b c. (ArrowChoice c, ArrowStack a c, ArrowCache a b c, ArrowParallelCache a b c) => FixpointCombinator c a b -> FixpointAlgorithm c a b
parallel combinator eval = iterate
  where
    iterate = proc a -> do
      b <- Function.fix (combinator . update . eval) -< a
      st <- Cache.isStable -< (); case st of
        Stable -> returnA -< b
        Unstable -> do
          Cache.nextIteration -< ()
          iterate -< a

    update f = proc a -> do
      m <- Cache.lookupNewCache -< a; case m of
        Just _ -> Cache.lookupOldCache -< a
        Nothing -> do
          Cache.initialize -< a
          b <- Stack.push f -< a
          (_,b') <- Cache.updateNewCache -< (a,b)
          returnA -< b'
{-# INLINE parallel #-}

adi :: forall a b c. (ArrowChoice c, ArrowStack a c, ArrowCache a b c, ArrowParallelCache a b c) => FixpointCombinator c a b -> FixpointAlgorithm c a b
adi combinator eval = iterate
  where
    iterate = proc a -> do
      b <- Function.fix (combinator . update . eval) -< a
      st <- Cache.isStable -< (); case st of
        Stable -> returnA -< b
        Unstable -> do
          Cache.nextIteration -< ()
          iterate -< a

    update f = proc a -> do
      m <- Cache.lookupNewCache -< a; case m of
        Just b -> returnA -< b
        Nothing -> do
          Cache.initialize -< a
          b <- Stack.push f -< a
          (_,b') <- Cache.updateNewCache -< (a,b)
          returnA -< b'
{-# INLINE adi #-}
