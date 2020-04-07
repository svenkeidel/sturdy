{-# LANGUAGE Arrows #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- | Parallel fixpoint iteration.
module Control.Arrow.Fix.Parallel(parallel,adi) where

import           Prelude hiding (map,iterate,lookup)

import           Control.Arrow hiding (loop)
import           Control.Arrow.Fix
import           Control.Arrow.Fix.Stack (ArrowStack)
import qualified Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Fix.Cache (ArrowParallelCache,ArrowCache,Widening)
import qualified Control.Arrow.Fix.Cache as Cache

import           Data.Abstract.Stable
import qualified Data.Function as Function

parallel :: forall a b c. (?cacheWidening :: Widening c, ArrowChoice c, ArrowStack a c, ArrowCache a b c, ArrowParallelCache a b c) => (FixpointCombinator c a b -> FixpointCombinator c a b) -> FixpointAlgorithm (c a b)
parallel combinator eval = iterate
  where
    iterate = proc a -> do
      b <- Function.fix (combinator update . eval) -< a
      st <- Cache.isStable -< (); case st of
        Stable   -> returnA -< b
        Unstable -> do
          (a',_) <- Cache.nextIteration -< (a,b)
          iterate -< a'

    update f = proc a -> do
      recurrentCall <- Stack.elem -< a
      if recurrentCall
        then do
          m <- Cache.lookupOldCache -< a; case m of
            Just b  -> returnA -< b
            Nothing -> do
              b <- Cache.initialize -< a
              returnA -< b
        else do
          _ <- Cache.initialize -< a
          b <- Stack.push' f -< a
          Cache.updateNewCache -< (a,b)
{-# INLINE parallel #-}

adi :: forall a b c. (?cacheWidening :: Widening c, ArrowChoice c, ArrowStack a c, ArrowCache a b c, ArrowParallelCache a b c) => (FixpointCombinator c a b -> FixpointCombinator c a b) -> FixpointAlgorithm (c a b)
adi combinator eval = iterate
  where
    iterate = proc a -> do
      b <- Function.fix (combinator update . eval) -< a
      st <- Cache.isStable -< (); case st of
        Stable   -> returnA -< b
        Unstable -> do
          (a',_) <- Cache.nextIteration -< (a,b)
          iterate -< a'

    update f = proc a -> do
      recurrentCall <- Stack.elem -< a
      if recurrentCall
        then do
          m <- Cache.lookupNewCache -< a; case m of
            Just b  -> returnA -< b
            Nothing -> Cache.initialize -< a
        else do
          Cache.initialize -< a
          b <- Stack.push' f -< a
          Cache.updateNewCache -< (a,b)
{-# INLINE adi #-}
