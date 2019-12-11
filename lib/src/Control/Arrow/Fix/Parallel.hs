{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
module Control.Arrow.Fix.Parallel where

import Prelude hiding (iterate)

import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.Fix
import Control.Arrow.Fix.Stack as Stack
import Control.Arrow.Fix.Cache (ArrowCache)
import qualified Control.Arrow.Fix.Cache as Cache

import Data.Profunctor
import Data.Abstract.Stable

class (Arrow c, Profunctor c) => ArrowParallel c where
  nextIteration :: c () ()

  default nextIteration :: (c ~ t c', ArrowLift t, ArrowParallel c') => c () ()
  nextIteration = lift' nextIteration
  {-# INLINE nextIteration #-}


parallel :: forall a b c. (ArrowParallel c, ArrowStack a c, ArrowCache a b c, ArrowChoice c) => FixpointCombinator c a b
parallel f = proc a -> do
  n <- Stack.size -< ()
  if n == 0
  then iterate -< a
  else update -< a

  where
    iterate = proc a -> do
      Cache.initialize -< a
      b <- Stack.push f -< a
      (s,b') <- Cache.update -< (a,b)
      case s of
        Stable -> returnA -< b'
        Unstable -> do
          nextIteration -< ()
          iterate -< a

    update = proc a -> do
      m <- Cache.lookup -< a
      case m of
        Just (_,b) -> returnA -< b
        Nothing -> do
          Cache.initialize -< a
          b <- Stack.push f -< a
          (_,b') <- Cache.update -< (a,b)
          returnA -< b'
{-# INLINE parallel #-}
