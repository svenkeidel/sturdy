{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Fix.Parallel where

import Prelude hiding (iterate)

import Control.Arrow hiding (loop)
import Control.Arrow.Fix
import Control.Arrow.Fix.Iterate
import Control.Arrow.Fix.Stack as Stack
import Control.Arrow.Fix.Cache (ArrowCache)
import qualified Control.Arrow.Fix.Cache as Cache

import Data.Abstract.Stable

parallel :: forall a b c. (ArrowIterate a c, ArrowStack a c, ArrowStackDepth c, ArrowCache a b c, ArrowChoice c) => FixpointCombinator c a b
parallel f = proc a -> do
  n <- Stack.depth -< ()
  if n == 0
  then iterate -< a
  else update -< a

  where
    iterate = proc a -> do
      b <- update -< a
      s <- isStable -< a
      case s of
        Stable -> returnA -< b
        Unstable -> do
          nextIteration -< a
          iterate -< a

    update = proc a -> do
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
{-# INLINE parallel #-}
