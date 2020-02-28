{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
module Control.Arrow.Fix.Iterate where

import Control.Arrow hiding (loop)
import Control.Arrow.Trans

import Data.Profunctor
import Data.Abstract.Stable

class (Arrow c, Profunctor c) => ArrowIterate c where
  isStable :: c () Stable
  nextIteration :: c () ()

  default isStable :: (c ~ t c', ArrowLift t, ArrowIterate c') => c () Stable
  default nextIteration :: (c ~ t c', ArrowLift t, ArrowIterate c') => c () ()

  isStable = lift' isStable
  nextIteration = lift' nextIteration

  {-# INLINE isStable #-}
  {-# INLINE nextIteration #-}
