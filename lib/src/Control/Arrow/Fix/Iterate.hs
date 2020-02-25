{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
module Control.Arrow.Fix.Iterate where

import Control.Arrow hiding (loop)
import Control.Arrow.Trans

import Data.Profunctor
import Data.Abstract.Stable

class (Arrow c, Profunctor c) => ArrowIterate a c | c -> a where
  isStable :: c a Stable
  nextIteration :: c a ()

  default isStable :: (c ~ t c', ArrowLift t, ArrowIterate a c') => c a Stable
  default nextIteration :: (c ~ t c', ArrowLift t, ArrowIterate a c') => c a ()

  isStable = lift' isStable
  nextIteration = lift' nextIteration

  {-# INLINE isStable #-}
  {-# INLINE nextIteration #-}
