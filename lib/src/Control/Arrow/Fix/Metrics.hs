{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Fix.Metrics where

import Control.Arrow
import Control.Arrow.Trans
import Data.Profunctor

class (Arrow c, Profunctor c) => ArrowMetrics a c | c -> a where
  filtered :: c a ()
  evaluated :: c a ()
  iterated :: c a ()

  default filtered :: (c ~ t c', ArrowLift t, ArrowMetrics a c') => c a ()
  default evaluated :: (c ~ t c', ArrowLift t, ArrowMetrics a c') => c a ()
  default iterated :: (c ~ t c', ArrowLift t, ArrowMetrics a c') => c a ()

  filtered = lift' filtered
  evaluated = lift' evaluated
  iterated = lift' iterated

  {-# INLINE filtered #-}
  {-# INLINE evaluated #-}
  {-# INLINE iterated #-}