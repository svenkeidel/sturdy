{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Fix.Metrics where

import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.Transformer.Reader
import Control.Arrow.Transformer.State
import Data.Profunctor

class (Arrow c, Profunctor c) => ArrowMetrics a c | c -> a where
  filtered :: c a ()
  evaluated :: c a ()
  iterated :: c a ()

  default filtered :: (c ~ t c', ArrowTrans t, ArrowMetrics a c') => c a ()
  default evaluated :: (c ~ t c', ArrowTrans t, ArrowMetrics a c') => c a ()
  default iterated :: (c ~ t c', ArrowTrans t, ArrowMetrics a c') => c a ()

  filtered = lift' filtered
  evaluated = lift' evaluated
  iterated = lift' iterated

  {-# INLINE filtered #-}
  {-# INLINE evaluated #-}
  {-# INLINE iterated #-}

------------- Instances --------------
instance ArrowMetrics a c => ArrowMetrics a (ReaderT r c)
instance ArrowMetrics a c => ArrowMetrics a (StateT s c)
