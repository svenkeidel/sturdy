{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Fix.Widening where

import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.Transformer.State

import Data.Profunctor
import Data.Order
import Data.Abstract.Stable
import Data.Abstract.Widening (finite)

class (Arrow c, Profunctor c) => ArrowWidening a c where
  widening :: c (a,a) (Stable,a)

instance Complete a => ArrowWidening a (->) where
  widening (a,a') = finite a a'

------------- Instances --------------
instance ArrowWidening y c => ArrowWidening y (StateT s c) where
  widening = lift' widening
  {-# INLINE widening #-}
