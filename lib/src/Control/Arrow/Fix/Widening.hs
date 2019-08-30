{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Arrow.Fix.Widening where

import Control.Arrow
import Data.Profunctor
import Data.Order
import Data.Abstract.Widening (Stable,finite)

class (Arrow c, Profunctor c) => ArrowWidening a c where
  widening :: c (a,a) (Stable,a)

instance Complete a => ArrowWidening a (->) where
  widening (a,a') = finite a a'
