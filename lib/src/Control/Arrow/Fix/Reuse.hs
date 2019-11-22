{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Fix.Reuse ( ArrowReuse(..))
where

import Prelude hiding (lookup)
import Control.Arrow

import Data.Abstract.Stable

import Data.Profunctor

class (Arrow c, Profunctor c) => ArrowReuse a b c where
  -- | Reuse cached results at the cost of precision.
  reuse :: (Monoid m) => Stable -> (a -> a -> Stable -> b -> m -> m) -> c a m
