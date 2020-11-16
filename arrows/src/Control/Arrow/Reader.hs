{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Reader where

import Control.Arrow
import Data.Profunctor

-- | Arrow type class that gives access to a read-only value.
class (Arrow c, Profunctor c) => ArrowReader r c | c -> r where
  -- | Retrieves the current value.
  ask :: c () r
  -- | Runs a computation with a new value.
  local :: c x y -> c (r,x) y
