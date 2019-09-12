{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Reader where

import Control.Arrow
import Data.Profunctor

-- | Arrow-based interface for read-only values.
class (Arrow c, Profunctor c) => ArrowReader r c | c -> r where
  -- | Retrieves the current read-only value.
  ask :: c () r
  -- | Runs a computation with a new value.
  local :: c x y -> c (r,x) y
