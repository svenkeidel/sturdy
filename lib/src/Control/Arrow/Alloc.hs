{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Alloc where

import Control.Arrow
import Data.Profunctor

-- | Arrow-based interface for allocating addresses.
class (Arrow c, Profunctor c) => ArrowAlloc x y c where
  -- | Allocates a new address.
  alloc :: c x y
