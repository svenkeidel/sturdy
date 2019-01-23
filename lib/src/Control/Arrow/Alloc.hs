{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Alloc where

import Control.Arrow

-- | Arrow-based interface for allocating addresses.
class Arrow c => ArrowAlloc x y c where
  -- | Allocates a new address.
  alloc :: c x y
