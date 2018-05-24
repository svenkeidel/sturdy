{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Store where

import Prelude hiding (lookup,id)

import Control.Arrow

-- | Arrow-based interface to describe computations that modify a store.
class Arrow c => ArrowStore var val lab c | c -> var, c -> val where
  -- | Reads a value from the store. Fails if the binding is not in the current store.
  read :: c (var,lab) val
  -- | Writes a value to the store.
  write :: c (var,val,lab) ()
