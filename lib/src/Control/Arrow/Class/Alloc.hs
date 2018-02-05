{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Class.Alloc where

import Control.Arrow

class Arrow c => ArrowAlloc x addr c | c -> x, c -> addr where
  alloc :: c x addr

