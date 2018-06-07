{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Alloc where

import Control.Arrow

class Arrow c => ArrowAlloc x y c where
  alloc :: c x y
