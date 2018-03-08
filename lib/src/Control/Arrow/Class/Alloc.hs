{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Class.Alloc where

import Control.Arrow
import Data.Store (Store)
import Data.Environment(Env)

class Arrow c => ArrowAlloc var addr val c where
  alloc :: c (var,Env var addr,Store addr val) addr
