{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Abstract.Alloc where

import Control.Arrow
import Data.Abstract.Store (Store)
import Data.Abstract.Environment(Env)

class Arrow c => ArrowAlloc var addr val c where
  alloc :: c (var,Env var addr,Store addr val) addr
