{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Store where

import Prelude hiding (lookup,id)

import Control.Arrow

class Arrow c => ArrowStore var val c | c -> var, c -> val where
  read :: c var val
  write :: c (var,val) ()
