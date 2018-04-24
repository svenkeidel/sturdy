{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Store where

import Prelude hiding (lookup,id)

import Control.Arrow

class Arrow c => ArrowStore var val lab c | c -> var, c -> val where
  read :: c (var,lab) val
  write :: c (var,val,lab) ()
