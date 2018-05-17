{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.MaybeStore where

import Prelude hiding (lookup,id)

import Control.Arrow

class Arrow c => ArrowMaybeStore var val c | c -> var, c -> val where
  read :: c (var) (Maybe val)
  write :: c (var,val) ()
