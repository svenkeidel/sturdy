{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Class.Config where

import Control.Arrow

class Arrow c => ArrowConfig cIn cOut c | c -> cIn, c -> cOut where
  getInConfig :: c () cIn
  getOutConfig :: c () cOut
  setOutConfig :: c cOut ()

instance ArrowConfig () () (->) where
  getInConfig = id
  getOutConfig = id
  setOutConfig = id
