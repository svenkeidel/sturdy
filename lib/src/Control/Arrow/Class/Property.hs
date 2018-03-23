{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Class.Property where

import Control.Arrow

class Arrow c => HasProp p c where
  modifyProp :: (c (x,p) p) -> c x ()
