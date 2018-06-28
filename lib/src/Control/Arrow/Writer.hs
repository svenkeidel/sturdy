{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Writer where

import Control.Arrow

class Arrow c => ArrowWriter w c | c -> w where
  tell :: c w ()
