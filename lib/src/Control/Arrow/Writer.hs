{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Writer where

import Control.Arrow
import Data.Profunctor

class (Arrow c, Profunctor c) => ArrowWriter w c | c -> w where
  tell :: c w ()
