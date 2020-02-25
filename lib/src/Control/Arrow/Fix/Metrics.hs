{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Fix.Metrics where

import Control.Arrow
import Control.Arrow.Trans
import Data.Profunctor

class (Arrow c, Profunctor c) => ArrowFiltered a c | c -> a where
  filtered :: c a ()
  default filtered :: (c ~ t c', ArrowLift t, ArrowFiltered a c') => c a ()
  filtered = lift' filtered
