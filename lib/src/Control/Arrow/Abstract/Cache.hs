{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Abstract.Cache where

import Control.Arrow

import Data.Abstract.Widening (Stable)
import Data.Profunctor 

class (Profunctor c, Arrow c) => ArrowCache a b c where
  lookup :: c a b
  update :: c (a,b) (Stable,b)
  cached :: c a b -> c a b
