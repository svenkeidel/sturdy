{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Fix.Stack where

import Control.Arrow

import Data.Profunctor
import Data.HashSet

class (Arrow c, Profunctor c) => ArrowStack a c | c -> a where
  peek :: c () (Maybe a)
  size :: c () Int
  push :: c a b -> c a b
  elem :: c a Bool
  elems :: c () (HashSet a)
