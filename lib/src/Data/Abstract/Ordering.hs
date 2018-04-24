module Data.Abstract.Ordering where

import Prelude hiding (Bool)

import Data.Abstract.Boolean

class Ordering a where
  (<) :: a -> a -> Bool
