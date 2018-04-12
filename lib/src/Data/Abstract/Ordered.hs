module Data.Abstract.Ordered where

import Data.Abstract.Boolean

class Ordered a where
  (<) :: a -> a -> Bool
