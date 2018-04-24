module Data.Abstract.Equality where

import Prelude hiding (Bool(..))
import Data.Abstract.Boolean

class Equality a where
  (==) :: a -> a -> Bool

instance Equality Bool where
  Top == _ = Top
  _ == Top = Top
  True == True = True
  False == False = True
  True == False = False
  False == True = False
