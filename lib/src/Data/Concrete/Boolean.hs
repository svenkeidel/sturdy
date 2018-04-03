{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Concrete.Boolean(Prelude.Bool(..)) where

import Prelude
import Data.Boolean

instance Logic Bool where
  true = True
  false = False
  and b1 b2 = case (b1,b2) of
    (True,True) -> true
    (False,_) -> false
    (_,False) -> false
  or b1 b2 = case (b1,b2) of
    (True,_) -> true
    (_,True) -> true
    (False,False) -> false
  not False = true
  not True = false
