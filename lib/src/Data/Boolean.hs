{-# LANGUAGE FlexibleInstances #-}
module Data.Boolean where

class Logic b where
  true :: b
  false :: b
  and :: b -> b -> b
  or :: b -> b -> b
  implies :: b -> b -> b
  not :: b -> b

instance Logic Bool where
  true = True
  false = False
  and b1 b2 = case (b1,b2) of
    (True,True) -> True
    (False,_) -> False
    (_,False) -> False
  or b1 b2 = case (b1,b2) of
    (True,_) -> True
    (_,True) -> True
    (False,False) -> False
  not False = True
  not True = False
  implies True False = False
  implies _ _ = True
