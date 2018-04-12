{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Boolean where

class Logic b where
  true :: b
  false :: b
  and :: b -> b -> b
  or :: b -> b -> b
  not :: b -> b

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
