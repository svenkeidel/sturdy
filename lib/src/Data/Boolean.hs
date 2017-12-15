{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Boolean where

import qualified Prelude as P

class Logic b where
  true :: b
  false :: b
  and :: b -> b -> b
  or :: b -> b -> b
  not :: b -> b

instance Logic P.Bool where
  true = P.True
  false = P.False
  and b1 b2 = case (b1,b2) of
    (P.True,P.True) -> true
    (P.False,_) -> false
    (_,P.False) -> false
  or b1 b2 = case (b1,b2) of
    (P.True,_) -> true
    (_,P.True) -> true
    (P.False,P.False) -> false
  not P.False = true
  not P.True = false

data AbsBool = True | False | Top

instance Logic AbsBool where
  true = True
  false = False
  and b1 b2 = case (b1,b2) of
    (True,True) -> true
    (False,_) -> false
    (_,False) -> false
    (_,_) -> Top
  or b1 b2 = case (b1,b2) of
    (True,_) -> true
    (_,True) -> true
    (False,False) -> false
    (_,_) -> Top
  not b = case b of
    True -> false
    False -> true
    Top -> Top
