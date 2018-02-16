{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Boolean where

import qualified Prelude as P
import           Data.Order
import           Data.Widening

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

data AbsBool = Bot | True | False | Top deriving (P.Eq)

instance P.Show AbsBool where
  show Bot = "⊥"
  show True = "True"
  show False = "False"
  show Top = "⊤"

instance Logic AbsBool where
  true = True
  false = False
  and b1 b2 = case (b1,b2) of
    (Bot,_) -> Bot
    (_,Bot) -> Bot
    (True,True) -> true
    (False,_) -> false
    (_,False) -> false
    (_,_) -> Top
  or b1 b2 = case (b1,b2) of
    (Bot,_) -> Bot
    (_,Bot) -> Bot
    (True,_) -> true
    (_,True) -> true
    (False,False) -> false
    (_,_) -> Top
  not b = case b of
    Bot -> Bot
    True -> false
    False -> true
    Top -> Top

instance PreOrd AbsBool where
  Bot ⊑ _ = P.True
  _ ⊑ Top = P.True
  True ⊑ True = P.True
  False ⊑ False = P.True
  _ ⊑ _ = P.False

instance Complete AbsBool where
  Bot ⊔ b = b
  b ⊔ Bot = b
  Top ⊔ _ = Top
  _ ⊔ Top = Top
  True ⊔ True = True
  False ⊔ False = False
  _ ⊔ _ = Top

instance LowerBounded AbsBool where
  bottom = Bot

instance UpperBounded AbsBool where
  top = Top

instance Widening AbsBool where
