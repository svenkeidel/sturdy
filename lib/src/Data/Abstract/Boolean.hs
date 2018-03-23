module Data.Abstract.Boolean where

import Prelude hiding (Bool(..))
import qualified Prelude as P

import Data.Boolean
import Data.Order
import Data.Abstract.Widening

data Bool = Bot | True | False | Top deriving (Eq)

instance Show Bool where
  show Bot = "⊥"
  show True = "True"
  show False = "False"
  show Top = "⊤"

instance Logic Bool where
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

instance PreOrd Bool where
  Bot ⊑ _ = P.True
  _ ⊑ Top = P.True
  True ⊑ True = P.True
  False ⊑ False = P.True
  _ ⊑ _ = P.False

instance Complete Bool where
  Bot ⊔ b = b
  b ⊔ Bot = b
  Top ⊔ _ = Top
  _ ⊔ Top = Top
  True ⊔ True = True
  False ⊔ False = False
  _ ⊔ _ = Top

instance LowerBounded Bool where
  bottom = Bot

instance UpperBounded Bool where
  top = Top

instance Widening Bool where
