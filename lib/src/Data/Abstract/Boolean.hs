{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Abstract.Boolean where

import Prelude hiding (Bool(..))
import qualified Prelude as P

import Data.Abstract.Widening
import Data.Boolean
import Data.Hashable
import Data.Order

import GHC.Generics

data Bool = True | False | Top deriving (Eq,Generic)

instance Show Bool where
  show True = "True"
  show False = "False"
  show Top = "⊤"

instance Logic Bool where
  true = True
  false = False
  and b1 b2 = case (b1,b2) of
    (True,True) -> True
    (False,_) -> False
    (_,False) -> False
    (_,_) -> Top
  or b1 b2 = case (b1,b2) of
    (True,_) -> True
    (_,True) -> True
    (False,False) -> False
    (_,_) -> Top
  not b = case b of
    True -> False
    False -> True
    Top -> Top

instance PreOrd Bool where
  _ ⊑ Top = P.True
  True ⊑ True = P.True
  False ⊑ False = P.True
  _ ⊑ _ = P.False

instance Complete Bool where
  Top ⊔ _ = Top
  _ ⊔ Top = Top
  True ⊔ True = True
  False ⊔ False = False
  _ ⊔ _ = Top

instance UpperBounded Bool where
  top = Top

instance Hashable Bool

widening :: Widening Bool
widening True True = (Stable,True)
widening False False = (Stable,False)
widening Top Top = (Stable,Top)
widening _ _ = (Instable,Top)
