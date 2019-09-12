{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Abstract.Boolean where

import Prelude hiding (Bool(..))
import qualified Prelude as P

import Data.Boolean
import Data.Hashable
import Data.Order

import Data.Abstract.Stable
import Data.Abstract.Widening

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
  {-# INLINE true #-}
  {-# INLINE false #-}
  {-# INLINE and #-}
  {-# INLINE or #-}
  {-# INLINE not #-}

instance PreOrd Bool where
  _ ⊑ Top = P.True
  True ⊑ True = P.True
  False ⊑ False = P.True
  _ ⊑ _ = P.False
  {-# INLINE (⊑) #-}

instance Complete Bool where
  Top ⊔ _ = Top
  _ ⊔ Top = Top
  True ⊔ True = True
  False ⊔ False = False
  _ ⊔ _ = Top
  {-# INLINE (⊔) #-}

instance UpperBounded Bool where
  top = Top
  {-# INLINE top #-}

instance Hashable Bool where
  hashWithSalt s True = s `hashWithSalt` (1::Int)
  hashWithSalt s False = s `hashWithSalt` (2::Int)
  hashWithSalt s Top = s `hashWithSalt` (3::Int)
  {-# INLINE hashWithSalt #-}

widening :: Widening Bool
widening True True = (Stable,True)
widening False False = (Stable,False)
widening Top Top = (Stable,Top)
widening _ _ = (Unstable,Top)
{-# INLINE widening #-}
