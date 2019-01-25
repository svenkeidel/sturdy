{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Data.Abstract.There where

import Data.Order
import Data.Hashable

import Control.DeepSeq
import GHC.Generics (Generic)

-- | Datatype that indicates if a value in the map must be there or may not be there.
data There = Must | May deriving (Eq, Generic, NFData)

instance Show There where
  show Must = ""
  show May = "?"

instance PreOrd There where
  Must ⊑ May = True
  Must ⊑ Must = True
  May ⊑ May = True
  _ ⊑ _ = False

instance Complete There where
  Must ⊔ Must = Must
  _ ⊔ _ = May

instance Hashable There where
  hashWithSalt s Must = s `hashWithSalt` (1::Int)
  hashWithSalt s May = s `hashWithSalt` (2::Int)

may :: a -> (There,a)
may a = (May,a)

must :: a -> (There,a)
must a = (Must,a)
