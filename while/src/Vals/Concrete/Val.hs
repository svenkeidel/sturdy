{-# LANGUAGE DeriveGeneric #-}
module Vals.Concrete.Val where

import Data.Order
import Data.Hashable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Powerset
import Data.Text (Text)

import GHC.Generics (Generic)

data Val = BoolVal Bool | NumVal Double
  deriving (Eq, Show, Generic)

instance Hashable Val

instance PreOrd Val where
  (⊑) = (==)
  (≈) = (==)

type Store = Map Text Val
initStore :: Store
initStore = Map.empty

type LiftedStore = Map Text (Pow Val)
liftStore :: Store -> LiftedStore
liftStore = Map.map singleton
