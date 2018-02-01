{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Props.UseDef.ReachingDefinitions.Prop where

import Label
import Data.Order
import Data.GaloisConnection
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Powerset

import qualified Props.UseDef.Prop as Concrete

---------------
-- Prop: Environment and use-def/def-use relations
---------------

-- Set of labeled assignments that reach the current execution point
data ReachingDefs = ReachingDefs {
  env :: Map Text (Set Label),
  useDef :: Map Label (Set Label),
  defUse :: Map Label (Set Label)
} deriving (Eq,Show)

instance PreOrd ReachingDefs where
  (⊑) = undefined
  (≈) = (==)

instance Complete ReachingDefs where
  (⊔) = undefined

initReachingDefs :: ReachingDefs
initReachingDefs = ReachingDefs Map.empty Map.empty Map.empty


type Prop = ReachingDefs

initProp :: Prop
initProp = initReachingDefs

---------------
-- Galois
---------------

instance Galois (Pow Concrete.UseDef) ReachingDefs where
  alpha = undefined
  gamma = undefined