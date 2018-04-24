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

import Props.UseDef.Prop


newtype ReachingDefs = ReachingDefs {
  -- For each variable, set of assignments that can reach the current execution point
  defs :: Map Text (Set Label)
} deriving (Eq,Show)

instance PreOrd ReachingDefs where
  rd1 ⊑ rd2 = defs rd1 ⊑ defs rd2
  (≈) = (==)

instance Complete ReachingDefs where
  rd1 ⊔ rd2 = ReachingDefs $ defs rd1 ⊔ defs rd2

instance LowerBounded ReachingDefs where
  bottom = ReachingDefs Map.empty

type Prop = ReachingDefs

---------------
-- Galois
---------------

instance Galois (Pow Trace) ReachingDefs where
  alpha = lifted lift
    where lift trace = ReachingDefs $ foldl extenddefs bottom trace
          extenddefs ds (TrUse _ _) = ds
          extenddefs ds (TrDef x l) = Map.insert x (Set.singleton l) ds

  gamma = undefined