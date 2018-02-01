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

---------------
-- Prop
---------------

data ReachingDefs = ReachingDefs {
  -- For each variable, set of assignments that can reach the current execution point
  env :: Map Text (Set Label)
} deriving (Eq,Show)

instance PreOrd ReachingDefs where
  rd1 ⊑ rd2 = env rd1 ⊑ env rd2
  (≈) = (==)

instance Complete ReachingDefs where
  rd1 ⊔ rd2 = ReachingDefs $ env rd1 ⊔ env rd2

instance LowerBounded ReachingDefs where
  bottom = ReachingDefs Map.empty

initReachingDefs :: ReachingDefs
initReachingDefs = bottom


type Prop = ReachingDefs

initProp :: Prop
initProp = initReachingDefs

---------------
-- Galois
---------------

instance Galois (Pow Trace) ReachingDefs where
  alpha = lifted lift
    where lift trace = ReachingDefs $ foldl extendEnv bottom trace
          extendEnv env (TrUse _ _) = env
          extendEnv env (TrDef x l) = Map.insert x (Set.singleton l) env

  gamma = undefined