{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Props.UseDef.DeadStores.Prop where

import Label
import Data.List
import Data.Order
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.GaloisConnection
import Data.Powerset

import Props.UseDef.Prop

-- A variable write is "dead" if at least once the variable was written
-- without a subsequent read observing that write
data DeadStores = DeadStores {
  maybeDead :: Map Text (Set Label), -- dead unless followed by a read
  dead :: Set Label -- definitely dead (e.g., because of double write)
}
type Prop = DeadStores

maybeDeadLabels :: DeadStores -> Set Label
maybeDeadLabels dw = Map.foldr Set.union Set.empty $ maybeDead dw

instance PreOrd DeadStores where
  dw1 ⊑ dw2 = (dead dw1 ⊑ dead dw2) && (maymay ⊑ maybeDeadLabels dw2)
    where maymay = Set.filter (\x -> Prelude.not $ Set.member x $ dead dw2) $ maybeDeadLabels dw1

instance Complete DeadStores where
  (DeadStores may1 must1) ⊔ (DeadStores may2 must2) = DeadStores (may1 ⊔ may2) (must1 ⊔ must2)

instance LowerBounded DeadStores where
  bottom = DeadStores Map.empty Set.empty


newtype FDeadStores = FDeadStores (Set Label)
  deriving (Show, Eq)
type FProp = FDeadStores

instance PreOrd FDeadStores where
  (FDeadStores s1) ⊑ (FDeadStores s2) = s1 ⊑ s2

instance Complete FDeadStores where
  (FDeadStores s1) ⊔ (FDeadStores s2) = FDeadStores $ s1 ⊔ s2

instance LowerBounded FDeadStores where
  bottom = FDeadStores bottom

finalizeDeadStores :: DeadStores -> FDeadStores
finalizeDeadStores dw = FDeadStores $ maybeDeadLabels dw `Set.union` dead dw


---------------
-- Galois
---------------

instance Galois (Pow Trace) FDeadStores where
  alpha = lifted (FDeadStores . findDeadStores)
    where findDeadStores [] = Set.empty
          findDeadStores (TrUse _ _ : tr) = findDeadStores tr
          findDeadStores (TrDef x l : tr) = case find ((x==) . useDefName) tr of
            -- never read => dead
            Nothing -> Set.insert l $ findDeadStores tr
            -- overwritten => dead
            Just (TrDef _ _) -> Set.insert l $ findDeadStores tr
            -- otherwise: not dead
            Just (TrUse _ _) -> findDeadStores tr

  gamma = undefined
