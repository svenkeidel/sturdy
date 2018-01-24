module Props.LiveVariables.Prop where

import Label
import Data.Maybe
import Data.Order
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Utils

-- A variable is "live" at a label if there exists a path to a use of the variable.
data LiveVars = LiveVars {
  maybeLive :: Map Text (Set Label), -- only live if followed by a use
  live :: Map Text (Set Label)
}

instance PreOrd LiveVars where
  lv1 ⊑ lv2 = (live lv1 ⊑ live lv2) && (maymay ⊑ maybeLive lv2)
    where maymay = Map.mapWithKey (\v labs -> labs Set.\\ (lookupM v $ live lv2)) $ maybeLive lv1

instance Complete LiveVars where
  (LiveVars may1 must1) ⊔ (LiveVars may2 must2) = LiveVars (may1 ⊔ may2) (must1 ⊔ must2)

initLiveVars :: LiveVars
initLiveVars = LiveVars Map.empty Map.empty

type FLiveVars = Map Label (Set Text)

finalizeLiveVars :: LiveVars -> FLiveVars
finalizeLiveVars lv = Map.foldrWithKey insert Map.empty $ live lv
  where insert :: Text -> Set Label -> FLiveVars -> FLiveVars
        insert t labs m = Set.foldr (\l m -> Map.insertWith (Set.union) l (Set.singleton t) m) m labs

---------------
-- concrete
---------------

type CProp = LiveVars

initCProp :: CProp
initCProp = initLiveVars

type FCProp = FLiveVars

finalizeCProp :: CProp -> FCProp
finalizeCProp = finalizeLiveVars

liftCProp :: FCProp -> FCProp
liftCProp = id

---------------
-- abstract
---------------

type AProp = LiveVars

initAProp :: AProp
initAProp = initLiveVars

type FAProp = FLiveVars

finalizeAProp :: AProp -> FAProp
finalizeAProp = finalizeLiveVars


---------------
-- Galois
---------------

-- derived
