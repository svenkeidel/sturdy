module Props.DeadWrites.Prop where

import Label
import Data.Order
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


-- A variable write is "dead" if at least once the variable was written
-- without a subsequent read observing that write
data DeadWrites = DeadWrites {
  written :: Map Text (Set Label), -- dead unless followed by a read
  overwritten :: Set Label -- definitely dead (e.g., because of double write)
}

writtenLabels :: DeadWrites -> Set Label
writtenLabels dw = Map.foldr Set.union Set.empty $ written dw

instance PreOrd DeadWrites where
  dw1 ⊑ dw2 = (overwritten dw1 ⊑ overwritten dw2) && (maymay ⊑ writtenLabels dw2)
    where maymay = Set.filter (\x -> Prelude.not $ Set.member x $ overwritten dw2) $ writtenLabels dw1

instance Complete DeadWrites where
  (DeadWrites may1 must1) ⊔ (DeadWrites may2 must2) = DeadWrites (may1 ⊔ may2) (must1 ⊔ must2)

initDeadWrites :: DeadWrites
initDeadWrites = DeadWrites Map.empty Set.empty

type FDeadWrites = Set Label

finalizeDeadWrites :: DeadWrites -> FDeadWrites
finalizeDeadWrites dw = writtenLabels dw `Set.union` overwritten dw

---------------
-- concrete
---------------

type CProp = DeadWrites

initCProp :: CProp
initCProp = initDeadWrites

type FCProp = FDeadWrites

finalizeCProp :: CProp -> FCProp
finalizeCProp = finalizeDeadWrites

liftCProp :: FCProp -> FCProp
liftCProp = id

---------------
-- abstract
---------------

type AProp = DeadWrites

initAProp :: AProp
initAProp = initDeadWrites

type FAProp = FDeadWrites

finalizeAProp :: AProp -> FAProp
finalizeAProp = finalizeDeadWrites


---------------
-- Galois
---------------

-- derived
