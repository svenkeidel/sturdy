module Props.DeadWrites.Prop where

import Data.Order
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set


-- A variable write is "dead" if at least once the variable was written
-- without a subsequent read observing that write
data DeadWrites = DeadWrites {
  maybeDead :: Set Text, -- dead unless followed by a read
  mustDead :: Set Text -- definitely dead (e.g., because of double write)
}

instance PreOrd DeadWrites where
  (DeadWrites may1 must1) ⊑ (DeadWrites may2 must2) = (must1 ⊑ must2) && (maymay ⊑ may2)
    where maymay = Set.filter (\x -> Prelude.not $ Set.member x must2) may1

instance Complete DeadWrites where
  (DeadWrites may1 must1) ⊔ (DeadWrites may2 must2) = DeadWrites (may1 ⊔ may2) (must1 ⊔ must2)

initDeadWrites :: DeadWrites
initDeadWrites = DeadWrites Set.empty Set.empty

type FDeadWrites = Set Text

finalizeDeadWrites :: DeadWrites -> FDeadWrites
finalizeDeadWrites (DeadWrites may must) = may `Set.union` must

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
