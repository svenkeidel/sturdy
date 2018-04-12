module Props.FailedReads.Prop where

import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set


---------------
-- concrete
---------------

type CProp = Set Text

initCProp :: CProp
initCProp = Set.empty

liftCProp :: CProp -> CProp
liftCProp = id

---------------
-- abstract
---------------

type AProp = Set Text

initAProp :: AProp
initAProp = Set.empty


---------------
-- Galois
---------------

-- derived
