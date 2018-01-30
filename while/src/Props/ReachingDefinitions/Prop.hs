module Props.ReachingDefinitions.Prop where

import Label
import Data.Order
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


-- Set of labeled assignments that reach the current execution point
type ReachingDefs = Map Text (Set Label)

initReachingDefs :: ReachingDefs
initReachingDefs = Map.empty

---------------
-- concrete
---------------

type CProp = ReachingDefs

initCProp :: CProp
initCProp = initReachingDefs

liftCProp :: CProp -> AProp
liftCProp = id

---------------
-- abstract
---------------

type AProp = ReachingDefs

initAProp :: AProp
initAProp = initReachingDefs


---------------
-- Galois
---------------

-- derived
