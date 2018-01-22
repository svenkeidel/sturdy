module Props.FailedReads.Prop where

import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set


---------------
-- concrete
---------------

type Prop = Set Text

initCProp :: Prop
initCProp = Set.empty
