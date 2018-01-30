module Props.UseDef.Prop where

import Label
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

---------------
-- concrete
---------------

data TrUseDef = TrUse Text Label
              | TrDef Text Label

type CProp = [TrUseDef]

initCProp :: CProp
initCProp = []

data UseDef = UseDef { useDef :: Map Label Label, defUse :: Map Label (Set Label) }
type FCProp = UseDef

finalizeCProp :: CProp -> FCProp
finalizeCProp = snd . foldl go (Map.empty, UseDef Map.empty Map.empty)
  where go :: (Map Text Label, UseDef) -> TrUseDef -> (Map Text Label, UseDef)
        go (env, ud) (TrDef v def) = (Map.insert v def env, ud)
        go (env, UseDef ud du) (TrUse v use) =
          (env, UseDef (Map.insert use def ud)
                       (Map.insertWith Set.union def (Set.singleton use) du))
          where def = env Map.! v
