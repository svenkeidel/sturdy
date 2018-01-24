module Data.Utils where

import Data.Maybe
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map

lookupM :: (Ord k, Monoid v) => k -> Map k v -> v
lookupM x m = fromMaybe mempty $ Map.lookup x m

