{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Abstract.MonotoneStore where

import           Data.Empty
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import           Data.Hashable
import           Data.Identifiable
import           Data.Order
import           Data.Text.Prettyprint.Doc


-- | This datatype provides an abstraction for stores, whose stored objects only
-- grow monotonically (i.e., no strong updates). In particular, the abstraction
-- features a version number, which allows efficient hashcode, equality and
-- ordering checks. Whenever the store grows strictly, the version number is
-- increased. However, for this abstraction to be valid, the store needs be used
-- in a linear fashion. More specifically, each store can only be used once and
-- duplicating the store is **not** allowed.
data Store addr val
  = Store
  { store   :: HashMap addr val
  , version :: Int
  }

insert :: (Identifiable addr, Complete val)
       => addr -> val -> Store addr val -> Store addr val
insert addr valNew st@(Store {..}) = case Map.lookup addr store of
  Just valOld
    | valNew ⊑ valOld -> st
    | otherwise       -> Store { store = Map.insert addr (valOld ⊔ valNew) store
                               , version = version + 1
                               }
  Nothing             -> Store { store = Map.insert addr valNew store
                               , version = version + 1
                               }

lookup :: Identifiable addr => addr -> Store addr val -> Maybe val
lookup addr st = Map.lookup addr (store st)

instance IsEmpty (Store addr val) where
  empty = Store { store = empty, version = 0}

instance PreOrd (Store addr val) where
  s1 ⊑ s2 = version s1 <= version s2

instance Hashable (Store addr val) where
  hashWithSalt salt st = salt `hashWithSalt` version st
  hash st = version st

instance Eq (Store addr val) where
  s1 == s2 = version s1 == version s2

instance (Pretty addr, Pretty val) => Pretty (Store addr val) where
  pretty m = list [ pretty k <+> " -> " <> pretty v | (k, v) <- Map.toList (store m) ]

