{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.MonotoneStore where

import           Data.Abstract.MonotoneVersioned
import           Data.Empty
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
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
newtype Store addr val = Store (Versioned (HashMap addr val))
  deriving (IsEmpty, PreOrd, Hashable, Eq, Show)

insert :: (Identifiable addr, Complete val)
       => addr -> val -> Store addr val -> Store addr val
insert addr valNew st@(Store (Versioned store version)) = case Map.lookup addr store of
  Just valOld
    | valNew ⊑ valOld -> st
    | otherwise       -> Store $ Versioned
                               { element = Map.insert addr (valOld ⊔ valNew) store
                               , version = version + 1
                               }
  Nothing             -> Store $ Versioned
                               { element = Map.insert addr valNew store
                               , version = version + 1
                               }
{-# SCC insert #-}

lookup :: Identifiable addr => addr -> Store addr val -> Maybe val
lookup addr (Store (Versioned { element = st })) = Map.lookup addr st
{-# SCC lookup #-}

instance (Pretty addr, Pretty val) => Pretty (Store addr val) where
  pretty (Store m) = list [ pretty k <+> " -> " <> pretty v | (k, v) <- Map.toList (element m) ]

