{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.MonotoneErrors where

import           Control.DeepSeq

import           Data.Abstract.MonotoneVersioned
import           Data.HashSet(HashSet)
import qualified Data.HashSet as Set
import           Data.Empty
import           Data.Hashable
import           Data.Identifiable
import           Data.Order
import           Data.Text.Prettyprint.Doc

newtype Errors a = Errors (Versioned (HashSet a))
  deriving (IsEmpty, PreOrd, Complete, Hashable, Eq, NFData)

insert :: Identifiable a => a -> Errors a -> Errors a
insert a err@(Errors (Versioned errors version))
  | Set.member a errors = err
  | otherwise = Errors $ Versioned
              { element = Set.insert a errors
              , version = version + 1
              }

instance (Pretty a) => Pretty (Errors a) where
  pretty (Errors (Versioned m _)) = pretty (Set.toList m)
