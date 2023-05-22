{-# LANGUAGE TypeFamilies #-}
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
import           Prettyprinter

import           GHC.Exts

newtype Errors a = Errors (Versioned (HashSet a))
  deriving (IsEmpty, PreOrd, Complete, Hashable, Eq, NFData, Show)

insert :: Identifiable a => a -> Errors a -> Errors a
insert a err@(Errors (Versioned errors version))
  | Set.member a errors = err
  | otherwise = Errors $ Versioned
              { element = Set.insert a errors
              , version = version + 1
              }

toSet :: Errors a -> HashSet a
toSet (Errors (Versioned s _)) = s

instance (Pretty a) => Pretty (Errors a) where
  pretty (Errors (Versioned m _)) = pretty (Set.toList m)

instance Identifiable a => IsList (Errors a) where
  type Item (Errors a) = a
  fromList l = Errors (Versioned (fromList l) 0)
  toList (Errors (Versioned l _)) = toList l
