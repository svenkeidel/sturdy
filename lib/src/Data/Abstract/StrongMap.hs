{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.StrongMap(Map,widening,empty,insert,lookup,lookup',delete,delete',fromList,toList) where

import           Prelude hiding (lookup)

import           Control.Arrow
import           Control.DeepSeq

import           Data.Identifiable
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M

import           Data.Order
import qualified Data.Abstract.Maybe as A
import           Data.Abstract.Widening
import           Data.Hashable

import           Text.Printf
import           GHC.Generics(Generic)

data Map a b = Map (HashMap a b) | Top deriving (Eq,Generic)

instance (Identifiable a, PreOrd b) => PreOrd (Map a b) where
  _      ⊑ Top    = True
  Map m1 ⊑ Map m2 = M.keys m1 == M.keys m2 && and (M.intersectionWith (⊑) m1 m2)
  _      ⊑ _      = False

instance (Identifiable a, Complete b) => Complete (Map a b) where
  Top ⊔ _ = Top
  _ ⊔ Top = Top
  Map m1 ⊔ Map m2 | M.keys m1 == M.keys m2 = Map $ M.unionWith (⊔) m1 m2
                  | otherwise              = Top

instance (Identifiable a, PreOrd b) => UpperBounded (Map a b) where
  top = Top

widening :: Identifiable a => Widening b -> Widening (Map a b)
widening _ Top Top = (Stable,Top)
widening w (Map m1) (Map m2) | M.keys m1 == M.keys m2 = second Map $ sequenceA $ M.intersectionWith w m1 m2
                             | otherwise = (Instable,Top)
widening _ _ _ = (Instable,Top)

empty :: Map a b
empty = Map M.empty

insert :: Identifiable a => a -> b -> Map a b -> Map a b
insert _ _ Top = Top
insert a b (Map m) = Map $ M.insert a b m

lookup :: (Identifiable a) => a -> b -> Map a b -> A.Maybe b
lookup _ def Top = A.JustNothing def
lookup a _ (Map m) = case M.lookup a m of
  Just x -> A.Just x
  Nothing -> A.Nothing

lookup' :: (Identifiable a, UpperBounded b) => a -> Map a b -> A.Maybe b
lookup' a = lookup a top

delete :: Identifiable a => a -> Map a b -> Map a b
delete _ Top = Top
delete a (Map m) = Map (M.delete a m)

delete' :: (Foldable f, Identifiable a) => f a -> Map a b -> Map a b
delete' l m = foldl (flip delete) m l

fromList :: Identifiable a => [(a,b)] -> Map a b
fromList l = Map (M.fromList l)

toList :: Map a b -> Maybe [(a,b)]
toList Top = Nothing
toList (Map m) = Just (M.toList m)

instance (Hashable a, Hashable b) => Hashable (Map a b)
instance (NFData a, NFData b) => NFData (Map a b)
instance (Show a,Show b) => Show (Map a b) where
  show Top = "⊤"
  show (Map h)
    | M.null h = "[]"
    | otherwise = "[" ++ init (unwords ([printf "%s -> %s," (show k) (show x) | (k,x) <- M.toList h])) ++ "]"
