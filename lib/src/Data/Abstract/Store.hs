{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Abstract.Store(Store,subsetKeys,empty,lookup,insert,insertWith,adjust,(!),keys,toList,fromList,map) where

import           Prelude hiding (lookup,map)

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import           Data.HashSet (HashSet)
import qualified Data.HashSet as S
import           Data.Hashable
import           Data.Order
import           Data.Identifiable

import           Data.Abstract.Widening

import           GHC.Exts

import           Text.Printf

newtype Store a b = Store (HashMap a b) deriving (Eq,Functor,Foldable,Traversable,Hashable)

instance (Show a,Show b) => Show (Store a b) where
  show (Store h)
    | H.null h = "[]"
    | otherwise = "[" ++ init (unwords [ printf "%s -> %s," (show k) (show v) | (k,v) <- H.toList h]) ++ "]"

instance (Identifiable a, PreOrd b) => PreOrd (Store a b) where
  Store m1 ⊑ Store m2 = subsetKeys m1 m2 && all (\(k,v1) -> v1 ⊑ (m2 H.! k)) (H.toList m1)
  Store m1 ≈ Store m2 = H.keys m1 == H.keys m2 && all (\(k,v_o) -> v_o ⊑ (m2 H.! k)) (H.toList m1)

instance (Identifiable a, Complete b) => Complete (Store a b) where
  Store m1 ⊔ Store m2 = Store (H.unionWith (⊔) m1 m2)

instance (Identifiable a, CoComplete b) => CoComplete (Store a b) where
  Store m1 ⊓ Store m2 = Store (H.intersectionWith (⊓) m1 m2)

instance (Identifiable a, Widening b) => Widening (Store a b) where
  Store m1 ▽ Store m2 = Store (H.unionWith (▽) m1 m2)

instance (Identifiable a, PreOrd b) => LowerBounded (Store a b) where
  bottom = empty

map :: (Identifiable a', Complete b') => ((a,b) -> Maybe (a',b')) -> Store a b -> Store a' b'
map f (Store h) = Store (H.fromListWith (⊔) [ (a,b) | Just (a,b) <- fmap f (H.toList h)])

subsetKeys :: Identifiable a => HashMap a b -> HashMap a b -> Bool
subsetKeys m1 m2 = subset (S.fromMap (H.map (const ()) m1)) (S.fromMap (H.map (const ()) m2))

subset :: Identifiable a => HashSet a -> HashSet a -> Bool
subset s1 s2 = S.size (S.intersection s1 s2) == S.size s1

empty :: Store a b
empty = Store H.empty

lookup :: Identifiable a => a -> Store a b -> Maybe b
lookup a (Store m) = H.lookup a m

insert :: Identifiable a => a -> b -> Store a b -> Store a b
insert a b (Store m) = Store (H.insert a b m)

insertWith :: Identifiable a => (b -> b -> b) -> a -> b -> Store a b -> Store a b
insertWith f a b (Store m) = Store (H.insertWith f a b m)

adjust :: Identifiable a => (b -> b) -> a -> Store a b -> Store a b
adjust f a (Store m) = Store (H.adjust f a m)

(!) :: Identifiable a => Store a b -> a -> b
Store m ! a = m H.! a

keys :: Store a b -> [a]
keys (Store m) = H.keys m

instance Identifiable a => IsList (Store a b) where
  type Item (Store a b) = (a,b)
  toList (Store m) = H.toList m
  fromList l = Store (H.fromList l)
