{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Store(Store,subsetKeys,empty,lookup,insert,insertWith,(!),keys,toList,fromList) where

import           Prelude hiding (lookup)

import           Data.Order
import           Data.Hashable
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import           Data.HashSet (HashSet)
import qualified Data.HashSet as S
import           Text.Printf

newtype Store a b = Store (HashMap a b) deriving (Eq,Functor,Foldable,Traversable,Hashable)

instance (Show a,Show b) => Show (Store a b) where
  show (Store h)
    | H.null h = "[]"
    | otherwise = "[" ++ init (unwords [ printf "%s -> %s," (show k) (show v) | (k,v) <- H.toList h]) ++ "]"

instance (Eq a, Hashable a, PreOrd b) => PreOrd (Store a b) where
  Store m1 ⊑ Store m2 = subsetKeys m1 m2 && all (\(k,v1) -> v1 ⊑ (m2 H.! k)) (H.toList m1)
  Store m1 ≈ Store m2 = H.keys m1 == H.keys m2 && all (\(k,v_o) -> v_o ⊑ (m2 H.! k)) (H.toList m1)

instance (Eq a, Hashable a, Complete b) => Complete (Store a b) where
  Store m1 ⊔ Store m2 = Store (H.unionWith (⊔) m1 m2)

instance (Eq a, Hashable a, CoComplete b) => CoComplete (Store a b) where
  Store m1 ⊓ Store m2 = Store (H.intersectionWith (⊓) m1 m2)

instance (Eq a, Hashable a, PreOrd b) => LowerBounded (Store a b) where
  bottom = empty

subsetKeys :: (Eq a, Hashable a) => HashMap a b -> HashMap a b -> Bool
subsetKeys m1 m2 = subset (S.fromMap (H.map (const ()) m1)) (S.fromMap (H.map (const ()) m2))

subset :: (Eq a, Hashable a) => HashSet a -> HashSet a -> Bool
subset s1 s2 = S.size (S.intersection s1 s2) == S.size s1

empty :: Store a b
empty = Store H.empty

lookup :: (Eq a, Hashable a) => a -> Store a b -> Maybe b
lookup a (Store m) = H.lookup a m

insert :: (Eq a, Hashable a) => a -> b -> Store a b -> Store a b
insert a b (Store m) = Store (H.insert a b m)

insertWith :: (Eq a, Hashable a) => (b -> b -> b) -> a -> b -> Store a b -> Store a b
insertWith f a b (Store m) = Store (H.insertWith f a b m)

(!) :: (Eq a, Hashable a) => Store a b -> a -> b
Store m ! a = m H.! a

keys :: Store a b -> [a]
keys (Store m) = H.keys m

toList :: Store a b -> [(a,b)]
toList (Store m) = H.toList m

fromList :: (Eq a, Hashable a) => [(a,b)] -> Store a b
fromList l = Store (H.fromList l)
