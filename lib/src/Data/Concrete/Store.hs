
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Concrete.Store(Store,subsetKeys,empty,lookup,insert,insertWith,adjust,(!),keys,toList,fromList) where

import           Prelude hiding (lookup)

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import           Data.HashSet (HashSet)
import qualified Data.HashSet as S
import           Data.Hashable
import           Data.Identifiable
import           Data.Concrete.Error

import           Text.Printf

newtype Store a b = Store (HashMap a b) deriving (Eq,Functor,Foldable,Traversable,Hashable)

instance (Show a,Show b) => Show (Store a b) where
  show (Store h)
    | H.null h = "[]"
    | otherwise = "[" ++ init (unwords [ printf "%s -> %s," (show k) (show v) | (k,v) <- H.toList h]) ++ "]"

subsetKeys :: Identifiable a => HashMap a b -> HashMap a b -> Bool
subsetKeys m1 m2 = subset (S.fromMap (H.map (const ()) m1)) (S.fromMap (H.map (const ()) m2))

subset :: Identifiable a => HashSet a -> HashSet a -> Bool
subset s1 s2 = S.size (S.intersection s1 s2) == S.size s1

empty :: Store a b
empty = Store H.empty

lookup :: Identifiable a => a -> Store a b -> Error () b
lookup a (Store m) = case H.lookup a m of
  Just x -> Success x
  Nothing -> Fail ()

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

toList :: Store a b -> [(a,b)]
toList (Store m) = H.toList m

fromList :: Identifiable a => [(a,b)] -> Store a b
fromList l = Store (H.fromList l)

