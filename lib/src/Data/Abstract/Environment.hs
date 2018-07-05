{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.Environment(Env,empty,lookup,insert,insertWith,fromList,toList,toMap) where

import           Prelude hiding (lookup)

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import           Data.Hashable
import           Data.Identifiable
import           Data.Order

import           Text.Printf

newtype Env a b = Env (HashMap a b) deriving (Eq,Functor,Foldable,Traversable,Hashable)

instance (Show a,Show b) => Show (Env a b) where
  show (Env h)
    | H.null h = "[]"
    | otherwise = "[" ++ init (unwords [ printf "%s -> %s," (show k) (show v) | (k,v) <- H.toList h]) ++ "]"

instance (Identifiable a, PreOrd b) => PreOrd (Env a b) where
  Env m1 ⊑ Env m2 = H.keys m1 == H.keys m2 && all (\(k,v1) -> v1 ⊑ (m2 H.! k)) (H.toList m1)
  Env m1 ≈ Env m2 = H.keys m1 == H.keys m2 && all (\(k,v_o) -> v_o ⊑ (m2 H.! k)) (H.toList m1)

empty :: Env a b
empty = Env H.empty

lookup :: Identifiable a => a -> Env a b -> Maybe b
lookup a (Env m) = H.lookup a m

insert :: Identifiable a => a -> b -> Env a b -> Env a b
insert a b (Env m) = Env (H.insert a b m)

insertWith :: Identifiable a => (b -> b -> b) -> a -> b -> Env a b -> Env a b
insertWith f a b (Env m) = Env (H.insertWith f a b m)

fromList :: Identifiable a => [(a,b)] -> Env a b
fromList = Env . H.fromList

toList :: Env a b -> [(a,b)]
toList (Env e) = H.toList e

toMap :: Env a b -> HashMap a b
toMap (Env e) = e
