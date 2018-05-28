{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Concrete.Environment(Env,empty,lookup,insert,insertWith,fromList,toList) where

import           Prelude hiding (lookup)

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import           Data.Hashable
import           Data.Identifiable

import           Text.Printf

newtype Env a b = Env (HashMap a b) deriving (Eq,Functor,Foldable,Traversable,Hashable)

instance (Show a,Show b) => Show (Env a b) where
  show (Env h)
    | H.null h = "[]"
    | otherwise = "[" ++ init (unwords [ printf "%s -> %s," (show k) (show v) | (k,v) <- H.toList h]) ++ "]"

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
