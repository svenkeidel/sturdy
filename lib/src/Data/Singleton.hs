{-# LANGUAGE TypeFamilies #-}
module Data.Singleton where

import           Data.Identifiable
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M

class IsSingleton f where
  type Elem f :: *
  singleton :: Elem f -> f

instance IsSingleton (Maybe a) where
  type Elem (Maybe a) = a
  singleton = Just

instance IsSingleton [a] where
  type Elem [a] = a
  singleton a = [a]

instance Identifiable a => IsSingleton (HashSet a) where
  type Elem (HashSet a) = a
  singleton = H.singleton

instance Identifiable a => IsSingleton (HashMap a b) where
  type Elem (HashMap a b) = (a,b)
  singleton (a,b) = M.singleton a b
 
