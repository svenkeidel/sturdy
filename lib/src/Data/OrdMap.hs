{-# LANGUAGE ViewPatterns #-}
module Data.OrdMap
( OrdMap
, empty
, insertLeq
, insertNotLeq
, insertGeq
, insertNotGeq
, leq
, geq
)
where

import           Prelude hiding (Ordering,compare)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Identifiable
import qualified Data.Abstract.Boolean as A

-- | Datatype that describes a subset of a heterogenous order with
-- fast access to the lower and upper sets of elements.
data OrdMap n1 n2 = OrdMap { lowerN1 :: HashMap n1 (Boundary n2), upperN2 :: HashMap n2 (Boundary n1) }
data Boundary n = Boundary { comparable :: HashSet (HashSet n), incomparable :: HashSet (HashSet n) }

comparables :: Identifiable n => [n] -> Boundary n
comparables ns = Boundary (H.singleton (H.fromList ns)) H.empty

incomparables :: Identifiable n => [n] -> Boundary n
incomparables ns = Boundary H.empty (H.singleton (H.fromList ns))

empty :: OrdMap n1 n2
empty = OrdMap M.empty M.empty

insertLeq :: (Identifiable n1, Identifiable n2) => n1 -> [n2] -> OrdMap n1 n2 -> OrdMap n1 n2
insertLeq n1 n2 o = o { lowerN1 = M.insertWith (<>) n1 (comparables n2) (lowerN1 o) }

insertNotLeq :: (Identifiable n1, Identifiable n2) => n1 -> [n2] -> OrdMap n1 n2 -> OrdMap n1 n2
insertNotLeq n1 n2 o = o { lowerN1 = M.insertWith (<>) n1 (incomparables n2) (lowerN1 o) } 

insertGeq :: (Identifiable n1, Identifiable n2) => [n1] -> n2 -> OrdMap n1 n2 -> OrdMap n1 n2
insertGeq n1 n2 o = o { upperN2 = M.insertWith (<>) n2 (comparables n1) (upperN2 o) }

insertNotGeq :: (Identifiable n1, Identifiable n2) => [n1] -> n2 -> OrdMap n1 n2 -> OrdMap n1 n2
insertNotGeq n1 n2 o = o { upperN2 = M.insertWith (<>) n2 (incomparables n1) (upperN2 o) }

leq :: (Identifiable n1, Identifiable n2) => n1 -> [n2] -> OrdMap n1 n2 -> A.Bool
leq n1 (H.fromList -> n2) o = case M.lookup n1 (lowerN1 o) of
  Just bounds
    | any (\xs -> xs `subset` n2) (comparable bounds)   -> A.True
    | any (\xs -> n2 `subset` xs) (incomparable bounds) -> A.False
  _ -> A.Top

geq :: (Identifiable n1, Identifiable n2) => [n1] -> n2 -> OrdMap n1 n2 -> A.Bool
geq (H.fromList -> n1) n2 o = case M.lookup n2 (upperN2 o) of
  Just bounds
    | any (\xs -> xs `subset` n1) (comparable bounds)   -> A.True
    | any (\xs -> n1 `subset` xs) (incomparable bounds) -> A.False
  _ -> A.Top

subset :: Identifiable a => HashSet a -> HashSet a -> Bool
subset xs ys = all (`H.member` ys) xs

instance Identifiable n => Semigroup (Boundary n) where
  Boundary c1 i1 <> Boundary c2 i2 = Boundary (c1 <> c2) (i1 <> i2)

instance (Identifiable n1, Identifiable n2) => Semigroup (OrdMap n1 n2) where
  OrdMap m1 m2 <> OrdMap m1' m2' = OrdMap (M.unionWith (<>) m1 m1') (M.unionWith (<>) m2 m2')
   
instance (Identifiable n1, Identifiable n2) => Monoid (OrdMap n1 n2) where
  mempty = empty
  mappend = (<>)
