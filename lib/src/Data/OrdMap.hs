{-# LANGUAGE RecordWildCards #-}
module Data.OrdMap
( OrdMap
, empty
, insert
, invert
, lower
, upper
, compare
, Ordering (..)
)
where

import           Prelude hiding (Ordering,compare)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.Identifiable

-- | Datatype that describes a subset of a heterogenous order with
-- fast access to the lower and upper sets of elements.
data OrdMap n1 n2 = OrdMap
  { lowerUpper :: HashMap n1 (HashSet n2,HashSet n2)
  , incomparable :: HashSet (n1,n2)
  }

data Ordering = LessThan | GreaterThan | Equivalent | Incomparable

empty :: OrdMap n1 n2
empty = OrdMap M.empty H.empty

insert :: (Identifiable n1, Identifiable n2) => n1 -> n2 -> Ordering -> OrdMap n1 n2 -> OrdMap n1 n2
insert n1 n2 ord m@(OrdMap {..}) = case ord of
  Incomparable -> m { incomparable = H.insert (n1,n2) incomparable }
  LessThan     -> m { lowerUpper = M.insertWith (\_ (lo,up) -> (lo,H.insert n2 up)) n1 (H.empty,H.singleton n2) lowerUpper }
  GreaterThan  -> m { lowerUpper = M.insertWith (\_ (lo,up) -> (H.insert n2 lo,up)) n1 (H.singleton n2,H.empty) lowerUpper }
  Equivalent   -> insert n1 n2 GreaterThan $ insert n1 n2 LessThan m

invert :: (Identifiable n1,Identifiable n2) => OrdMap n1 n2 -> OrdMap n2 n1
invert o0 = M.foldlWithKey' (\o n1 (lo,up) -> foldl (\o' n2 -> insert n2 n1 GreaterThan o')
                                                (foldl (\o' n2 -> insert n2 n1 LessThan o') o lo)
                                              up)
           (OrdMap M.empty (H.map (\(n1,n2) -> (n2,n1)) (incomparable o0)))
           (lowerUpper o0)

lower :: Identifiable n1 => n1 -> OrdMap n1 n2 -> Maybe (HashSet n2)
lower n1 m = fst <$> M.lookup n1 (lowerUpper m)

upper :: Identifiable n1 => n1 -> OrdMap n1 n2 -> Maybe (HashSet n2)
upper n1 m = snd <$> M.lookup n1 (lowerUpper m)

compare :: (Identifiable n1,Identifiable n2) => n1 -> n2 -> OrdMap n1 n2 -> Maybe Ordering
compare n1 n2 (OrdMap {..}) = case M.lookup n1 lowerUpper of
  Just (lo,up)
    | n2 `H.member` lo && n2 `H.member` up -> Just Equivalent
    | n2 `H.member` lo                     -> Just GreaterThan
    | n2 `H.member` up                     -> Just LessThan
  _ | H.member (n1,n2) incomparable        -> Just Incomparable
    | otherwise                            -> Nothing

instance (Identifiable n1, Identifiable n2) => Semigroup (OrdMap n1 n2) where
  o1 <> o2 =
    OrdMap
    { lowerUpper   = M.unionWith (<>) (lowerUpper o1) (lowerUpper o2)
    , incomparable = H.union (incomparable o1) (incomparable o2)
    }
instance (Identifiable n1, Identifiable n2) => Monoid (OrdMap n1 n2) where
  mempty = empty
  mappend = (<>)
