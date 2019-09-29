module Data.TreeGrammar.IntMap where

import           Prelude hiding (pred,filter,map,traverse)

import           Control.Monad

import           Data.TreeGrammar.Terminal

import qualified Data.IntMap as M
import qualified Data.HashSet as H
import qualified Data.Traversable as T
import qualified Data.Hashable as Hash

newtype IntMap b n = IntMap (M.IntMap (b n))

instance (Terminal b) => Terminal (IntMap b) where
  nonTerminals (IntMap m) = H.unions $ nonTerminals <$> M.elems m
  productive prod (IntMap m)
    | M.null m = False
    | otherwise = any (productive prod) m
  filter pred (IntMap m) = IntMap (M.map (filter pred) m)
  determinize f (IntMap m) = IntMap <$> T.traverse (determinize f) m
  subsetOf leq (IntMap m1) (IntMap m2) = do
    guard (m1 `subsetKeys` m2)
    forM_ (M.intersectionWith (,) m1 m2) $ uncurry (subsetOf leq)
  map f (IntMap m) = IntMap (M.map (map f) m)
  intersection f (IntMap m1) (IntMap m2) = fmap IntMap $ T.sequenceA $ M.intersectionWith (intersection f) m1 m2
  traverse f (IntMap m) = IntMap <$> T.traverse (traverse f) m
  hashWithSalt f s0 (IntMap m) = foldM (\s (a,b) -> hashWithSalt f (s `Hash.hashWithSalt` a) b) s0 (M.toList m)

subsetKeys ::M.IntMap b -> M.IntMap b' -> Bool
subsetKeys m1 m2 = all (`M.member` m2) (M.keys m1)

instance (Semigroup (b n)) => Semigroup (IntMap b n) where
  IntMap m1 <> IntMap m2 = IntMap (M.unionWith (<>) m1 m2)

instance (Semigroup (b n)) => Monoid (IntMap b n) where
  mappend = (<>)
  mempty = IntMap M.empty
