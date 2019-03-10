{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Abstract.TreeGrammar.ClosedSet where

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.IntSet(IntSet)
import qualified Data.IntSet as S
import           Data.IntMap(IntMap)
import qualified Data.IntMap as IM
import           Data.Identifiable

data ClosedSet a = ClosedSet
  { fresh :: Int
  , upper :: HashMap a IntSet
  , lower :: IntMap (HashSet a)
  } deriving (Show)

empty :: ClosedSet a
empty = ClosedSet 0 M.empty IM.empty

insert :: Identifiable a => [a] -> ClosedSet a -> ClosedSet a
insert l u =
  let n = fresh u
  in u { fresh = n+1
       , upper = foldl (\m a -> M.insertWith (const (S.insert n)) a (S.singleton n) m) (upper u) l
       , lower = IM.insert n (H.fromList l) (lower u)
       }

insertUpper :: Identifiable a => [a] -> ClosedSet a -> ClosedSet a
insertUpper l u
  | memberUpper l u = u
  | otherwise       = insert l u

insertLower :: Identifiable a => [a] -> ClosedSet a -> ClosedSet a
insertLower l u
  | memberLower l u = u
  | otherwise       = insert l u

member :: Identifiable a => (HashSet a -> Bool) -> [a] -> ClosedSet a -> Bool
member p l (ClosedSet {..}) = 
  let u  = foldl (\s a -> S.union s (M.lookupDefault S.empty a upper)) S.empty l
  in any (\n -> p (IM.findWithDefault H.empty n lower)) (S.toList u)

memberUpper :: Identifiable a => [a] -> ClosedSet a -> Bool
memberUpper l = member (`subset` H.fromList l) l

memberLower :: Identifiable a => [a] -> ClosedSet a -> Bool
memberLower l u = member (H.fromList l `subset`) l u

subset :: Identifiable a => HashSet a -> HashSet a -> Bool
subset xs ys = all (`H.member` ys) xs
