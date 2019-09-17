{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.WeakMap(Map,widening,empty,insert,lookup,lookup',delete,delete',deleteIfNotPresent,deleteIfNotPresent',union,fromList,fromList',toList,dropNegativeBindings) where

import           Prelude hiding (lookup)

import           Control.DeepSeq

import           Data.Identifiable
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M

import           Data.Order
import qualified Data.Abstract.Maybe as A
import           Data.Abstract.Widening
import           Data.Coerce
import           Data.Hashable
import           Data.Maybe (mapMaybe)

import           Text.Printf

newtype Map a b = Map (HashMap a (A.Maybe b)) deriving (NFData)

instance (Eq a, Eq b, Hashable a, UpperBounded b) => Eq (Map a b) where
  m1 == m2 = let Map m1' = normalize m1
                 Map m2' = normalize m2
             in m1' == m2'

instance (Eq a, Eq b, Hashable a, UpperBounded b, Hashable b) => Hashable (Map a b) where
  hashWithSalt s m = let Map m' = normalize m in hashWithSalt s m'

normalize :: (UpperBounded b, Eq b) => Map a b -> Map a b
normalize (Map m) = Map (M.filter (\v -> v /= A.JustNothing top) m)

instance (Identifiable a, PreOrd b) => PreOrd (Map a b) where
  (⊑) = withMap2 $ \m1 m2 -> all (\(k,v) -> M.lookup k m1 ⊑ Just v) (M.toList m2)

instance (Identifiable a, Complete b) => Complete (Map a b) where
  (⊔) = withMap2 $ M.intersectionWith (⊔)

instance (Show a,Show b) => Show (Map a b) where
  show (Map h)
    | M.null h = "[]"
    | otherwise = "[" ++ init (unwords (map showElem (M.toList h))) ++ "]"
    where
      showElem (k,m) = case m of
        A.Just x -> printf "%s -> %s," (show k) (show x)
        A.Nothing -> printf "%s❌," (show k)
        A.JustNothing x -> printf "%s? -> %s," (show k) (show x)

instance Identifiable a => Functor (Map a) where
  fmap f = withMap (fmap (fmap f))

widening :: Identifiable a => Widening b -> Widening (Map a b)
widening w = withMap2 $ \m1 m2 -> sequenceA $ M.intersectionWith (A.widening w) m1 m2

empty :: Map a b
empty = Map M.empty

insert :: Identifiable a => a -> b -> Map a b -> Map a b
insert a b = withMap $ M.insert a (A.Just b)

lookup :: (Identifiable a) => a -> b -> Map a b -> A.Maybe b
lookup a def = withMap $ \m -> case M.lookup a m of
  Just (A.Just x) -> A.Just x
  Just A.Nothing -> A.Nothing
  Just (A.JustNothing b) -> A.JustNothing b
  Nothing -> A.JustNothing def

lookup' :: (Identifiable a, UpperBounded b) => a -> Map a b -> A.Maybe b
lookup' a = lookup a top

delete :: Identifiable a => a -> Map a b -> Map a b
delete a = withMap $ M.insert a A.Nothing

delete' :: (Foldable f, Identifiable a) => f a -> Map a b -> Map a b
delete' l m = foldl (flip delete) m l

deleteIfNotPresent :: Identifiable a => a -> Map a b -> Map a b
deleteIfNotPresent a = withMap $ M.insertWith (\_ old -> old) a A.Nothing

deleteIfNotPresent' :: (Foldable f, Identifiable a) => f a -> Map a b -> Map a b
deleteIfNotPresent' l m = foldl (flip deleteIfNotPresent) m l

union :: (Identifiable a, Complete b) => Map a b -> Map a b -> Map a b
union = withMap2 $ M.unionWith join
  where
    join (A.Just a) _        = A.Just a
    join A.Nothing b         = b
    join (A.JustNothing a) b = A.Just a ⊔ b

fromList :: Identifiable a => [(a,b)] -> Map a b
fromList l = fromList' [ (a,A.Just b) | (a,b) <- l]

fromList' :: Identifiable a => [(a,A.Maybe b)] -> Map a b
fromList' l = coerce (M.fromList l)

toList :: Map a b -> [(a,b)]
toList (Map m) = mapMaybe (\(a,mb) -> case mb of A.Just b -> Just (a,b); A.JustNothing b -> Just (a,b); A.Nothing -> Nothing) (M.toList m)

withMap2 :: Coercible c c' => (HashMap a (A.Maybe b) -> HashMap a (A.Maybe b) -> c) -> Map a b -> Map a b -> c'
withMap2 = coerce
{-# INLINE withMap2 #-}

withMap :: Coercible c c' => (HashMap a (A.Maybe b) -> c) -> Map a b -> c'
withMap = coerce
{-# INLINE withMap #-}

dropNegativeBindings :: Map a b -> Map a b
dropNegativeBindings (Map m) = Map (M.filter noNothing m)
  where
    noNothing x = case x of
      A.Just _ -> True
      A.JustNothing _ -> True
      A.Nothing -> False
