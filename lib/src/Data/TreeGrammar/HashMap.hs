{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.TreeGrammar.HashMap where

import           Prelude hiding (pred,filter,map,traverse)

import           Control.Monad

import           Data.TreeGrammar.Terminal

import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as H
import           Data.Identifiable
import qualified Data.Traversable as T
import qualified Data.Hashable as Hash
import           Data.Empty
import           Data.Coerce

import           Text.Printf

newtype HashMap a b n = HashMap (M.HashMap a (b n))
  deriving (Eq,IsEmpty)

instance (Show a, Show (b n)) => Show (HashMap a b n) where
  show (HashMap m)
    | M.null m = "[]"
    | otherwise = "[" ++ init (unwords [ printf "%s -> %s," (show k) (show v) | (k,v) <- M.toList m]) ++ "]"

instance (Identifiable a, Terminal b) => Terminal (HashMap a b) where
  nonTerminals (HashMap m) = H.unions $ nonTerminals <$> M.elems m
  productive prod (HashMap m)
    | M.null m = False
    | otherwise = any (productive prod) m
  filter pred (HashMap m) = HashMap (M.map (filter pred) m)
  determinize f (HashMap m) = HashMap <$> T.traverse (determinize f) m
  subsetOf leq (HashMap m1) (HashMap m2) = do
    guard (m1 `subsetKeys` m2)
    forM_ (M.intersectionWith (,) m1 m2) $ uncurry (subsetOf leq)
  intersection f (HashMap m1) (HashMap m2) = fmap HashMap $ T.sequenceA $ M.intersectionWith (intersection f) m1 m2
  map f (HashMap m) = HashMap (M.map (map f) m)
  traverse f (HashMap m) = HashMap <$> T.traverse (traverse f) m
  hashWithSalt f s0 (HashMap m) = foldM (\s (a,b) -> hashWithSalt f (s `Hash.hashWithSalt` a) b) s0 (M.toList m)

withHashMap :: (M.HashMap a (b n) -> M.HashMap a' (b' n')) -> HashMap a b n -> HashMap a' b' n'
withHashMap = coerce
{-# INLINE withHashMap #-}

subsetKeys :: Identifiable a => M.HashMap a b -> M.HashMap a b' -> Bool
subsetKeys m1 m2 = all (`M.member` m2) (M.keys m1)

instance (Identifiable a, Semigroup (b n)) => Semigroup (HashMap a b n) where
  HashMap m1 <> HashMap m2 = HashMap (M.unionWith (<>) m1 m2)

instance (Identifiable a, Semigroup (b n)) => Monoid (HashMap a b n) where
  mappend = (<>)
  mempty = HashMap M.empty
