{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.TreeGrammar.Constructor(Constr) where

import           Prelude hiding (pred,traverse,map,Either(..))
import           Control.Monad
import           Control.DeepSeq

import           Data.TreeGrammar.Terminal
import           Data.Text (Text)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.Hashable (Hashable)
import qualified Data.Hashable as Hash
import qualified Data.Traversable as T
import qualified Data.List as L
import           Data.Functor.Identity
import           Data.Identifiable

import           GHC.Exts (IsList(..))
import           Text.Printf

newtype Constr n = Constr (HashMap Text (IntMap (HashSet [n]))) deriving (Eq,NFData)

instance (Eq n, Hashable n) => Hashable (Constr n) where
  hashWithSalt s t = runIdentity (hashWithSalt (\s' -> Identity . Hash.hashWithSalt s') s t)

instance Identifiable n => IsList (Constr n) where
  type Item (Constr n) = (Text,[n])
  fromList l = Constr $ M.fromListWith (IM.unionWith H.union) [ (c, IM.singleton (length ts) (H.singleton ts)) | (c,ts) <- l]
  toList (Constr n) = [ (c,t) | (c,tss) <- M.toList n, ts <- IM.elems tss, t <- H.toList ts ]

instance Identifiable n => Semigroup (Constr n) where
  Constr m1 <> Constr m2 = Constr (M.unionWith (IM.unionWith (<>)) m1 m2)
instance Identifiable n => Monoid (Constr n) where
  mempty = Constr mempty
  mappend = (<>)

instance (Show n, Identifiable n) => Show (Constr n) where
  show m = L.intercalate " | " [ printf "%s%s" c (show ts) | (c,ts) <- toList m]

instance Terminal Constr where
  nonTerminals (Constr m) = H.fromList $ do
    ar <- M.elems m
    sub <- IM.elems ar
    concat (H.toList sub)

  productive prod (Constr m)
    | M.null m  = False
    | otherwise = any (any (any (all (`H.member` prod)))) m

  filter pred (Constr m) = Constr (M.map (IM.map (H.filter (all pred))) m)

  determinize f (Constr m) = Constr <$> T.traverse (T.traverse (fmap H.singleton . T.traverse f . transpose)) m

  subsetOf leq (Constr m1) (Constr m2) = do
    guard (m1 `subsetKeys` m2)
    forM_ (M.intersectionWith (,) m1 m2) $ \(a1,a2) -> do
      guard (a1 `subsetKeys'` a2)
      forM_ (IM.intersectionWith (,) a1 a2) $ \(l1,l2) ->
        leq (l1, l2)

  intersection f (Constr m1) (Constr m2) = fmap Constr
                                         $ T.traverse (T.traverse (fmap H.fromList . T.traverse (T.traverse f)))
                                         $ M.intersectionWith (IM.intersectionWith cross) m1 m2

  traverse f (Constr m) = Constr <$> T.traverse (T.traverse (traverseHashSet (T.traverse f))) m
  hashWithSalt f s (Constr n)
    = foldM (\s' (c,ts) -> foldM f (s' `Hash.hashWithSalt` c) ts) s
      [ (c,t) | (c,tss) <- M.toList n, ts <- IM.elems tss, t <- H.toList ts ]

traverseHashSet :: (Applicative f, Identifiable b) => (a -> f b) -> HashSet a -> f (HashSet b)
traverseHashSet f h = H.fromList <$> T.traverse f (H.toList h)

subsetKeys :: Identifiable a => HashMap a b -> HashMap a b' -> Bool
subsetKeys m1 m2 = all (`M.member` m2) (M.keys m1)

subsetKeys' :: IntMap a -> IntMap a' -> Bool
subsetKeys' m1 m2 = all (`IM.member` m2) (IM.keys m1)

transpose :: Identifiable n => HashSet [n] -> [HashSet n]
transpose = fmap H.fromList . L.transpose . H.toList

cross :: HashSet [a] -> HashSet [b] -> [[(a,b)]]
cross t1 t2 = [ zip as bs | as <- H.toList t1, bs <- H.toList t2]
