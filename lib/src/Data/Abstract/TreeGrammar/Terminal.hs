{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Abstract.TreeGrammar.Terminal where

import           Prelude hiding (pred,traverse)
import           Control.Monad

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

import           GHC.Exts (IsList(..))
import           Text.Printf

type Identifiable a = (Hashable a, Eq a)

class Terminal t where
  nonTerminals :: Identifiable n => t n -> HashSet n
  productive :: Identifiable n => HashSet n -> t n -> Bool
  filter :: (n -> Bool) -> t n -> t n
  determinize :: (Identifiable n, Identifiable n', Applicative f) => (HashSet n -> f n') -> t n -> f (t n')
  subsetOf :: (Identifiable n, Identifiable n', MonadPlus f) => ([(n,n')] -> f ()) -> t n -> t n' -> f ()
  intersection :: (Identifiable n1, Identifiable n2, Identifiable n', Applicative f) => ([(n1,n2)] -> f [n']) -> t n1 -> t n2 -> f (t n')
  traverse :: (Identifiable n', Applicative f) => (n -> f n') -> t n -> f (t n')
  hashWithSalt :: (Identifiable n, Monad f) => (Int -> n -> f Int) -> Int -> t n -> f Int

map :: (Identifiable n, Identifiable n', Terminal t) => (n -> n') -> t n -> t n'
map f t = runIdentity (traverse (Identity . f) t)

newtype Constr n = Constr (HashMap Text (IntMap (HashSet [n]))) deriving (Eq)

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
      forM_ (IM.intersectionWith (,) a1 a2) $ \(l1,l2)-> do
        forM_ l1 $ \as ->
          msum [ leq (zip as bs) | bs <- H.toList l2 ]

  intersection f (Constr m1) (Constr m2) = Constr <$> T.traverse (T.traverse (fmap H.fromList . T.traverse f)) (M.intersectionWith (IM.intersectionWith cross) m1 m2)
    where
      cross :: HashSet [a] -> HashSet [b] -> [[(a,b)]]
      cross t1 t2 = [ zip as bs | as <- (H.toList t1), bs <- (H.toList t2)]

  traverse f (Constr m) = Constr <$> T.traverse (T.traverse (traverseHashSet (T.traverse f))) m
  hashWithSalt f s m = foldM (\s' (c,ts) -> foldM f (s' `Hash.hashWithSalt` c) ts) s (toList m)

traverseHashSet :: (Applicative f, Identifiable b) => (a -> f b) -> HashSet a -> f (HashSet b)
traverseHashSet f h = H.fromList <$> T.traverse f (H.toList h)

subsetKeys :: Identifiable a => HashMap a b -> HashMap a b' -> Bool
subsetKeys m1 m2 = all (`M.member` m2) (M.keys m1)

subsetKeys' :: IntMap a -> IntMap a' -> Bool
subsetKeys' m1 m2 = all (`IM.member` m2) (IM.keys m1)

transpose :: Identifiable n => HashSet [n] -> [HashSet n]
transpose = fmap H.fromList . L.transpose . H.toList
