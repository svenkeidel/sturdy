{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Abstract.Constructor(Constr,isEmpty,isSingleton,widening) where

import           Control.DeepSeq

import           Data.Order
import           Data.Hashable
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.Text (Text)
import           Data.Identifiable
import           Data.Abstract.Widening
import           Data.Abstract.Stable

import           GHC.Exts

newtype Constr n = Constr (HashMap Text (IntMap [n])) deriving (Eq,Hashable,NFData)

instance (Identifiable n, Complete n, Show n) => Show (Constr n) where
  show cs = show (toList cs)

instance PreOrd n => PreOrd (Constr n) where
  Constr m1 ⊑ Constr m2 =
   (m1 `subsetKeys` m2) && forAll (M.intersectionWith (,) m1 m2) (\(a1,a2) ->
     (a1 `subsetKeys'` a2) && forAll (IM.intersectionWith (,) a1 a2) (\(ts1,ts2) ->
        ts1 ⊑ ts2))

instance Complete n => Complete (Constr n) where
  Constr m1 ⊔ Constr m2 = Constr (M.unionWith (IM.unionWith (zipWith (⊔))) m1 m2)

widening :: (Complete n) => Widening n -> Widening (Constr n)
widening w c1@(Constr m1) c2@(Constr m2) =
  let c3 = Constr (M.unionWith (IM.unionWith (zipWith (\x y -> snd (w x y)))) m1 m2)
  in (if c3 ⊑ c1 && c3 ⊑ c2 then Stable else Unstable, c3)

instance PreOrd n => LowerBounded (Constr n) where
  bottom = Constr M.empty

instance CoComplete n => CoComplete (Constr n) where
  Constr m1 ⊓ Constr m2 = removeEmpty $ Constr (M.intersectionWith (IM.intersectionWith (zipWith (⊓))) m1 m2)

instance (Identifiable n, Complete n) => IsList (Constr n) where
  type Item (Constr n) = (Text,[n])
  fromList l = Constr $ M.fromListWith (IM.unionWith (zipWith (⊔))) [ (c, IM.singleton (length ts) ts) | (c,ts) <- l]
  toList (Constr n) = [ (c,ts) | (c,tss) <- M.toList n, ts <- IM.elems tss ]

instance Hashable n => Hashable (IntMap n) where
  hashWithSalt s m = hashWithSalt s (IM.toList m)

isEmpty :: Constr n -> Bool
isEmpty (Constr n) = M.null n

isSingleton :: (n -> Bool) -> Constr n -> Bool
isSingleton isSing (Constr n) = M.size n == 1 && forAll n (\a -> IM.size a == 1 && forAll a (all isSing))

-- size :: (Identifiable n, Complete n) => Constr n -> Int
-- size c = length (toList c)

removeEmpty :: Constr n -> Constr n
removeEmpty (Constr m) = Constr (M.mapMaybe (\a -> if IM.null a then Nothing else Just a) m)

forAll :: Foldable t => t a -> (a -> Bool) -> Bool
forAll l p = all p l

subsetKeys :: Identifiable a => HashMap a b -> HashMap a b' -> Bool
subsetKeys m1 m2 = all (`M.member` m2) (M.keys m1)

subsetKeys' :: IntMap a -> IntMap a' -> Bool
subsetKeys' m1 m2 = all (`IM.member` m2) (IM.keys m1)
