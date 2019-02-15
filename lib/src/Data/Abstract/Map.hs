{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
module Data.Abstract.Map (Map,singleton,empty,lookup,unsafeLookup,insert,insertWith,unsafeInsertWith,unsafeInsertWithLookup,delete,union,adjust,toList,fromList,mapMaybe,map,compose,widening,fromThereList) where

import           Prelude hiding (lookup,map,Either(..),(**))

import           Control.Arrow

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import           Data.Hashable
import           Data.Order
import           Data.Identifiable
import           Data.Maybe (fromJust)
import qualified Data.Abstract.Maybe as M
import           Data.Abstract.There (There(..))
import qualified Data.Abstract.There as T

import           Data.Abstract.Widening

import           GHC.Exts

import           Text.Printf
import           Control.DeepSeq

-- | Abstract hashmap
newtype Map a b = Map (HashMap a (There,b)) deriving (Eq,Functor,Foldable,Traversable,Hashable,NFData)

instance (Show a,Show b) => Show (Map a b) where
  show (Map h)
    | H.null h = "[]"
    | otherwise = "[" ++ init (unwords [ printf "%s%s -> %s," (show k) (show t) (show v) | (k,(t,v)) <- H.toList h]) ++ "]"

instance (Identifiable a, PreOrd b) => PreOrd (Map a b) where
  Map m1 ⊑ m2 = all (\(k,(t,v)) -> (case t of Must -> M.Just v; May -> M.JustNothing v) ⊑ lookup k m2) (H.toList m1)

instance (Identifiable a, Complete b) => Complete (Map a b) where
  (Map m1) ⊔ (Map m2) = Map $ foldl (\m a -> H.insert a (join a) m) H.empty keys
    where
      keys = H.keys (H.union m1 m2)
      join k = case (H.lookup k m1,H.lookup k m2) of
        (Just (t1,b1),Just (t2,b2)) -> (t1,b1) ⊔ (t2,b2)
        (Nothing,Just (_,b2)) -> (May,b2)
        (Just (_,b1),Nothing) -> (May,b1)
        (Nothing,Nothing) -> error "cannot happen"

widening :: Identifiable a => Widening b -> Widening (Map a b)
widening w (Map m1) (Map m2) = second Map $ foldl (\(s,m) a -> let (s',b) = join a in (s ⊔ s',H.insert a b m)) (Stable,H.empty) keys
  where
     keys = H.keys (H.union m1 m2)
     join k = case (H.lookup k m1,H.lookup k m2) of
       (Just (t1,b1),Just (t2,b2)) -> (T.widening ** w) (t1,b1) (t2,b2)
       (Nothing,Just (_,b2)) -> (Instable,(May,b2))
       (Just (_,b1),Nothing) -> (Instable,(May,b1))
       (Nothing,Nothing) -> error "cannot happen"

instance (Identifiable a, PreOrd b) => LowerBounded (Map a b) where
  bottom = empty

empty :: Map a b
empty = Map H.empty

singleton :: Identifiable a => a -> b -> Map a b
singleton a b = Map (H.singleton a (Must,b))

map :: (b -> b') -> Map a b -> Map a b'
map f (Map h) = Map (H.map (second f) h)

mapMaybe :: (Identifiable a', Complete b') => ((a,b) -> Maybe (a',b')) -> Map a b -> Map a' b'
mapMaybe f (Map h) = Map (H.fromListWith (⊔) [ (a',(here,b')) | (a,(here,b)) <- H.toList h, Just (a',b') <- return (f (a,b))])

lookup :: (Identifiable a) => a -> Map a b -> M.Maybe b
lookup a (Map m) = case H.lookup a m of
  Just (Must,b) -> M.Just b
  Just (May,b) -> M.JustNothing b
  Nothing -> M.Nothing

unsafeLookup :: (Identifiable a) => a -> Map a b -> Maybe b
unsafeLookup a (Map m) = case H.lookup a m of
  Just (_,b) -> Just b
  Nothing -> Nothing

insert :: Identifiable a => a -> b -> Map a b -> Map a b
insert a b (Map m) = Map (H.insert a (Must,b) m)

insertWith :: (Identifiable a,Complete b) => (b -> b -> b) -> a -> b -> Map a b -> Map a b
insertWith f a b (Map m) = Map (H.insertWith (\(_,new) (here,old) -> case here of
    Must -> (Must,f new old)
    May -> (Must,new ⊔ f new old)
  ) a (Must,b) m)

unsafeInsertWith :: (Identifiable a) => (b -> b -> b) -> a -> b -> Map a b -> Map a b
unsafeInsertWith f a b (Map m) = Map (H.insertWith (\(_,new) (_,old) -> (Must,f new old)) a (Must,b) m)

unsafeInsertWithLookup :: (Identifiable a) => (b -> b -> b) -> a -> b -> Map a b -> (b,Map a b)
unsafeInsertWithLookup f a b m =
  let m' = unsafeInsertWith f a b m
  in (fromJust (unsafeLookup a m'),m')

delete :: Identifiable a => a -> Map a b -> Map a b
delete a (Map m) = Map (H.delete a m)

union :: (Identifiable a, Complete b) => Map a b -> Map a b -> Map a b
union (Map m1) (Map m2) = Map (H.unionWith (\(here,l) (_,r) -> case here of
    Must -> (Must,l)
    May -> (May,l ⊔ r)
  ) m1 m2)

adjust :: Identifiable a => (b -> b) -> a -> Map a b -> Map a b
adjust f a (Map m) = Map (H.adjust (second f) a m)

compose :: (Identifiable a, Identifiable b, Complete c) => [(a,b)] -> Map b c -> Map a c
compose f (Map g) = Map $ H.fromListWith (⊔) [ (a,c) | (a,b) <- f, Just c <- return $ H.lookup b g ]

instance Identifiable a => IsList (Map a b) where
  type Item (Map a b) = (a,b)
  toList (Map m) = H.toList (H.map snd m)
  fromList l = Map (H.fromList (fmap (second (Must,)) l))

fromThereList :: Identifiable a => [(a,(There,b))] -> Map a b
fromThereList = Map . H.fromList
