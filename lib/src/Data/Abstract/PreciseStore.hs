{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
module Data.Abstract.PreciseStore (Store,singleton,empty,lookup,insert,insertWith,delete,union,adjust,toList,fromList,mapMaybe,map,compose,widening) where

import           Prelude                hiding (Either (..), lookup, map, (**))

import           Control.Arrow

import           Data.Abstract.Either   (Either (..))
import qualified Data.Abstract.Either   as E
import qualified Data.Abstract.Maybe    as M
import           Data.Hashable
import           Data.HashMap.Lazy      (HashMap)
import qualified Data.HashMap.Lazy      as H
import           Data.Identifiable
import           Data.Order

import           Data.Abstract.Widening

import           GHC.Exts

import           Text.Printf

-- | Datatype that indicates if a value in the map must be there or may not be there.
data There = Must | May deriving (Eq)

-- | Abstract hashmap
newtype Store a b = Store (HashMap a (There,b)) deriving (Eq,Functor,Foldable,Traversable,Hashable)

instance (Show a,Show b) => Show (Store a b) where
  show (Store h)
    | H.null h = "[]"
    | otherwise = "[" ++ init (unwords [ printf "%s%s -> %s," (show k) (show t) (show v) | (k,(t,v)) <- H.toList h]) ++ "]"

instance (Identifiable a, PreOrd b) => PreOrd (Store a b) where
  Store m1 ⊑ m2 = all (\(k,(t,v)) -> (case t of Must -> M.Just v; May -> M.JustNothing v) ⊑ lookup k m2) (H.toList m1)

instance (Identifiable a, Complete b) => Complete (Store a b) where
  Store m1 ⊔ Store m2 = Store $ H.map join $ H.unionWith (⊔) (H.map Left m1) (H.map Right m2)
    where
      join :: Complete a => Either (There,a) (There,a) -> (There,a)
      join e = case e of
        Left (_,a)              -> (May,a)
        Right (_,b)             -> (May,b)
        LeftRight (t1,a) (t2,b) -> (t1 ⊔ t2,a ⊔ b)

instance (Identifiable a, PreOrd b) => LowerBounded (Store a b) where
  bottom = empty

empty :: Store a b
empty = Store H.empty

singleton :: Identifiable a => a -> b -> Store a b
singleton a b = Store (H.singleton a (Must,b))

map :: (b -> b') -> Store a b -> Store a b'
map f (Store h) = Store (H.map (second f) h)

mapMaybe :: (Identifiable a', Complete b') => ((a,b) -> Maybe (a',b')) -> Store a b -> Store a' b'
mapMaybe f (Store h) = Store (H.fromListWith (⊔) [ (a',(here,b')) | (a,(here,b)) <- H.toList h, Just (a',b') <- return (f (a,b))])

lookup :: (Identifiable a) => a -> Store a b -> M.Maybe b
lookup a (Store m) = case H.lookup a m of
  Just (Must,b) -> M.Just b
  Just (May,b)  -> M.JustNothing b
  Nothing       -> M.Nothing

insert :: Identifiable a => a -> b -> Store a b -> Store a b
insert a b (Store m) = Store (H.insert a (Must,b) m)

insertWith :: (Identifiable a,Complete b) => (b -> b -> b) -> a -> b -> Store a b -> Store a b
insertWith f a b (Store m) = Store (H.insertWith (\(_,new) (here,old) -> case here of
    Must -> (Must,f new old)
    May  -> (Must,new ⊔ f new old)
  ) a (Must,b) m)

delete :: Identifiable a => a -> Store a b -> Store a b
delete a (Store m) = Store (H.delete a m)

union :: (Identifiable a, Complete b) => Store a b -> Store a b -> Store a b
union (Store m1) (Store m2) = Store (H.unionWith (\(here,l) (_,r) -> case here of
    Must -> (Must,l)
    May  -> (May,l ⊔ r)
  ) m1 m2)

adjust :: Identifiable a => (b -> b) -> a -> Store a b -> Store a b
adjust f a (Store m) = Store (H.adjust (second f) a m)

compose :: (Identifiable a, Identifiable b, Complete c) => [(a,b)] -> Store b c -> Store a c
compose f (Store g) = Store $ H.fromListWith (⊔) [ (a,c) | (a,b) <- f, Just c <- return $ H.lookup b g ]

widening :: Identifiable a => Widening b -> Widening (Store a b)
widening w (Store m1) (Store m2) = Store $ H.map join $ H.unionWith (E.widening (finite ** w) (finite ** w)) (H.map Left m1) (H.map Right m2)
    where
      join e = case e of
        Left (_,a)              -> (May,a)
        Right (_,b)             -> (May,b)
        LeftRight (t1,a) (t2,b) -> (t1 ⊔ t2,a `w` b)

instance Identifiable a => IsList (Store a b) where
  type Item (Store a b) = (a,b)
  toList (Store m) = H.toList (H.map snd m)
  fromList l = Store (H.fromList (fmap (second (Must,)) l))

instance Show There where
  show Must = ""
  show May  = "?"

instance PreOrd There where
  Must ⊑ May = True
  Must ⊑ Must = True
  May ⊑ May = True
  _ ⊑ _ = False

instance Complete There where
  Must ⊔ Must = Must
  _ ⊔ _ = May

instance Hashable There where
  hashWithSalt s Must = s `hashWithSalt` (1::Int)
  hashWithSalt s May  = s `hashWithSalt` (2::Int)
