{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.FiniteMap where

import           Prelude hiding (lookup)

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H

import           Data.Identifiable
import           Data.Order
import           Data.Hashable

import           Data.Abstract.There
import qualified Data.Abstract.Maybe as M

import           Control.Arrow
import           Control.Arrow.Alloc

import           Text.Printf


newtype Map x addr y = Map (HashMap x addr, HashMap addr (There,y)) deriving (Eq,Hashable)

instance (Show x,Show addr,Show y) => Show (Map x addr y) where
  show (Map (xa,ay)) = show (showMap xa,showMap ay)
    where
      showMap h
        | H.null h = "[]"
        | otherwise = "[" ++ init (unwords [ printf "%s -> %s," (show k) (show v) | (k,v) <- H.toList h]) ++ "]"


instance (Identifiable x, Identifiable addr, PreOrd y) => PreOrd (Map x addr y) where
  m1 ⊑ m2 = keys m1 == keys m2 && all (\k -> lookup k m1 ⊑ lookup k m2) (keys m1)
  m1 ≈ m2 = keys m1 == keys m2 && all (\k -> lookup k m1 ≈ lookup k m2) (keys m1)

empty :: Map x addr y
empty = (Map (H.empty,H.empty))

keys :: Map x addr y -> [x]
keys (Map (e,_)) = H.keys e

lookup :: (Identifiable x, Identifiable addr) => x -> Map x addr y -> M.Maybe y
lookup x (Map (e,s)) = case do {addr <- H.lookup x e; H.lookup addr s} of
  Just (Must,b) -> M.Just b
  Just (May,b) -> M.JustNothing b
  Nothing -> M.Nothing

unsafeLookup :: (Identifiable x, Identifiable addr) => x -> Map x addr y -> Maybe y
unsafeLookup x (Map (e,s)) = fmap snd $ do
  addr <- H.lookup x e
  H.lookup addr s

insertBy :: (Identifiable x, Identifiable addr, Complete y, ArrowChoice c)
         => c (x,y,Map x addr y) addr -> c (x,y,Map x addr y) (Map x addr y)
insertBy alloc' = proc (x,y,Map (e,s)) -> case H.lookup x e of
  Just addr -> returnA -< Map (e,insertJoin addr y s)
  Nothing -> do
    addr <- alloc' -< (x,y,Map (e,s))
    returnA -< Map (H.insert x addr e,insertJoin addr y s)
  where
    insertJoin a b m = H.insertWith (\(_,new) (_,old) -> (Must,new ⊔ old)) a (Must,b) m

insert :: (Identifiable x, Identifiable addr, Complete y, ArrowChoice c, ArrowAlloc (x,y,Map x addr y) addr c)
       => c (x,y,Map x addr y) (Map x addr y)
insert = insertBy alloc


