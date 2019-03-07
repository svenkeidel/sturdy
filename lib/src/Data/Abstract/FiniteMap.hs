{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Abstract.FiniteMap where

import           Prelude hiding (lookup,Either(..),id,(.),(**))

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H

import           Data.Identifiable
import           Data.Order
import           Data.Hashable
import           Data.Maybe (maybeToList)

import           Data.Abstract.There (There(..))
import qualified Data.Abstract.There as T
import qualified Data.Abstract.Maybe as M
import           Data.Abstract.Widening

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
  Map (e,s) ⊑ m2 = all (\(k,(t,v)) -> (case t of Must -> M.Just v; May -> M.JustNothing v) ⊑ lookup k m2)
    [ (a,b) | (a,addr) <- H.toList e, b <- maybeToList (H.lookup addr s) ]

instance (Identifiable x, Identifiable addr, Complete y) => Complete (Map x addr y) where
  (Map (e1,s1)) ⊔ (Map (e2,s2)) = Map (e,foldl (\m a -> let (a',b) = join a in H.insert a' b m) H.empty (H.keys e))
    where
      e = H.union e1 e2
      join k = case (H.lookup k e1,H.lookup k e2) of
        (Just a1,Just a2) -> (a1, (s1 H.! a1) ⊔ (s2 H.! a2))
        (Just a1,Nothing) -> (a1, (s1 H.! a1))
        (Nothing,Just a2) -> (a2, (s2 H.! a2))
        (Nothing,Nothing) -> error "cannot happen"

widening :: (Identifiable x,Identifiable addr) => Widening y -> Widening (Map x addr y)
widening w (Map (e1,s1)) (Map (e2,s2)) =
  let (stable,s') = foldl (\(s,m) a -> let (a',(s'',b)) = join a in (s ⊔ s'',H.insert a' b m)) (Stable, H.empty) (H.keys e)
  in (stable,(Map (e,s')))
  where
    e = H.union e1 e2
    join k = case (H.lookup k e1,H.lookup k e2) of
      (Just a1,Just a2) -> (a1, (T.widening ** w) (s1 H.! a1) (s2 H.! a2))
      (Just a1,Nothing) -> (a1, (Instable, s1 H.! a1))
      (Nothing,Just a2) -> (a2, (Instable, s2 H.! a2))
      (Nothing,Nothing) -> error "cannot happen"


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


toList :: (Identifiable x, Identifiable addr) => Map x addr y -> [(x,y)]
toList (Map (e,s)) = [ (a,b)| (a,addr) <- H.toList e, (_,b) <- maybeToList (H.lookup addr s) ]
