{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Data.Abstract.Environment.Flat
( Env
, empty
, fromList
, lookup
, insert
, insertNonRec
, insertRec
, delete
, mask
, widening
) where

import           Prelude hiding (lookup)

import qualified Data.Abstract.Maybe as A
import           Data.Abstract.Widening
import           Data.Abstract.Stable

import           Data.Abstract.IntersectionSet (Set)
import qualified Data.Abstract.IntersectionSet as H
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Empty
import           Data.Identifiable
import qualified Data.Scan as S
import           Data.Order
import           Data.Hashable

import           Text.Printf
import           GHC.Generics

data Env var val = Env { visible :: Set var, env :: HashMap var (val (Set var)) }
  deriving (Generic)

deriving instance (Eq var, Eq (val (Set var))) => Eq (Env var val)
instance (Hashable var, Hashable (val (Set var))) => Hashable (Env var val)

instance (Identifiable var, Show var,Show (val (Set var))) => Show (Env var val) where
  show Env {..}
    | M.null env = "[]"
    | otherwise = "[" ++ init (unwords
                         [ printf "%s %s %s," (show var) (if H.member var visible then "->" else "~>") (show val)
                         | (var,val) <- M.toList env])
                  ++ "]"

instance IsEmpty (Env var val) where
  empty = Env empty empty

mask :: Identifiable var => [var] -> Env var val -> Env var val
mask vis (Env _ m) = Env (H.fromList vis) m

fromList :: (Identifiable var, Complete (val (Set var)), Traversable val) => [(var,val env)] -> Env var val
fromList ls = insertRec ls empty

lookup :: (Identifiable var, Traversable val) => var -> Env var val -> A.Maybe (val (Env var val))
lookup var Env {..} = case M.lookup var env of
  Just val | H.member var visible -> A.Just $ fmap (\vis -> Env vis env) val
           | otherwise -> A.JustNothing $ fmap (\vis -> Env vis env) val
  Nothing -> A.Nothing

insert :: (Identifiable var, Complete (val (Set var)), Traversable val) => var -> val (Env var val) -> Env var val -> Env var val
insert var val Env {..} =
  let S.Scan envs val' = traverse (\case (Env vis e) -> S.Scan e vis) val
  in Env (H.insert var visible) (M.insertWith (⊔) var val' env <> envs)

insertNonRec :: (Identifiable var, Complete (val (Set var)), Traversable val) => var -> val env -> Env var val -> Env var val
insertNonRec var val Env {..} = Env (H.insert var visible) (M.insertWith (⊔) var (visible <$ val) env)

insertRec :: (Identifiable var, Complete (val (Set var)), Traversable val) => [(var,val env)] -> Env var val -> Env var val
insertRec ls Env{..} = Env vis (M.unionWith (⊔) (M.fromList [(var,vis <$ val) | (var,val) <- ls]) env)
  where
    vars = H.fromList [ v | (v,_) <- ls ]
    vis = vars `H.union` visible

delete :: Identifiable var => [var] -> Env var val -> Env var val
delete vars Env {..} = Env (foldr H.delete visible vars) env

instance (Identifiable var, PreOrd (val (Set var))) => PreOrd (Env var val) where
  Env vis1 m1 ⊑ Env vis2 m2 = vis1 ⊑ vis2 && m1 ⊑ m2

instance (Identifiable var, Complete (val (Set var))) => Complete (Env var val) where
  Env vis1 m1 ⊔ Env vis2 m2 = Env (vis1 ⊔ vis2) (m1 ⊔ m2)

widening :: Identifiable var => Widening (val (Set var)) -> Widening (Env var val)
widening widenVal (Env vis1 env1) (Env vis2 env2) =
  let (s,env) = sequenceA $ M.unionWith (\(_,v1) (_,v2) -> widenVal v1 v2) (M.map (Stable,) env1) (M.map (Stable,) env2)
      (s',vis) = setWiden vis1 vis2
  in (s⊔s',Env vis env)

setWiden :: Identifiable a => Widening (Set a)
setWiden s1 s2 =
  let s = s1 ⊔ s2
  in (if H.size s1 == H.size s && H.size s2 == H.size s then Stable else Unstable,s)
