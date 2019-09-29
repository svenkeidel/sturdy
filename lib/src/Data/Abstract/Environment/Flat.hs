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

import           Control.Arrow.Closure

import           Data.Abstract.IntersectionSet (Set)
import qualified Data.Abstract.IntersectionSet as H
import qualified Data.Abstract.Maybe as A
import           Data.Abstract.Widening
import           Data.Abstract.Stable

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Empty
import           Data.Identifiable
import qualified Data.Scan as S
import           Data.Order
import           Data.Hashable

import           Text.Printf
import           GHC.Generics

data Env var val = Env { visible :: Set var, env :: HashMap var val } | Visible (Set var)
  deriving (Generic)

deriving instance (Eq var, Eq val) => Eq (Env var val)
instance (Hashable var, Hashable val) => Hashable (Env var val)

instance (Identifiable var, Show var,Show val) => Show (Env var val) where
  show (Visible vs) = show vs
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
mask _ _ = error "can only mask a top-level environment"

fromList :: (Identifiable var, Complete val, IsClosure val (Env var val)) => [(var,val)] -> Env var val
fromList ls = insertRec ls empty

lookup :: (Identifiable var, IsClosure val (Env var val)) => var -> Env var val -> A.Maybe val
lookup var Env {..} = case M.lookup var env of
  Just val | H.member var visible -> A.Just $ mapEnvironment (\case Visible vis -> Env vis env; e -> e) val
           | otherwise -> A.JustNothing $ mapEnvironment (\case Visible vis -> Env vis env; e -> e) val
  Nothing -> A.Nothing
lookup _ _ = error "can lookup in a top-level environment"

insert :: (Identifiable var, Complete val, IsClosure val (Env var val)) => var -> val -> Env var val -> Env var val
insert var val Env {..} =
  let S.Scan envs val' = traverseEnvironment (\case Env vis m -> S.Scan m (Visible vis); Visible vis -> S.Scan empty (Visible vis)) val
  in Env (H.insert var visible) (M.insertWith (⊔) var val' env <> envs)
insert _ _ _ = error "can only insert in a top-level environment"

insertNonRec :: (Identifiable var, Complete val, IsClosure val (Env var val)) => var -> val -> Env var val -> Env var val
insertNonRec var val Env {..} = Env (H.insert var visible) (M.insertWith (⊔) var (setEnvironment (Visible visible) val) env)
insertNonRec _ _ _ = error "can only insert in a top-level environment"

insertRec :: (Identifiable var, Complete val, IsClosure val (Env var val)) => [(var,val)] -> Env var val -> Env var val
insertRec ls Env {..} = Env vis (M.unionWith (⊔) (M.fromList [(var,setEnvironment (Visible vis) val) | (var,val) <- ls]) env)
  where
    vars = H.fromList [ v | (v,_) <- ls ]
    vis = vars `H.union` visible
insertRec _ _ = error "can only insert in a top-level environment"

delete :: Identifiable var => [var] -> Env var val -> Env var val
delete vars Env {..} = Env (foldr H.delete visible vars) env
delete _ _ = error "can only delete from a top-level environment"

instance (Identifiable var, PreOrd val) => PreOrd (Env var val) where
  Env vis1 m1 ⊑ Env vis2 m2 = vis1 ⊑ vis2 && m1 ⊑ m2
  Visible vis1 ⊑ Visible vis2 = vis1 ⊑ vis2
  Visible vis1 ⊑ Env vis2 _ = vis1 ⊑ vis2
  Env vis1 _ ⊑ Visible vis2 = vis1 ⊑ vis2

instance (Identifiable var, Complete val) => Complete (Env var val) where
  Env vis1 m1 ⊔ Env vis2 m2 = Env (vis1 ⊔ vis2) (m1 ⊔ m2)
  Visible vis1 ⊔ Visible vis2 = Visible (vis1 ⊔ vis2)
  Env vis1 m1 ⊔ Visible vis2 = Env (vis1 ⊔ vis2) m1
  Visible vis1 ⊔ Env vis2 m2 = Env (vis1 ⊔ vis2) m2

widening :: Identifiable var => Widening val -> Widening (Env var val)
widening widenVal (Env vis1 env1) (Env vis2 env2) =
  let (s,env) = sequenceA $ M.unionWith (\(_,v1) (_,v2) -> widenVal v1 v2) (M.map (Stable,) env1) (M.map (Stable,) env2)
      (s',vis) = H.widening vis1 vis2
  in (s⊔s',Env vis env)
widening _ (Visible vis1) (Env vis2 env2) = let (s,vis) = H.widening vis1 vis2 in (s,Env vis env2)
widening w e1@Env{} e2@Visible{} = widening w e2 e1
widening _ (Visible vis1) (Visible vis2) = Visible <$> H.widening vis1 vis2
