{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.Environment.Grammar
  ( Env
  , empty
  , fromList
  , fromList'
  , lookup
  , insert, insertList
  , insertRec, insertRecList
  , filter
  , union
  ) where

import           Prelude hiding (lookup,filter,pred)

import           Data.TreeGrammar (Grammar)
import qualified Data.TreeGrammar as G
import           Data.TreeGrammar.HashMap
import           Data.TreeGrammar.Terminal (Terminal)
import qualified Data.TreeGrammar.Terminal as T

import           Data.Hashable
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as H
import           Data.Identifiable
import           Data.Coerce
import           Data.Empty

newtype Env var val = Env (Grammar Int (HashMap var val))
  deriving (IsEmpty)

instance (Identifiable var, Terminal val, Semigroup (val Int), Eq (val Int)) => Eq (Env var val) where
  Env env1 == Env env2 = G.identical env1 env2 || env1 == env2

instance (Identifiable var, Terminal val, Semigroup (val Int)) => Hashable (Env var val) where
  hashWithSalt s (Env env) = G.hashTerminals s env

deriving instance (Show var, Show (val Int)) => Show (Env var val)

fromList :: (Identifiable var, Terminal val, Semigroup (val String), Semigroup (val Int)) => [(var, val ())] -> Env var val
fromList l = Env $ G.grammar "Init" [("Init", HashMap (M.fromList [ (x,T.map (const "Init") v)| (x,v) <- l]))] []

fromList' :: (Identifiable var, Terminal val) => [(var, val ())] -> Env var val -> Env var val
fromList' = updateEnv (\start _ -> start) (\_ _ -> [])

lookup :: (Identifiable var, Terminal val, Semigroup (val Int), Eq (val Int)) => var -> Env var val -> Maybe (val (Env var val))
lookup var (Env env) =
  let HashMap m = G.toSubterms env
  in T.map Env <$> M.lookup var m

insert :: (Identifiable var, Terminal val) => var -> (forall a. a -> val a)  -> Env var val -> Env var val
insert var val = insertList [(var,val ())]

insertList :: (Identifiable var, Terminal val) => [(var,val ())] -> Env var val -> Env var val
insertList = updateEnv (\start _ -> start) (\start _ -> [start])

insertRec :: (Identifiable var, Terminal val) => var -> (forall a. a -> val a) -> Env var val -> Env var val
insertRec var val = insertRecList [(var,val ())]

-- OPT: Cash largest non-terminal of the grammar to generate fresh non-terminals faster.
insertRecList :: (Identifiable var, Terminal val) => [(var,val ())] -> Env var val -> Env var val
insertRecList = updateEnv (\_ fresh -> fresh) (\start _ -> [start])

union :: (Identifiable var, Terminal val, Semigroup (val Int)) => Env var val -> Env var val -> Env var val
union (Env env1) (Env env2) = Env (G.union env1 env2)

updateEnv :: (Terminal val, Identifiable var) => (Int -> Int -> Int) -> (Int -> Int -> [Int]) -> [(var, val ())] -> Env var val -> Env var val
updateEnv _ _ [] env = env
updateEnv closureEnv epsilon ls (Env env@G.Grammar{..}) = Env env
  { G.start = fresh
  , G.productions = M.insert fresh (G.Rhs { G.cons = coerce (M.fromList [ (var,T.map (const (closureEnv start fresh)) val)
                                                                        | (var,val) <- ls ])
                                          , G.eps = H.fromList (epsilon start fresh) }) productions
  }
  where
    fresh = succ $ M.foldlWithKey' (\x y _ -> max x y) 0 productions
{-# INLINE updateEnv #-}

filter :: (var -> Bool) -> Env var val -> Env var val
filter pred (Env env@G.Grammar{..})= Env env
  { G.productions = M.map (\rhs -> rhs { G.cons = withHashMap (M.filterWithKey (\k _ -> pred k)) (G.cons rhs)}) productions
  }
