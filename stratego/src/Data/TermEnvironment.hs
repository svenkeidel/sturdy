{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.TermEnvironment where

import           Syntax
import           SharedSemantics

import           Control.Category
import           Control.Arrow
import           Control.Arrow.State
import           Control.Arrow.Transformer.State

import           Control.Arrow.Abstract.Join

import           Data.Order
import           Data.Abstract.FreeCompletion as Free
import           Data.Abstract.WeakMap (Map)
import qualified Data.Abstract.WeakMap as S
import qualified Data.Abstract.Maybe as A
import           Data.Profunctor

type TermEnv t = Map TermVar t
  
newtype EnvironmentT t c x y = EnvironmentT (StateT (TermEnv t) c x y)
  deriving (Category,Profunctor,Arrow,ArrowChoice,ArrowJoin)

runEnvironmentT :: EnvironmentT t c x y -> c (TermEnv t,x) (TermEnv t,y)
runEnvironmentT (EnvironmentT (StateT f)) = f

instance Complete t => Complete (FreeCompletion (TermEnv t)) where
  Lower x ⊔ Lower y = Lower (x ⊔ y)
  _ ⊔ _ = Free.Top

instance (UpperBounded t, Complete t, ArrowChoice c, ArrowJoin c, ArrowState (TermEnv t) c) => IsTermEnv (TermEnv t) t (EnvironmentT t c) where
  getTermEnv = EnvironmentT get
  putTermEnv = EnvironmentT put
  emptyTermEnv = EnvironmentT $ lmap (\() -> S.empty) put
  lookupTermVar f g = proc (v,env,ex) -> do
    case S.lookup v top env of
      A.Just t        -> f -< t
      A.Nothing       -> g -< ex
      A.JustNothing t -> (f -< t) <⊔> (g -< ex)
  insertTerm = arr $ \(v,t,env) -> S.insert v t env
  deleteTermVars = arr $ \(vars,env) -> S.delete' vars env
  unionTermEnvs = arr (\(vars,e1,e2) -> S.union e1 (S.delete' vars e2))
