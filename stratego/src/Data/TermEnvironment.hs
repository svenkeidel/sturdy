{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.TermEnvironment where

import           Prelude hiding ((.))

import           Syntax
import qualified ConcreteSemantics as C
import           SharedSemantics

import           Control.Category
import           Control.Arrow
import           Control.Arrow.State
import           Control.Arrow.Transformer.State
import           Control.Arrow.Fix
import           Control.Arrow.Fail
import           Control.Arrow.Except
import           Control.Arrow.Trans
import           Control.Arrow.Deduplicate
import           Control.Arrow.Const
import           Control.Arrow.Abstract.Join

import           Data.Order
import qualified Data.Concrete.Powerset as C
import           Data.Abstract.FreeCompletion as Free
import           Data.Abstract.WeakMap (Map)
import qualified Data.Abstract.WeakMap as S
import qualified Data.Abstract.Maybe as A
import qualified Data.HashMap.Lazy as LM
import           Data.Profunctor
import           Data.Identifiable
import           Data.Coerce
import           Data.GaloisConnection

type TermEnv t = Map TermVar t

type instance Fix x y (EnvironmentT t c) = EnvironmentT t (Fix (Dom (EnvironmentT t) x y) (Cod (EnvironmentT t) x y) c)
newtype EnvironmentT t c x y = EnvironmentT (StateT (TermEnv t) c x y)
  deriving (Category,Profunctor,Arrow,ArrowChoice,ArrowJoin,ArrowTrans,ArrowLift,ArrowFail e,ArrowExcept e,ArrowConst r)

deriving instance ArrowFix (Dom (EnvironmentT t) x y) (Cod (EnvironmentT t) x y) c => ArrowFix x y (EnvironmentT t c)
deriving instance (Identifiable t, ArrowDeduplicate (Dom (EnvironmentT t) x y) (Cod (EnvironmentT t) x y) c) => ArrowDeduplicate x y (EnvironmentT t c)
deriving instance (PreOrd (c (Dom (EnvironmentT t) x y) (Cod (EnvironmentT t) x y))) => PreOrd (EnvironmentT t c x y)
deriving instance (LowerBounded (c (Dom (EnvironmentT t) x y) (Cod (EnvironmentT t) x y))) => LowerBounded (EnvironmentT t c x y)
deriving instance (Complete (c (Dom (EnvironmentT t) x y) (Cod (EnvironmentT t) x y))) => Complete (EnvironmentT t c x y)

instance (Profunctor c, ArrowApply c) => ArrowApply (EnvironmentT t c) where
  app = EnvironmentT $ lmap (first coerce) app

runEnvironmentT :: EnvironmentT t c x y -> c (TermEnv t,x) (TermEnv t,y)
runEnvironmentT (EnvironmentT (StateT f)) = f

instance Complete t => Complete (FreeCompletion (TermEnv t)) where
  Lower x ⊔ Lower y = Lower (x ⊔ y)
  _ ⊔ _ = Free.Top

instance (Complete t, ArrowChoice c, ArrowJoin c, ArrowTop t (EnvironmentT t c))
  => IsTermEnv (TermEnv t) t (EnvironmentT t c) where
  getTermEnv = EnvironmentT get
  putTermEnv = EnvironmentT put
  emptyTermEnv = EnvironmentT $ lmap (\() -> S.empty) put
  lookupTermVar f g = proc (v,env,ex) -> do
    topTerm <- topA -< ()
    case S.lookup v topTerm env of
      A.Just t        -> f -< t
      A.Nothing       -> g -< ex
      A.JustNothing t -> (f -< t) <⊔> (g -< ex)
  insertTerm = arr $ \(v,t,env) -> S.insert v t env
  deleteTermVars = arr $ \(vars,env) -> S.delete' vars env
  unionTermEnvs = arr (\(vars,e1,e2) -> S.union e1 (S.delete' vars e2))

instance (Galois (C.Pow C.Term) t, Complete t) => Galois (C.Pow C.TermEnv) (TermEnv t) where
  alpha = lub . fmap (\(C.TermEnv e) -> S.fromList (LM.toList (fmap alphaSing e)))
  gamma = undefined
