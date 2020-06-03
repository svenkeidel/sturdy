{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Concrete.TermEnvironment where

import           Prelude hiding ((.))

import           Syntax (TermVar)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Arrow.Transformer.Concrete.Store
import           Control.Arrow.Fix
import           Control.Arrow.Fail
import           Control.Arrow.Except
import           Control.Arrow.Trans
import           Control.Arrow.Const

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Profunctor
import           Data.Coerce

import           TermEnv

type TermEnv t = HashMap TermVar t

newtype EnvT t c x y = EnvT (StoreT TermVar t c x y)
  deriving (Category,Profunctor,Arrow,ArrowChoice,ArrowRun,ArrowTrans,ArrowTrans,ArrowFail e,ArrowExcept e,ArrowConst r, ArrowStore TermVar t, ArrowState (TermEnv t))

instance (Profunctor c, ArrowChoice c) => IsTermEnv (TermEnv term) term (EnvT term c) where
  type Join x (EnvT term c) = ()
  deleteTermVars = EnvT $ modify' (\(vars,env) -> ((), delete vars env))
  unionTermEnvs = EnvT $ modify' (\((vars,oldEnv),newEnv) -> ((), delete vars newEnv `M.union` oldEnv))
  {-# INLINE deleteTermVars #-}
  {-# INLINE unionTermEnvs #-}

delete :: [TermVar] -> TermEnv term -> TermEnv term
delete vars env = foldr M.delete env vars
{-# INLINE delete #-}

type instance Fix (EnvT t c) x y = EnvT t (Fix c (TermEnv t, x) (TermEnv t, y))
deriving instance ArrowFix (Underlying (EnvT t c) x y) => ArrowFix (EnvT t c x y)
instance (Profunctor c, ArrowApply c) => ArrowApply (EnvT t c) where
  app = EnvT $ lmap (first coerce) app
