{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Abstract.TermEnvironment where

import           Prelude hiding ((.))

import           Syntax

import           Control.Category
import           Control.Arrow
import           Control.Arrow.State
import           Control.Arrow.Store as Store
import           Control.Arrow.Fix
import           Control.Arrow.Fail
import           Control.Arrow.Except
import           Control.Arrow.Trans
import           Control.Arrow.Const
import           Control.Arrow.Order
import           Control.Arrow.Transformer.Abstract.Store

import           Data.Order
import           Data.Abstract.FreeCompletion as Free
import           Data.Abstract.Map (Map)
import qualified Data.Abstract.Map as S
import           Data.Profunctor
import           Data.Coerce

import           TermEnv

type TermEnv t = Map TermVar t

newtype EnvT t c x y = EnvT (StoreT (Map TermVar t) c x y)
  deriving (Category,Profunctor,Arrow,ArrowChoice,ArrowJoin,ArrowLift,ArrowTrans,ArrowRun,ArrowLowerBounded a,
            ArrowFail e,ArrowExcept e,ArrowConst r, ArrowState (TermEnv t), ArrowStore TermVar t)

instance (Complete t, ArrowChoice c, ArrowJoin c) => IsTermEnv (TermEnv t) t (EnvT t c) where
  type Join y (EnvT t c) = Store.Join y (StoreT (Map TermVar t) c)
  deleteTermVars = EnvT $ modify' $ \(vars,env) -> ((), S.delete' vars env)
  unionTermEnvs = EnvT $ modify' $ \((vars,oldEnv),newEnv) -> ((), S.delete' vars newEnv `union` oldEnv)
  {-# INLINE deleteTermVars #-}
  {-# INLINE unionTermEnvs #-}

union :: TermEnv t -> TermEnv t -> TermEnv t
union = S.unsafeUnion

instance ArrowFix (Underlying (EnvT t c) x y) => ArrowFix (EnvT t c x y) where
  type Fix (EnvT t c x y) = Fix (Underlying (EnvT t c) x y)

deriving instance ArrowComplete (TermEnv t, y) c => ArrowComplete y (EnvT t c)

instance (Profunctor c, ArrowApply c) => ArrowApply (EnvT t c) where
  app = EnvT $ lmap (first coerce) app

instance Complete t => Complete (FreeCompletion (TermEnv t)) where
  Lower x ⊔ Lower y = Lower (x ⊔ y)
  _ ⊔ _ = Free.Top

-- instance (Galois (C.Pow C.Term) t, Complete t) => Galois (C.Pow C.TermEnv) (TermEnv t) where
--   alpha = lub . fmap (\(C.TermEnv e) -> S.fromList (LM.toList (fmap alphaSing e)))
--   gamma = undefined
