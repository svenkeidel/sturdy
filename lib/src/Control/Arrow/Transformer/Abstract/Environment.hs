{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.Environment where

import Prelude hiding ((.),read,Maybe(..))

import Control.Category
import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Transformer.Reader
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store
import Control.Arrow.Fail
import Control.Arrow.Except
import Control.Arrow.Trans
import Control.Arrow.Environment
import Control.Arrow.Fix
import Control.Arrow.Order

import Data.Order(UpperBounded, Complete)
import Data.Identifiable
import Data.Abstract.Maybe
import Data.Abstract.StrongMap (Map)
import qualified Data.Abstract.StrongMap as M
import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce

newtype EnvT var val c x y = EnvT (ReaderT (Map var val) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowLift,ArrowLowerBounded, ArrowComplete,
            ArrowState s, ArrowFail e, ArrowExcept e, ArrowStore var' val', ArrowConst k, ArrowRun)

runEnvT :: (Arrow c, Profunctor c) => EnvT var val c x y -> c (Map var val,x) y
runEnvT = coerce
{-# INLINE runEnvT #-}

runEnvT' :: (Arrow c, Profunctor c, Identifiable var) => EnvT var val c x y -> c ([(var,val)],x) y
runEnvT' f = lmap (first M.fromList) (runEnvT f)
{-# INLINE runEnvT' #-}

instance (Identifiable var, UpperBounded val, ArrowChoice c, ArrowComplete c, Profunctor c) => ArrowEnv var val (Map var val) (EnvT var val c) where
  type Join (EnvT var val c) x y = Complete y
  lookup (EnvT f) (EnvT g) = EnvT $ proc (var,x) -> do
    env <- ask -< ()
    case M.lookup' var env of
      Just val        -> f -< (val,x)
      Nothing         -> g -< x
      JustNothing val -> (f -< (val,x)) <⊔> (g -< x)
  getEnv = EnvT ask
  extendEnv = arr $ \(x,y,env) -> M.insert x y env
  localEnv (EnvT f) = EnvT (local f)

instance (ArrowApply c, Profunctor c) => ArrowApply (EnvT var val c) where
  app = EnvT (app .# first coerce)

instance ArrowReader r c => ArrowReader r (EnvT var val c) where
  ask = lift' ask
  local f = lift $ lmap (\(env,(r,x)) -> (r,(env,x))) (local (unlift f))

type instance Fix x y (EnvT var val c) = EnvT var val (Fix (Dom (EnvT var val) x y) (Cod (EnvT var val) x y) c)
deriving instance ArrowFix (Map var val,x) y c => ArrowFix x y (EnvT var val c)
