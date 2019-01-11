{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.Environment where

import           Prelude hiding ((.),read)

import           Data.Identifiable
import           Data.Concrete.Environment (Env)
import qualified Data.Concrete.Environment as E

import           Control.Category

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Reader
import           Control.Arrow.Store
import           Control.Arrow.State
import           Control.Arrow.Fail
import           Control.Arrow.Lift
import           Control.Arrow.Except
import           Control.Arrow.Environment
import           Control.Arrow.Fix

-- | Arrow transformer that adds an environment to a computation.
newtype EnvT var val c x y = EnvT (ReaderT (Env var val) c x y)

runEnvT :: (Arrow c) => EnvT var val c x y -> c (Env var val,x) y
runEnvT (EnvT (ReaderT f)) = f

runEnvT' :: (Arrow c, Identifiable var) => EnvT var val c x y -> c ([(var,val)],x) y
runEnvT' f = first E.fromList ^>> runEnvT f

instance (Identifiable var, ArrowChoice c) => ArrowEnv var val (Env var val) (EnvT var val c) where
  type Join (EnvT var val c) x y = ()
  lookup (EnvT f) (EnvT g) = EnvT $ proc (var,x) -> do
    env <- ask -< ()
    case E.lookup var env of
      Just val -> f -< (val,x)
      Nothing -> g -< x
  getEnv = EnvT ask
  extendEnv = arr $ \(x,y,env) -> E.insert x y env
  localEnv (EnvT f) = EnvT (local f)

instance ArrowApply c => ArrowApply (EnvT var val c) where
  app = EnvT $ (\(EnvT f,x) -> (f,x)) ^>> app

instance ArrowReader r c => ArrowReader r (EnvT var val c) where
  ask = lift ask
  local (EnvT (ReaderT f)) = EnvT (ReaderT ((\(env,(r,x)) -> (r,(env,x))) ^>> local f))

deriving instance Arrow c => Category (EnvT var val c)
deriving instance Arrow c => Arrow (EnvT var val c)
deriving instance ArrowLift (EnvT var val)
deriving instance ArrowChoice c => ArrowChoice (EnvT var val c)
deriving instance ArrowState s c => ArrowState s (EnvT var val c)
deriving instance ArrowFail e c => ArrowFail e (EnvT var val c)
deriving instance ArrowExcept e c => ArrowExcept e (EnvT var val c)
deriving instance ArrowConst r c => ArrowConst r (EnvT var val c)
deriving instance ArrowStore var val c => ArrowStore var val (EnvT var val c)

type instance Fix x y (EnvT var val c) = EnvT var val (Fix (Env var val,x) y c)
deriving instance ArrowFix (Env var val,x) y c => ArrowFix x y (EnvT var val c)
