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

import           Control.Category

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Reader as Reader
import           Control.Arrow.Store
import           Control.Arrow.State
import           Control.Arrow.Fail
import           Control.Arrow.Trans
import           Control.Arrow.Except
import           Control.Arrow.Environment as Env
import           Control.Arrow.Fix

import           Data.Identifiable
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Profunctor
import           Data.Profunctor.Unsafe
import           Data.Coerce

-- | Arrow transformer that adds an environment to a computation.
newtype EnvT var val c x y = EnvT (ReaderT (HashMap var val) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowLift,ArrowTrans,
            ArrowFail e,ArrowExcept e,ArrowState s,ArrowConst r,
            ArrowStore var' val', ArrowRun)

runEnvT :: EnvT var val c x y -> c (HashMap var val,x) y
runEnvT = coerce
{-# INLINE runEnvT #-}

runEnvT' :: (Profunctor c, Identifiable var) => EnvT var val c x y -> c ([(var,val)],x) y
runEnvT' f = lmap (first M.fromList) (runEnvT f)

instance (Identifiable var, ArrowChoice c, Profunctor c) => ArrowEnv var val (EnvT var val c) where
  type Join (EnvT var val c) x y = ()
  lookup (EnvT f) (EnvT g) = EnvT $ proc (var,x) -> do
    env <- Reader.ask -< ()
    case M.lookup var env of
      Just val -> f -< (val,x)
      Nothing -> g -< x
  extend (EnvT f) = EnvT $ proc (var,val,x) -> do
    env <- Reader.ask -< ()
    Reader.local f -< (M.insert var val env, x)

instance (Identifiable var, ArrowChoice c, Profunctor c) => ArrowClosure var val (HashMap var val) (EnvT var val c) where
  ask = EnvT Reader.ask
  local (EnvT f) = EnvT (Reader.local f)

instance (ArrowApply c,Profunctor c) => ArrowApply (EnvT var val c) where
  app = EnvT $ app .# first coerce

instance ArrowReader r c => ArrowReader r (EnvT var val c) where
  ask = lift' Reader.ask
  local (EnvT (ReaderT f)) = EnvT (ReaderT (lmap (\(env,(r,x)) -> (r,(env,x))) (Reader.local f)))

deriving instance ArrowFix (HashMap var val,x) y c => ArrowFix x y (EnvT var val c)
