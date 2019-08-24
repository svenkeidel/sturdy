{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
-- | Abstract domain for environments in which concrete environments
-- are approximated by a mapping from variables to addresses and a
-- mapping from addresses to values. The number of allocated addresses
-- allows to tune the precision and performance of the analysis.
--
-- Furthermore, closures and environments are defined mutually
-- recursively. By only allowing a finite number of addresses, the
-- abstract domain of closures and environments becomes finite.
module Control.Arrow.Transformer.Abstract.BoundedEnvironment(EnvT,runEnvT) where

import           Prelude hiding ((.),id)

import           Control.Arrow
import           Control.Arrow.Environment
import           Control.Arrow.Fail
import           Control.Arrow.Except
import           Control.Arrow.Fix
import           Control.Arrow.Trans
import           Control.Arrow.Reader as Reader
import           Control.Arrow.Order
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Static
import           Control.Arrow.Transformer.Reader
import           Control.Category

import           Data.Order (Complete(..))
import           Data.Identifiable
import qualified Data.HashMap.Lazy as HM
import           Data.Profunctor
import           Data.Profunctor.Unsafe((.#))
import           Data.Coerce

type Env var addr val = (HM.HashMap var addr,HM.HashMap addr val)
type Alloc c var addr val = c (var,val,Env var addr val) addr

newtype EnvT var addr val c x y = EnvT (ConstT (Alloc c var addr val) (ReaderT (Env var addr val) c) x y )
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowFail e, ArrowComplete z, ArrowLowerBounded, ArrowTrans)

deriving instance ArrowExcept e c => ArrowExcept e (EnvT var addr val c)

runEnvT :: Alloc c var addr val -> EnvT var addr val c x y -> c (Env var addr val,x) y
runEnvT alloc (EnvT f) = runReaderT (runConstT alloc f)

instance (Identifiable var, Identifiable addr, Complete val, ArrowChoice c, Profunctor c) =>
  ArrowEnv var val (EnvT var addr val c) where
  type Join y (EnvT var addr val c) = ()
  lookup (EnvT f) (EnvT g) = EnvT $ proc (var,x) -> do
    (env,store) <- Reader.ask -< ()
    case do { addr <- HM.lookup var env; HM.lookup addr store } of
      Just val -> f -< (val,x)
      Nothing  -> g -< x
  extend (EnvT f) = EnvT $ ConstT $ StaticT $ \alloc -> ReaderT $ proc ((env,store),(var,val,x)) -> do
    addr <- alloc -< (var,val,(env,store))
    let env'   = HM.insert var addr env
        store' = HM.insertWith (⊔) addr val store
    runReaderT (runConstT alloc f) -< ((env',store'),x)

instance (Identifiable var, Identifiable addr, Complete val, ArrowChoice c, Profunctor c) =>
  ArrowClosure var val (HM.HashMap var addr) (EnvT var addr val c) where
  ask = EnvT (rmap fst Reader.ask)
  local (EnvT f) = EnvT $ proc (env,x) -> do
    (_,store) <- Reader.ask -< ()
    Reader.local f -< ((env,store),x)

instance (ArrowChoice c, ArrowRun c) => ArrowRun (EnvT var addr val c) where
  type Run (EnvT var addr val c) x y = Alloc c var addr val -> Run c (Env var addr val,x) y
  run f alloc = run (runEnvT alloc f)

instance ArrowLift (EnvT var addr val) where
  lift' f = EnvT (lift' (lift' f))

instance ArrowReader r c => ArrowReader r (EnvT var addr val c) where
  ask = lift' Reader.ask
  local (EnvT (ConstT (StaticT f))) =
    EnvT $ ConstT $ StaticT $ \alloc -> ReaderT $ (\(env,(r,x)) -> (r,(env,x))) ^>> Reader.local (runReaderT (f alloc))

instance (ArrowApply c, Profunctor c) => ArrowApply (EnvT var addr val c) where
  app = EnvT (app .# first coerce)

type instance Fix (EnvT var addr val c) x y = EnvT var addr val (Fix c (Env var addr val,x) y)
deriving instance (Arrow c, Profunctor c, ArrowFix (c (Env var addr val,x) y)) => ArrowFix (EnvT var addr val c x y)
