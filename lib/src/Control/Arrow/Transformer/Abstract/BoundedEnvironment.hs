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
import           Control.Arrow.State as State
import           Control.Arrow.Order
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Static
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Category

import           Data.Order (Complete(..))
import           Data.Identifiable
import           Data.HashMap.Lazy as HM
import           Data.Profunctor
import           Data.Profunctor.Unsafe((.#))
import           Data.Coerce

type Env var addr val = (HM.HashMap var addr,HM.HashMap addr val)

newtype EnvT var addr val c x y = EnvT (ConstT (c (var,val,Env var addr val) addr) (ReaderT (HM.HashMap var addr) (StateT (HM.HashMap addr val) c)) x y )
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowFail e, ArrowLowerBounded)

deriving instance ArrowExcept e c => ArrowExcept e (EnvT var addr val c)

runEnvT :: (Identifiable var, Identifiable addr, Complete val, ArrowChoice c, Profunctor c)
               => c (var,val,Env var addr val) addr -> EnvT var addr val c x y -> c x (HM.HashMap addr val,y)
runEnvT alloc (EnvT f) =
  lmap (\x -> (HM.empty,(HM.empty,x)))
          (runStateT (runReaderT (runConstT alloc f)))

instance (Identifiable var, Identifiable addr, Complete val, ArrowChoice c, Profunctor c) =>
  ArrowEnv var val (EnvT var addr val c) where
  type Join y (EnvT var addr val c) = ()
  lookup (EnvT f) (EnvT g) = EnvT $ proc (var,x) -> do
    env <- Reader.ask -< ()
    store <- State.get -< ()
    case do { addr <- HM.lookup var env; HM.lookup addr store } of
      Just val -> f -< (val,x)
      Nothing  -> g -< x
  extend (EnvT f) = EnvT $ ConstT $ StaticT $ \alloc -> ReaderT $ StateT $ proc (store,(env,(var,val,x))) -> do
    addr <- alloc -< (var,val,(env,store))
    let env' = HM.insert var addr env
        store' = HM.insertWith (⊔) addr val store
    runStateT (runReaderT (runConstT alloc f)) -< (store',(env',x))

instance (Identifiable var, Identifiable addr, Complete val, ArrowChoice c, Profunctor c) =>
  ArrowClosure var val (HM.HashMap var addr) (EnvT var addr val c) where
  ask = EnvT Reader.ask
  local (EnvT f) = EnvT $ Reader.local f

instance (Identifiable var, Identifiable addr, Complete val, ArrowChoice c, ArrowRun c) => ArrowRun (EnvT var addr val c) where
  type Rep (EnvT var addr val c) x y = c (var,val,Env var addr val) addr -> Rep c x (HM.HashMap addr val,y)
  run f alloc = run (runEnvT alloc f)

instance ArrowTrans (EnvT var addr val) where
  type Dom (EnvT var addr val) x y = (HM.HashMap addr val,(HM.HashMap var addr,x))
  type Cod (EnvT var addr val) x y = (HM.HashMap addr val,y)
  lift = undefined
  unlift = undefined
      
instance ArrowLift (EnvT var addr val) where
  lift' f = EnvT (lift' (lift' (lift' f)))

instance ArrowReader r c => ArrowReader r (EnvT var addr val c) where
  ask = lift' Reader.ask
  local (EnvT (ConstT (StaticT f))) =
    EnvT $ ConstT $ StaticT $ \alloc -> ReaderT $ (\(env,(r,x)) -> (r,(env,x))) ^>> Reader.local (runReaderT (f alloc))

instance (ArrowApply c, Profunctor c) => ArrowApply (EnvT var addr val c) where
  app = EnvT (app .# first coerce)

deriving instance ArrowFix (Dom (EnvT var addr val) x y) (Cod (EnvT var addr val) x y) c => ArrowFix x y (EnvT var addr val c)

type instance Fix x y (EnvT var addr val c) = EnvT var addr val (Fix (Dom (EnvT var addr val) x y) (Cod (EnvT var addr val) x y) c)
