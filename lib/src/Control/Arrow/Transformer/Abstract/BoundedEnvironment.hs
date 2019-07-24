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
module Control.Arrow.Transformer.Abstract.BoundedEnvironment(EnvT,runEnvT) where

import           Prelude hiding ((.),id,Maybe(..))

import           Control.Arrow
import           Control.Arrow.Environment
import           Control.Arrow.Fail
import           Control.Arrow.Except
import           Control.Arrow.Fix
import           Control.Arrow.Trans
import           Control.Arrow.Reader as Reader
import           Control.Arrow.State
import           Control.Arrow.Order
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Static
import           Control.Arrow.Transformer.Reader
import           Control.Category

import           Data.Order (Complete)
import           Data.Identifiable
import           Data.Abstract.FiniteMap (Map)
import qualified Data.Abstract.FiniteMap as M
import           Data.Abstract.Maybe(Maybe(..))
import           Data.Profunctor
import           Data.Profunctor.Unsafe((.#))
import           Data.Coerce

-- | Abstract domain for environments in which concrete environments
-- are approximated by a mapping from variables to addresses and a
-- mapping from addresses to values. The number of allocated addresses
-- allows to tune the precision and performance of the analysis.
--
-- Furthermore, closures and environments are defined mutually
-- recursively. By only allowing a finite number of addresses, the
-- abstract domain of closures and environments becomes finite.
newtype EnvT var addr val c x y = EnvT ( ConstT (c (var,val,Map var addr val) addr) (ReaderT (Map var addr val) c) x y )
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowState s, ArrowFail e, ArrowExcept e, ArrowLowerBounded, ArrowComplete)

runEnvT :: (Identifiable var, Identifiable addr, Complete val, ArrowComplete c, ArrowChoice c, Profunctor c)
               => c (var,val,Map var addr val) addr -> EnvT var addr val c x y -> c ([(var,val)],x) y
runEnvT alloc f =
  let EnvT f' = proc (bs,x) -> do
       extend' f -< (bs,x)
  in (const (M.empty) &&& id) ^>> runReaderT (runConstT alloc f')

instance (Identifiable var, Identifiable addr, Complete val, ArrowComplete c, ArrowChoice c, ArrowRun c) => ArrowRun (EnvT var addr val c) where
  type Rep (EnvT var addr val c) x y = c (var,val,Map var addr val) addr -> Rep c ([(var,val)],x) y
  run f alloc = run (runEnvT alloc f)

instance ArrowTrans (EnvT var addr val) where
  type Dom (EnvT var addr val) x y = Dom (ReaderT (Map var addr val)) x y
  type Cod (EnvT var addr val) x y = Cod (ReaderT (Map var addr val)) x y
  lift = undefined
  unlift = undefined
      
instance ArrowLift (EnvT var addr val) where
  lift' f = EnvT (lift' (lift' f))

instance (Identifiable var, Identifiable addr, Complete val, ArrowChoice c, ArrowComplete c, Profunctor c) =>
  ArrowEnv var val (EnvT var addr val c) where
  type Join (EnvT var addr val c) x y = (Complete y)
  lookup (EnvT f) (EnvT g) = EnvT $ proc (var,x) -> do
    env <- Reader.ask -< ()
    case do M.lookup var env of
      Just val        -> f -< (val,x)
      Nothing         -> g -< x
      JustNothing val -> (f -< (val,x)) <⊔> (g -< x)
  extend (EnvT f) = EnvT $ ConstT $ StaticT $ \alloc -> ReaderT $ proc (env,(var,val,x)) -> do
    env' <- M.insertBy alloc -< (var,val,env)
    runReaderT (runConstT alloc f) -< (env',x)

instance (Identifiable var, Identifiable addr, Complete val, ArrowChoice c, ArrowComplete c, Profunctor c) =>
  ArrowClosure var val (Map var addr val) (EnvT var addr val c) where
  ask = EnvT Reader.ask
  local (EnvT f) = EnvT $ Reader.local f

instance ArrowReader r c => ArrowReader r (EnvT var addr val c) where
  ask = lift' Reader.ask
  local (EnvT (ConstT (StaticT f))) =
    EnvT $ ConstT $ StaticT $ \alloc -> ReaderT $ (\(env,(r,x)) -> (r,(env,x))) ^>> Reader.local (runReaderT (f alloc))

instance (ArrowApply c, Profunctor c) => ArrowApply (EnvT var addr val c) where
  app = EnvT (app .# first coerce)

type instance Fix x y (EnvT var addr val c) = EnvT var addr val (Fix (Dom (EnvT var addr val) x y) (Cod (EnvT var addr val) x y) c)
deriving instance ArrowFix (Dom (EnvT var addr val) x y) (Cod (EnvT var addr val) x y) c => ArrowFix x y (EnvT var addr val c)
