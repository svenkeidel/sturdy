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
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Static
import           Control.Arrow.Transformer.Reader
import           Control.Category

import           Data.Order
import           Data.Identifiable
import           Data.Abstract.FiniteMap (Map)
import qualified Data.Abstract.FiniteMap as M
import           Data.Abstract.Maybe(Maybe(..))

-- | Abstract domain for environments in which concrete environments
-- are approximated by a mapping from variables to addresses and a
-- mapping from addresses to values. The number of allocated addresses
-- allows to tune the precision and performance of the analysis.
--
-- Furthermore, closures and environments are defined mutually
-- recursively. By only allowing a finite number of addresses, the
-- abstract domain of closures and environments becomes finite.
newtype EnvT var addr val c x y =
  EnvT ( ConstT (c (var,val,Map var addr val) addr) (ReaderT (Map var addr val) c) x y )

runEnvT :: (Show var, Identifiable var, Identifiable addr, Complete val, ArrowChoice c)
               => c (var,val,Map var addr val) addr -> EnvT var addr val c x y -> c ([(var,val)],x) y
runEnvT alloc f =
  let EnvT f' = proc (bs,x) -> do
       env <- getEnv -< ()
       env' <- bindings -< (bs,env)
       localEnv f -< (env',x)
  in (const (M.empty) &&& id) ^>> runReaderT (runConstT alloc f')

instance ArrowTrans (EnvT var addr val) where
  type Dom (EnvT var addr val) x y = Dom (ReaderT (Map var addr val)) x y
  type Cod (EnvT var addr val) x y = Cod (ReaderT (Map var addr val)) x y
  lift = undefined
  unlift = undefined
      
instance ArrowLift (EnvT var addr val) where
  lift' f = EnvT (lift' (lift' f))

instance (Identifiable var, Identifiable addr, Complete val, ArrowChoice c) =>
  ArrowEnv var val (Map var addr val) (EnvT var addr val c) where
  type Join (EnvT var addr val c) x y = Complete (c (Map var addr val,x) y)
  lookup (EnvT f) (EnvT g) = EnvT $ proc (var,x) -> do
    env <- ask -< ()
    case do M.lookup var env of
      Just val        -> f          -< (val,x)
      JustNothing val -> joined f g -< ((val,x),x)
      Nothing         -> g          -< x
  getEnv = EnvT ask
  extendEnv = EnvT $ ConstT $ StaticT $ \alloc -> lift' $ M.insertBy alloc
  localEnv (EnvT f) = EnvT $ local f

instance ArrowReader r c => ArrowReader r (EnvT var addr val c) where
  ask = lift' ask
  local (EnvT (ConstT (StaticT f))) =
    EnvT $ ConstT $ StaticT $ \alloc -> ReaderT $ (\(env,(r,x)) -> (r,(env,x))) ^>> local (runReaderT (f alloc))

instance ArrowApply c => ArrowApply (EnvT var addr val c) where
  app = EnvT $ (\(EnvT f,x) -> (f,x)) ^>> app

type instance Fix x y (EnvT var addr val c) = EnvT var addr val (Fix (Dom (EnvT var addr val) x y) (Cod (EnvT var addr val) x y) c)
deriving instance ArrowFix (Dom (EnvT var addr val) x y) (Cod (EnvT var addr val) x y) c => ArrowFix x y (EnvT var addr val c)
deriving instance Arrow c => Category (EnvT var addr val c)
deriving instance Arrow c => Arrow (EnvT var addr val c)
deriving instance ArrowChoice c => ArrowChoice (EnvT var addr val c)
deriving instance ArrowState s c => ArrowState s (EnvT var addr val c)
deriving instance ArrowFail e c => ArrowFail e (EnvT var addr val c)
deriving instance ArrowExcept e c => ArrowExcept e (EnvT var addr val c)

deriving instance PreOrd (c ((Map var addr val),x) y) => PreOrd (EnvT var addr val c x y)
deriving instance Complete (c ((Map var addr val),x) y) => Complete (EnvT var addr val c x y)
deriving instance CoComplete (c ((Map var addr val),x) y) => CoComplete (EnvT var addr val c x y)
deriving instance LowerBounded (c ((Map var addr val),x) y) => LowerBounded (EnvT var addr val c x y)
deriving instance UpperBounded (c ((Map var addr val),x) y) => UpperBounded (EnvT var addr val c x y)
