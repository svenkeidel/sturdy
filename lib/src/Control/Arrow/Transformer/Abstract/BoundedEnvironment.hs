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
module Control.Arrow.Transformer.Abstract.BoundedEnvironment(Environment,runEnvironment) where

import           Control.Arrow
import           Control.Arrow.Environment
import           Control.Arrow.Fail
import           Control.Arrow.Except
import           Control.Arrow.Fix
import           Control.Arrow.Lift
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Static
import           Control.Arrow.Transformer.Reader
import           Control.Category
import           Prelude hiding ((.),id)

import           Data.Order
import           Data.Identifiable
import           Data.Abstract.FiniteMap (Map)
import qualified Data.Abstract.FiniteMap as M

-- | Abstract domain for environments in which concrete environments
-- are approximated by a mapping from variables to addresses and a
-- mapping from addresses to values. The number of allocated addresses
-- allows to tune the precision and performance of the analysis.
--
-- Furthermore, closures and environments are defined mutually
-- recursively. By only allowing a finite number of addresses, the
-- abstract domain of closures and environments becomes finite.
newtype Environment var addr val c x y =
  Environment ( Const (c (var,val,Map var addr val) addr) (Reader (Map var addr val) c) x y )

runEnvironment :: (Show var, Identifiable var, Identifiable addr, Complete val, ArrowChoice c,
                   ArrowFail String c)
               => c (var,val,Map var addr val) addr -> Environment var addr val c x y -> c ([(var,val)],x) y
runEnvironment alloc f =
  let Environment f' = proc (bs,x) -> do
       env <- getEnv -< ()
       env' <- bindings -< (bs,env)
       localEnv f -< (env',x)
  in (const (M.empty) &&& id) ^>> runReader (runConst alloc f')

instance ArrowLift (Environment var addr val) where
  lift f = Environment (lift (lift f))

instance (Identifiable var, Identifiable addr, Complete val, ArrowChoice c) =>
  ArrowEnv var val (Map var addr val) (Environment var addr val c) where
  lookup (Environment f) (Environment g) = Environment $ proc (var,x) -> do
    env <- ask -< ()
    case do M.lookup var env of
      Just val -> f -< (val,x)
      Nothing  -> g -< x
  getEnv = Environment ask
  extendEnv = Environment $ Const $ Static $ \alloc -> lift $ M.insertBy alloc
  localEnv (Environment f) = Environment $ local f

instance ArrowReader r c => ArrowReader r (Environment var addr val c) where
  ask = lift ask
  local (Environment (Const (Static f))) =
    Environment $ Const $ Static $ \alloc -> Reader $ (\(env,(r,x)) -> (r,(env,x))) ^>> local (runReader (f alloc))

instance ArrowApply c => ArrowApply (Environment var addr val c) where
  app = Environment $ (\(Environment f,x) -> (f,x)) ^>> app

type instance Fix x y (Environment var addr val c) = Environment var addr val (Fix ((Map var addr val),x) y c)
deriving instance ArrowFix ((Map var addr val),x) y c => ArrowFix x y (Environment var addr val c)
deriving instance Arrow c => Category (Environment var addr val c)
deriving instance Arrow c => Arrow (Environment var addr val c)
deriving instance ArrowChoice c => ArrowChoice (Environment var addr val c)
deriving instance ArrowState s c => ArrowState s (Environment var addr val c)
deriving instance ArrowFail e c => ArrowFail e (Environment var addr val c)
deriving instance ArrowExcept ((Map var addr val),x) y e c => ArrowExcept x y e (Environment var addr val c)

deriving instance PreOrd (c ((Map var addr val),x) y) => PreOrd (Environment var addr val c x y)
deriving instance Complete (c ((Map var addr val),x) y) => Complete (Environment var addr val c x y)
deriving instance CoComplete (c ((Map var addr val),x) y) => CoComplete (Environment var addr val c x y)
deriving instance LowerBounded (c ((Map var addr val),x) y) => LowerBounded (Environment var addr val c x y)
deriving instance UpperBounded (c ((Map var addr val),x) y) => UpperBounded (Environment var addr val c x y)
