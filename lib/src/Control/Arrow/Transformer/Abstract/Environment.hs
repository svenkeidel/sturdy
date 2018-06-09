{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.Environment where

import Prelude hiding ((.))

import Data.Order
import Data.Identifiable
import Data.Abstract.Environment (Env)
import qualified Data.Abstract.Environment as E

import Control.Category
import Control.Arrow
import Control.Arrow.Transformer.Reader
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store
import Control.Arrow.Fail
import Control.Arrow.Except
import Control.Arrow.Lift
import Control.Arrow.Environment
import Control.Arrow.Fix

newtype Environment var val c x y = Environment (Reader (Env var val) c x y)

runEnvironment :: (Arrow c) => Environment var val c x y -> c (Env var val,x) y
runEnvironment (Environment (Reader f)) = f

runEnvironment' :: (Arrow c, Identifiable var) => Environment var val c x y -> c ([(var,val)],x) y
runEnvironment' f = first E.fromList ^>> runEnvironment f

instance (Show var, Identifiable var, ArrowChoice c) => ArrowEnv var val (Env var val) (Environment var val c) where
  lookup (Environment f) (Environment g) = Environment $ proc (var,x) -> do
    env <- ask -< ()
    case E.lookup var env of
      Just val -> f -< (val,x)
      Nothing -> g -< x
  getEnv = Environment ask
  extendEnv = arr $ \(x,y,env) -> E.insert x y env
  localEnv (Environment f) = Environment (local f)

instance ArrowApply c => ArrowApply (Environment var val c) where
  app = Environment $ (\(Environment f,x) -> (f,x)) ^>> app

instance ArrowReader r c => ArrowReader r (Environment var val c) where
  ask = lift ask
  local (Environment (Reader f)) = Environment (Reader ((\(env,(r,x)) -> (r,(env,x))) ^>> local f))

type instance Fix x y (Environment var val c) = Environment var val (Fix (Env var val,x) y c)

deriving instance ArrowFix (Env var val,x) y c => ArrowFix x y (Environment var val c)
deriving instance Arrow c => Category (Environment var val c)
deriving instance Arrow c => Arrow (Environment var val c)
deriving instance ArrowLift (Environment var val)
deriving instance ArrowChoice c => ArrowChoice (Environment var val c)
deriving instance ArrowState s c => ArrowState s (Environment var val c)
deriving instance ArrowFail e c => ArrowFail e (Environment var val c)
deriving instance ArrowExcept (Env var val,x) y e c => ArrowExcept x y e (Environment var val c)
deriving instance ArrowRead x y (Env var val,u) v c => ArrowRead x y u v (Environment var val c)
deriving instance ArrowWrite x y c => ArrowWrite x y (Environment var val c)

deriving instance PreOrd (c (Env var val,x) y) => PreOrd (Environment var val c x y)
deriving instance Complete (c (Env var val,x) y) => Complete (Environment var val c x y)
deriving instance CoComplete (c (Env var val,x) y) => CoComplete (Environment var val c x y)
deriving instance LowerBounded (c (Env var val,x) y) => LowerBounded (Environment var val c x y)
deriving instance UpperBounded (c (Env var val,x) y) => UpperBounded (Environment var val c x y)
