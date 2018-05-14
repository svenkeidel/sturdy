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
newtype Environment var val c x y = Environment (Reader (Env var val) c x y)

runEnvironment :: (Arrow c) => Environment var val c x y -> c (Env var val,x) y
runEnvironment (Environment (Reader f)) = f

runEnvironment' :: (Arrow c, Identifiable var) => Environment var val c x y -> c ([(var,val)],x) y
runEnvironment' f = first E.fromList ^>> runEnvironment f

instance (Identifiable var, ArrowChoice c) => ArrowEnv var val (Env var val) (Environment var val c) where
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

deriving instance ArrowTry (Env var val,x) (Env var val,y) z c => ArrowTry x y z (Environment var val c)
deriving instance ArrowError (Env var val,e) (Env var val,x) y c => ArrowError e x y (Environment var val c)

deriving instance Arrow c => Category (Environment var val c)
deriving instance Arrow c => Arrow (Environment var val c)
deriving instance ArrowLift (Environment var val)
deriving instance ArrowChoice c => ArrowChoice (Environment var val c)
deriving instance ArrowStore var val lab c => ArrowStore var val lab (Environment var' val' c)
deriving instance ArrowState s c => ArrowState s (Environment var val c)
deriving instance ArrowFail e c => ArrowFail e (Environment var val c)
deriving instance ArrowExcept (Env var val,x) y e c => ArrowExcept x y e (Environment var val c)
deriving instance ArrowConst r c => ArrowConst r (Environment var val c)
deriving instance ArrowRead x y  (Env var val,u) v c => ArrowRead x y u v (Environment var val c)
deriving instance ArrowWrite x y c => ArrowWrite x y (Environment var val c)

type instance Fix x y (Environment var val c) = Environment var val (Fix (Env var val,x) y c)
deriving instance ArrowFix (Env var val,x) y c => ArrowFix x y (Environment var val c)
