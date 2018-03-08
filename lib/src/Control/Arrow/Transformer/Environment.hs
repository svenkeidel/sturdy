{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Transformer.Environment where

import Prelude hiding ((.))

import Data.Hashable
import Data.Order
import Data.Environment (Env)
import qualified Data.Environment as E

import Control.Category
import Control.Arrow
import Control.Arrow.Transformer.Reader
import Control.Arrow.Class.Reader
import Control.Arrow.Class.State
import Control.Arrow.Class.Fail
import Control.Arrow.Class.Environment
import Control.Arrow.Class.Fix

newtype Environment var val c x y = Environment (ReaderArrow (Env var val) c x y)
  deriving (Category,Arrow,ArrowChoice)

runEnvironment :: (Arrow c, Eq var, Hashable var) => Environment var val c x y -> c ([(var,val)],x) y
runEnvironment (Environment (ReaderArrow f)) = first E.fromList ^>> f

liftEnv :: Arrow c => c x y -> Environment var val c x y
liftEnv f = Environment (liftReader f)

instance ArrowApply c => ArrowApply (Environment var val c) where
  app = Environment $ (\(Environment f,x) -> (f,x)) ^>> app

instance ArrowReader r c => ArrowReader r (Environment var val c) where
  askA = liftEnv askA
  localA (Environment (ReaderArrow f)) = Environment (ReaderArrow ((\(env,(r,x)) -> (r,(env,x))) ^>> localA f))

instance ArrowState s c => ArrowState s (Environment var val c) where
  getA = liftEnv getA
  putA = liftEnv putA

instance ArrowFail e c => ArrowFail e (Environment var val c) where
  failA = liftEnv failA

instance (Eq var, Hashable var, ArrowChoice c) => ArrowEnv var val (Env var val) (Environment var val c) where
  lookup = proc x -> do
    env <- getEnv -< ()
    returnA -< E.lookup x env
  getEnv = Environment askA
  extendEnv = arr $ \(x,y,env) -> E.insert x y env
  localEnv (Environment f) = Environment (localA f)

instance (ArrowFix (Env var val,x) y c) => ArrowFix x y (Environment var val c) where
  fixA f = Environment (ReaderArrow (fixA (unlift . f . lift)))
    where
      lift = Environment . ReaderArrow
      unlift (Environment (ReaderArrow f')) = f'

deriving instance PreOrd (c (Env var val,x) y) => PreOrd (Environment var val c x y)
deriving instance Complete (c (Env var val,x) y) => Complete (Environment var val c x y)
deriving instance CoComplete (c (Env var val,x) y) => CoComplete (Environment var val c x y)
deriving instance LowerBounded (c (Env var val,x) y) => LowerBounded (Environment var val c x y)
deriving instance UpperBounded (c (Env var val,x) y) => UpperBounded (Environment var val c x y)
