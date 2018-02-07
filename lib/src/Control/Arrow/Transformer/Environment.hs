{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Transformer.Environment where

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import Data.Hashable
import Data.Order

import Control.Category
import Control.Arrow
import Control.Arrow.Transformer.Reader
import Control.Arrow.Class.Reader
import Control.Arrow.Class.State
import Control.Arrow.Class.Fail
import Control.Arrow.Class.Environment
import Control.Arrow.Class.Config

newtype Environment a b c x y = Environment (ReaderArrow (HashMap a b) c x y)
  deriving (Category,Arrow,ArrowChoice)

runEnvironment :: Environment a b c x y -> c (HashMap a b,x) y
runEnvironment (Environment (ReaderArrow f)) = f

liftEnv :: Arrow c => c x y -> Environment a b c x y
liftEnv f = Environment (liftReader f)

instance ArrowApply c => ArrowApply (Environment a b c) where
  app = Environment $ (\(Environment f,x) -> (f,x)) ^>> app

instance ArrowReader r c => ArrowReader r (Environment a b c) where
  askA = liftEnv askA
  localA (Environment (ReaderArrow f)) = Environment (ReaderArrow ((\(env,(r,x)) -> (r,(env,x))) ^>> localA f))

instance ArrowState s c => ArrowState s (Environment a b c) where
  getA = liftEnv getA
  putA = liftEnv putA

instance ArrowFail e c => ArrowFail e (Environment a b c) where
  failA = liftEnv failA

instance (Eq a, Hashable a, Arrow c) => ArrowEnv a b (HashMap a b) (Environment a b c) where
  lookup = proc x -> do
    env <- getEnv -< ()
    returnA -< H.lookup x env
  getEnv = Environment askA
  extendEnv = arr $ \(x,y,env) -> H.insert x y env
  localEnv (Environment f) = Environment (localA f)

instance (Eq a, Hashable a, ArrowConfig cIn cOut c) => ArrowConfig (HashMap a b,cIn) cOut (Environment a b c) where
  getInConfig = getEnv &&& liftEnv getInConfig
  getOutConfig = liftEnv getOutConfig
  setOutConfig = liftEnv setOutConfig


deriving instance PreOrd (c (HashMap a b,x) y) => PreOrd (Environment a b c x y)
deriving instance Complete (c (HashMap a b,x) y) => Complete (Environment a b c x y)
deriving instance CoComplete (c (HashMap a b,x) y) => CoComplete (Environment a b c x y)
deriving instance LowerBounded (c (HashMap a b,x) y) => LowerBounded (Environment a b c x y)
deriving instance UpperBounded (c (HashMap a b,x) y) => UpperBounded (Environment a b c x y)
