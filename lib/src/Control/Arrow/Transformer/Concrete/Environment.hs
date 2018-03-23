{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Transformer.Concrete.Environment where

import           Prelude hiding ((.))

import           Data.Hashable
import           Data.Identifiable
import           Data.Concrete.Error
import           Data.Concrete.Environment (Env)
import qualified Data.Concrete.Environment as E

import           Control.Category

import           Control.Arrow
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Fail
import           Control.Arrow.Lift
import           Control.Arrow.Environment
import           Control.Arrow.Fix

import           Text.Printf

newtype Environment var val c x y = Environment (Reader (Env var val) c x y)

runEnvironment :: (Arrow c, Eq var, Hashable var) => Environment var val c x y -> c ([(var,val)],x) y
runEnvironment (Environment (Reader f)) = first E.fromList ^>> f

instance (Show var, Identifiable var, ArrowChoice c, ArrowFail String c) => ArrowEnv var val (Env var val) (Environment var val c) where
  lookup = proc x -> do
    env <- getEnv -< ()
    case E.lookup x env of
      Success y -> returnA -< y
      Fail _ -> failA -< printf "variable %s not found" (show x)
  getEnv = Environment askA
  extendEnv = arr $ \(x,y,env) -> E.insert x y env
  localEnv (Environment f) = Environment (localA f)

instance ArrowApply c => ArrowApply (Environment var val c) where
  app = Environment $ (\(Environment f,x) -> (f,x)) ^>> app

instance ArrowReader r c => ArrowReader r (Environment var val c) where
  askA = lift askA
  localA (Environment (Reader f)) = Environment (Reader ((\(env,(r,x)) -> (r,(env,x))) ^>> localA f))

deriving instance Arrow c => Category (Environment var val c)
deriving instance Arrow c => Arrow (Environment var val c)
deriving instance ArrowLift (Environment var val)
deriving instance ArrowChoice c => ArrowChoice (Environment var val c)
deriving instance ArrowState s c => ArrowState s (Environment var val c)
deriving instance ArrowFail e c => ArrowFail e (Environment var val c)
deriving instance ArrowFix (Env var val,x) y c => ArrowFix x y (Environment var val c)
