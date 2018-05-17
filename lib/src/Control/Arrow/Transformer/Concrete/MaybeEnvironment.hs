{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.MaybeEnvironment where

import           Prelude hiding ((.), read)

import           Data.Hashable
import           Data.Identifiable
import           Data.Concrete.Error
import           Data.Concrete.Environment (Env)
import qualified Data.Concrete.Environment as E

import           Control.Category

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.TryCatch
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.MaybeStore
import           Control.Arrow.Fail
import           Control.Arrow.Lift
import           Control.Arrow.Try
import           Control.Arrow.MaybeEnvironment
import           Control.Arrow.Fix

newtype MaybeEnvironment var val c x y = MaybeEnvironment (Reader (Env var val) c x y)

runMaybeEnvironment :: (Arrow c, Eq var, Hashable var) => MaybeEnvironment var val c x y -> c ([(var,val)],x) y
runMaybeEnvironment (MaybeEnvironment (Reader f)) = first E.fromList ^>> f

instance (Identifiable var, ArrowChoice c) =>
  ArrowMaybeEnv var val (Env var val) (MaybeEnvironment var val c) where
  lookup = proc x -> do
    env <- getEnv -< ()
    case E.lookup x env of
      Success y -> returnA -< Just y
      Fail _ -> returnA -< Nothing
  getEnv = MaybeEnvironment askA
  extendEnv = arr $ \(x,y,env) -> E.insert x y env
  localEnv (MaybeEnvironment f) = MaybeEnvironment (localA f)

instance ArrowApply c => ArrowApply (MaybeEnvironment var val c) where
  app = MaybeEnvironment $ (\(MaybeEnvironment f,x) -> (f,x)) ^>> app

instance ArrowConst r c => ArrowConst r (MaybeEnvironment var val c) where
  askConst = lift askConst

instance ArrowReader r c => ArrowReader r (MaybeEnvironment var val c) where
  askA = lift askA
  localA (MaybeEnvironment (Reader f)) = MaybeEnvironment (Reader ((\(env,(r,x)) -> (r,(env,x))) ^>> localA f))

instance ArrowMaybeStore var val c => ArrowMaybeStore var val (MaybeEnvironment var' val' c) where
  read = lift read
  write = lift write

deriving instance ArrowTry (Env var val,x) (Env var val,y) z c => ArrowTry x y z (MaybeEnvironment var val c)

deriving instance ArrowTryCatch (Env var val,e) (Env var val,x) (Env var val,y) (Env var val,z) c =>
  ArrowTryCatch e x y z (MaybeEnvironment var val c)

deriving instance Arrow c => Category (MaybeEnvironment var val c)
deriving instance Arrow c => Arrow (MaybeEnvironment var val c)
deriving instance ArrowLift (MaybeEnvironment var val)
deriving instance ArrowChoice c => ArrowChoice (MaybeEnvironment var val c)
deriving instance ArrowState s c => ArrowState s (MaybeEnvironment var val c)
deriving instance ArrowFail e c => ArrowFail e (MaybeEnvironment var val c)

type instance Fix x y (MaybeEnvironment var val c) = MaybeEnvironment var val (Fix (Env var val,x) y c)
deriving instance ArrowFix (Env var val,x) y c => ArrowFix x y (MaybeEnvironment var val c)
