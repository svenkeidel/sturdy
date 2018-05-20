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
import           Control.Arrow.Store
import           Control.Arrow.Fix
import           Control.Arrow.Try

import           Text.Printf

newtype Environment var val c x y = Environment (Reader (Env var val) c x y)

runEnvironment :: (Arrow c, Eq var, Hashable var) => Environment var val c x y -> c ([(var,val)],x) y
runEnvironment (Environment (Reader f)) = first E.fromList ^>> f

runEnvironment' :: (Arrow c, Eq var, Hashable var) => Environment var val c x y -> c (Env var val,x) y
runEnvironment' (Environment (Reader f)) = f

instance (Show var, Identifiable var, ArrowChoice c, ArrowFail String c) => ArrowEnv var val (Env var val) (Environment var val c) where
  lookup = proc x -> do
    env <- getEnv -< ()
    case E.lookup x env of
      Success y -> returnA -< y
      Fail _ -> failA -< printf "Variable %s not bound" (show x)
  getEnv = Environment askA
  extendEnv = arr $ \(x,y,env) -> E.insert x y env
  localEnv (Environment f) = Environment (localA f)

instance ArrowApply c => ArrowApply (Environment var val c) where
  app = Environment $ (\(Environment f,x) -> (f,x)) ^>> app

instance ArrowReader r c => ArrowReader r (Environment var val c) where
  askA = lift askA
  localA (Environment (Reader f)) = Environment (Reader ((\(env,(r,x)) -> (r,(env,x))) ^>> localA f))

instance ArrowStore x y l c => ArrowStore x y l (Environment var val c) where
  read = lift read
  write = lift write

instance ArrowTry (Env var val,x) (Env var val,y) z c => ArrowTry x y z (Environment var val c) where
  tryA (Environment f) (Environment g) (Environment h) = Environment (tryA f g h)

deriving instance Arrow c => Category (Environment var val c)
deriving instance Arrow c => Arrow (Environment var val c)
deriving instance ArrowLift (Environment var val)
deriving instance ArrowChoice c => ArrowChoice (Environment var val c)
deriving instance ArrowState s c => ArrowState s (Environment var val c)
deriving instance ArrowFail e c => ArrowFail e (Environment var val c)

type instance Fix x y (Environment var val c) = Environment var val (Fix (Env var val,x) y c)
deriving instance ArrowFix (Env var val,x) y c => ArrowFix x y (Environment var val c)
