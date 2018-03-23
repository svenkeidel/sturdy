{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
module Control.Arrow.Transformer.Abstract.BoundedEnvironment(Environment,runEnvironment,liftEnvironment,ArrowAlloc(..)) where

import           Prelude hiding ((.),id)
import           Control.Category
import           Control.Arrow
import           Control.Arrow.Abstract.Alloc
import           Control.Arrow.Environment
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Transformer.Reader

import           Data.Order
import           Data.Identifiable
import           Data.Abstract.Error
import           Data.Abstract.Environment (Env)
import qualified Data.Abstract.Environment as E
import           Data.Abstract.Store (Store)
import qualified Data.Abstract.Store as S

import           Text.Printf

-- The galois connection for environment store pairs ensures that if
-- an variable is bound to an address in the environment, then the
-- address has an binding in the store.
newtype Environment var addr val c x y = Environment ( ReaderArrow (Env var addr,Store addr val) c x y )

runEnvironment :: (Show var, Identifiable var, Identifiable addr, Complete val, ArrowChoice c, ArrowFail String c, ArrowAlloc var addr val c,LowerBounded (c () val))
               => Environment var addr val c x y -> c ([(var,val)],x) y
runEnvironment f =
  let Environment (ReaderArrow f') = proc (bs,x) -> do
       env <- getEnv -< ()
       env' <- bindings -< (bs,env)
       localEnv f -< (env',x)
  in (const (E.empty,S.empty) &&& id) ^>> f'

liftEnvironment :: Arrow c => c x y -> Environment var addr val c x y
liftEnvironment f = Environment (liftReader f)

instance (Show var, Identifiable var, Identifiable addr, Complete val, ArrowChoice c, ArrowFail String c, ArrowAlloc var addr val c, LowerBounded (c () val)) =>
  ArrowEnv var val (Env var addr,Store addr val) (Environment var addr val c) where
  lookup = Environment $ ReaderArrow $ proc ((env,store),x) -> do
    case do {addr <- E.lookup x env; S.lookup addr store} of
      Success v -> returnA -< v
      Fail _ -> failA -< printf "variable %s not found" (show x)
      Bot -> bottom -< ()
  getEnv = Environment askA
  extendEnv = proc (x,y,(env,store)) -> do
    addr <- liftEnvironment alloc -< (x,env,store)
    returnA -< (E.insert x addr env,S.insertWith (⊔) addr y store)
  localEnv (Environment (ReaderArrow f)) = Environment (ReaderArrow ((\(_,(e,a)) -> (e,a)) ^>> f))

instance ArrowReader r c => ArrowReader r (Environment var addr val c) where
  askA = liftEnvironment askA
  localA (Environment (ReaderArrow f)) = Environment $ ReaderArrow $ (\(env,(r,x)) -> (r,(env,x))) ^>> localA f

instance ArrowApply c => ArrowApply (Environment var addr val c) where
  app = Environment $ (\(Environment f,x) -> (f,x)) ^>> app

instance ArrowFix (Env var addr,Store addr val,x) y c => ArrowFix x y (Environment var addr val c) where
  fixA f = Environment $ ReaderArrow $ proc ((e,s),x) -> fixA (unlift . f . lift) -< (e,s,x)
    where
      lift :: Arrow c => c (Env var addr,Store addr val,x) y -> Environment var addr val c x y
      lift g = Environment (ReaderArrow ((\((e,s),x) -> (e,s,x)) ^>> g))

      unlift :: Arrow c => Environment var addr val c x y -> c (Env var addr,Store addr val,x) y
      unlift (Environment (ReaderArrow g)) = (\(e,s,x) -> ((e,s),x)) ^>> g

deriving instance Arrow c => Category (Environment var addr val c)
deriving instance Arrow c => Arrow (Environment var addr val c)
deriving instance ArrowChoice c => ArrowChoice (Environment var addr val c)
deriving instance ArrowState s c => ArrowState s (Environment var addr val c)
deriving instance ArrowFail e c => ArrowFail e (Environment var addr val c)
deriving instance PreOrd (c ((Env var addr,Store addr val),x) y) => PreOrd (Environment var addr val c x y)
deriving instance Complete (c ((Env var addr,Store addr val),x) y) => Complete (Environment var addr val c x y)
deriving instance CoComplete (c ((Env var addr,Store addr val),x) y) => CoComplete (Environment var addr val c x y)
deriving instance LowerBounded (c ((Env var addr,Store addr val),x) y) => LowerBounded (Environment var addr val c x y)
deriving instance UpperBounded (c ((Env var addr,Store addr val),x) y) => UpperBounded (Environment var addr val c x y)
