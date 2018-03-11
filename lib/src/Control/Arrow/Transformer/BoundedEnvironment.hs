{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
module Control.Arrow.Transformer.BoundedEnvironment(BoundedEnv,runBoundedEnv,liftBoundedEnv,ArrowAlloc(..)) where

import           Prelude hiding ((.),id)
import           Control.Category
import           Control.Arrow
import           Control.Arrow.Class.Alloc
import           Control.Arrow.Class.Environment
import           Control.Arrow.Class.Reader
import           Control.Arrow.Class.State
import           Control.Arrow.Class.Fail
import           Control.Arrow.Class.Fix
import           Control.Arrow.Transformer.Reader

import           Data.Hashable
import           Data.Order
import           Data.Environment (Env)
import qualified Data.Environment as E
import           Data.Store (Store)
import qualified Data.Store as S

-- The galois connection for environment store pairs ensures that if
-- an variable is bound to an address in the environment, then the
-- address has an binding in the store.
newtype BoundedEnv var addr val c x y =
  BoundedEnv ( ReaderArrow (Env var addr,Store addr val) c x y )
  deriving (Category,Arrow,ArrowChoice)

runBoundedEnv :: (Eq var, Hashable var, Eq addr, Hashable addr, Complete val, ArrowChoice c, ArrowAlloc var addr val c)
              => BoundedEnv var addr val c x y -> c ([(var,val)],x) y
runBoundedEnv f =
  let BoundedEnv (ReaderArrow f') = proc (bs,x) -> do
       env <- getEnv -< ()
       env' <- bindings -< (bs,env)
       localEnv f -< (env',x)
  in (const (E.empty,S.empty) &&& id) ^>> f'

liftBoundedEnv :: Arrow c => c x y -> BoundedEnv var addr val c x y
liftBoundedEnv f = BoundedEnv (liftReader f)

instance (Eq var, Hashable var, Eq addr, Hashable addr, Complete val, ArrowAlloc var addr val c) =>
  ArrowEnv var val (Env var addr,Store addr val) (BoundedEnv var addr val c) where
  lookup = proc x -> do
    (env,store) <- getEnv -< ()
    returnA -< do
      addr <- E.lookup x env
      S.lookup addr store
  getEnv = BoundedEnv askA
  extendEnv = proc (x,y,(env,store)) -> do
    addr <- liftBoundedEnv alloc -< (x,env,store)
    returnA -< (E.insert x addr env,S.insertWith (⊔) addr y store)
  localEnv (BoundedEnv (ReaderArrow f)) = BoundedEnv (ReaderArrow ((\(_,(e,a)) -> (e,a)) ^>> f))

instance ArrowReader r c => ArrowReader r (BoundedEnv var addr val c) where
  askA = liftBoundedEnv askA
  localA (BoundedEnv (ReaderArrow f)) = BoundedEnv $ ReaderArrow $ (\(env,(r,x)) -> (r,(env,x))) ^>> localA f

instance ArrowState s c => ArrowState s (BoundedEnv var addr val c) where
  getA = liftBoundedEnv getA
  putA = liftBoundedEnv putA

instance ArrowFail e c => ArrowFail e (BoundedEnv var addr val c) where
  failA = liftBoundedEnv failA

instance ArrowApply c => ArrowApply (BoundedEnv var addr val c) where
  app = BoundedEnv $ (\(BoundedEnv f,x) -> (f,x)) ^>> app

instance ArrowFix (Env var addr,Store addr val,x) y c => ArrowFix x y (BoundedEnv var addr val c) where
  fixA f = BoundedEnv $ ReaderArrow $ proc ((e,s),x) -> fixA (unlift . f . lift) -< (e,s,x)
    where
      lift :: Arrow c => c (Env var addr,Store addr val,x) y -> BoundedEnv var addr val c x y
      lift g = BoundedEnv (ReaderArrow ((\((e,s),x) -> (e,s,x)) ^>> g))

      unlift :: Arrow c => BoundedEnv var addr val c x y -> c (Env var addr,Store addr val,x) y
      unlift (BoundedEnv (ReaderArrow g)) = (\(e,s,x) -> ((e,s),x)) ^>> g

deriving instance PreOrd (c ((Env var addr,Store addr val),x) y) => PreOrd (BoundedEnv var addr val c x y)
deriving instance Complete (c ((Env var addr,Store addr val),x) y) => Complete (BoundedEnv var addr val c x y)
deriving instance CoComplete (c ((Env var addr,Store addr val),x) y) => CoComplete (BoundedEnv var addr val c x y)
deriving instance LowerBounded (c ((Env var addr,Store addr val),x) y) => LowerBounded (BoundedEnv var addr val c x y)
deriving instance UpperBounded (c ((Env var addr,Store addr val),x) y) => UpperBounded (BoundedEnv var addr val c x y)
