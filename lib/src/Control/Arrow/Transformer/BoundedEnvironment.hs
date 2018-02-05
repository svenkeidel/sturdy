{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Arrow.Transformer.BoundedEnvironment(ArrowAlloc(..),BoundedEnv,runBoundedEnv,liftBoundedEnv) where

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Class.Alloc
import           Control.Arrow.Class.Environment
import           Control.Arrow.Class.Reader
import           Control.Arrow.Class.State
import           Control.Arrow.Class.Fail
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import           Data.Hashable
import           Data.Order

newtype BoundedEnv a addr b c x y = BoundedEnv ( ReaderArrow (HashMap a addr) (StateArrow (HashMap addr b) c) x y )
  deriving (Category,Arrow,ArrowChoice)

runBoundedEnv :: Arrow c => BoundedEnv a addr b c x y -> c (HashMap a addr,HashMap addr b,x) (HashMap addr b,y)
runBoundedEnv (BoundedEnv (ReaderArrow (StateArrow f))) = (\(env,store,x) -> (store,(env,x))) ^>> f

liftBoundedEnv :: Arrow c => c x y -> BoundedEnv a addr b c x y
liftBoundedEnv f = BoundedEnv (liftReader (liftState f))

instance (Eq a, Hashable a, Eq addr, Hashable addr, Complete b, Arrow c, ArrowAlloc a addr (BoundedEnv a addr b c)) =>
  ArrowEnv a b (HashMap a addr) (BoundedEnv a addr b c) where
  lookup = proc x -> do
    env <- getEnv -< ()
    store <- getStore -< ()
    returnA -< do
      addr <- H.lookup x env
      H.lookup addr store
  getEnv = BoundedEnv askA
  extendEnv = proc (x,y,env) -> do
    addr <- localEnv alloc -< (env,x)
    store <- getStore -< ()
    putStore -< H.insertWith (⊔) addr y store
    returnA -< H.insert x addr env
  localEnv (BoundedEnv f) = BoundedEnv (localA f)

instance ArrowReader r c => ArrowReader r (BoundedEnv a addr b c) where
  askA = liftBoundedEnv askA
  localA (BoundedEnv (ReaderArrow (StateArrow f))) = BoundedEnv $ ReaderArrow $ StateArrow $ 
    (\(e,(s,(r,x))) -> (r,(e,(s,x)))) ^>> localA f

instance ArrowState s c => ArrowState s (BoundedEnv a addr b c) where
  getA = liftBoundedEnv getA
  putA = liftBoundedEnv putA

instance ArrowFail e c => ArrowFail e (BoundedEnv a addr b c) where
  failA = liftBoundedEnv failA

instance ArrowApply c => ArrowApply (BoundedEnv a addr b c) where
  app = BoundedEnv $ (\(BoundedEnv f,x) -> (f,x)) ^>> app

getStore :: Arrow c => BoundedEnv a addr b c () (HashMap addr b)
getStore = BoundedEnv getA
{-# INLINE getStore #-}

putStore :: Arrow c => BoundedEnv a addr b c (HashMap addr b) ()
putStore = BoundedEnv putA
{-# INLINE putStore #-}

deriving instance PreOrd (c (HashMap addr b,(HashMap a addr,x)) (HashMap addr b,y)) => PreOrd (BoundedEnv a addr b c x y)
deriving instance Complete (c (HashMap addr b,(HashMap a addr,x)) (HashMap addr b,y)) => Complete (BoundedEnv a addr b c x y)
deriving instance CoComplete (c (HashMap addr b,(HashMap a addr,x)) (HashMap addr b,y)) => CoComplete (BoundedEnv a addr b c x y)
deriving instance LowerBounded (c (HashMap addr b,(HashMap a addr,x)) (HashMap addr b,y)) => LowerBounded (BoundedEnv a addr b c x y)
deriving instance UpperBounded (c (HashMap addr b,(HashMap a addr,x)) (HashMap addr b,y)) => UpperBounded (BoundedEnv a addr b c x y)
