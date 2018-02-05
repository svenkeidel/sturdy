{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.BoundedEnvironment(ArrowAlloc(..),BoundedEnv,runBoundedEnv,liftBoundedEnv) where

import           Control.Category
import           Control.Arrow
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

class Arrow c => ArrowAlloc x addr c | c -> x, c -> addr where
  alloc :: c x addr

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

getStore :: Arrow c => BoundedEnv a addr b c () (HashMap addr b)
getStore = BoundedEnv getA
{-# INLINE getStore #-}

putStore :: Arrow c => BoundedEnv a addr b c (HashMap addr b) ()
putStore = BoundedEnv putA
{-# INLINE putStore #-}
