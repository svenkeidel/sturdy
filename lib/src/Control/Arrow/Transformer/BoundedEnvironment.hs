{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Arrow.Transformer.BoundedEnvironment(BoundedEnv,runBoundedEnv,liftBoundedEnv,Alloc) where

import           Prelude hiding ((.))
import           Control.Category
import           Control.Arrow
import           Control.Arrow.Class.Environment
import           Control.Arrow.Class.Reader
import           Control.Arrow.Class.State
import           Control.Arrow.Class.Fail
import           Control.Arrow.Class.Fix
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Utils

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import           Data.Hashable
import           Data.Order
import           Data.Store (Store)
import qualified Data.Store as S

type Alloc a addr b c = BoundedEnv a addr b c a addr
newtype BoundedEnv a addr b c x y = BoundedEnv ( ReaderArrow (Alloc a addr b c, HashMap a addr) (StateArrow (Store addr b) c) x y )
  deriving (Category,Arrow,ArrowChoice)

runBoundedEnv :: (Eq a, Hashable a, Eq addr, Hashable addr, Complete b, ArrowChoice c, ArrowApply c)
              => BoundedEnv a addr b c x y -> c (Alloc a addr b c,HashMap a b,x) y
runBoundedEnv f =
  let BoundedEnv (ReaderArrow (StateArrow f')) = proc (bs,x) -> do
       env <- getEnv -< ()
       env' <- bindings -< (bs,env)
       localEnv f -< (env',x)
  in (\(alloc,env,x) -> (S.empty,((alloc,H.empty),(H.toList env,x)))) ^>> f' >>^ snd

liftBoundedEnv :: Arrow c => c x y -> BoundedEnv a addr b c x y
liftBoundedEnv f = BoundedEnv (liftReader (liftState f))

instance (Eq a, Hashable a, Eq addr, Hashable addr, Complete b, ArrowApply c) =>
  ArrowEnv a b (HashMap a addr) (BoundedEnv a addr b c) where
  lookup = proc x -> do
    env <- getEnv -< ()
    store <- getStore -< ()
    returnA -< do
      addr <- H.lookup x env
      S.lookup addr store
  getEnv = BoundedEnv (pi2 <<< askA)
  extendEnv = proc (x,y,env) -> do
    alloc <- BoundedEnv (pi1 <<< askA) -< ()
    addr <- localEnv alloc -<< (env,x)
    store <- getStore -< ()
    putStore -< S.insertWith (⊔) addr y store
    returnA -< H.insert x addr env
  localEnv (BoundedEnv (ReaderArrow f)) = BoundedEnv (ReaderArrow ((\((alloc,_),(env,a)) -> ((alloc,env),a)) ^>> f))

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

getStore :: Arrow c => BoundedEnv a addr b c () (Store addr b)
getStore = BoundedEnv getA
{-# INLINE getStore #-}

putStore :: Arrow c => BoundedEnv a addr b c (Store addr b) ()
putStore = BoundedEnv putA
{-# INLINE putStore #-}

instance (ArrowApply c, ArrowFix (HashMap a addr,Store addr b,x) (Store addr b,y) c) => ArrowFix x y (BoundedEnv a addr b c) where
  fixA f = lift $ proc (a,e,s,x) -> do
    fixA (unlift a . f . lift') -<< (e,s,x)
    where
      lift :: Arrow c => c (Alloc a addr b c, HashMap a addr,Store addr b,x) (Store addr b,y) -> BoundedEnv a addr b c x y
      lift g = BoundedEnv (ReaderArrow (StateArrow ((\(s,((a,e),x)) -> (a,e,s,x)) ^>> g)))

      lift' :: Arrow c => c (HashMap a addr,Store addr b,x) (Store addr b,y) -> BoundedEnv a addr b c x y
      lift' g = BoundedEnv (ReaderArrow (StateArrow ((\(s,((_,e),x)) -> (e,s,x)) ^>> g)))

      unlift :: Arrow c => Alloc a addr b c -> BoundedEnv a addr b c x y -> c (HashMap a addr,Store addr b,x) (Store addr b,y)
      unlift a (BoundedEnv (ReaderArrow (StateArrow g))) = (\(s,e,x) -> (e,((a,s),x))) ^>> g


deriving instance PreOrd (c (Store addr b,((Alloc a addr b c,HashMap a addr),x)) (Store addr b,y)) => PreOrd (BoundedEnv a addr b c x y)
deriving instance Complete (c (Store addr b,((Alloc a addr b c,HashMap a addr),x)) (Store addr b,y)) => Complete (BoundedEnv a addr b c x y)
deriving instance CoComplete (c (Store addr b,((Alloc a addr b c,HashMap a addr),x)) (Store addr b,y)) => CoComplete (BoundedEnv a addr b c x y)
deriving instance LowerBounded (c (Store addr b,((Alloc a addr b c,HashMap a addr),x)) (Store addr b,y)) => LowerBounded (BoundedEnv a addr b c x y)
deriving instance UpperBounded (c (Store addr b,((Alloc a addr b c,HashMap a addr),x)) (Store addr b,y)) => UpperBounded (BoundedEnv a addr b c x y)
