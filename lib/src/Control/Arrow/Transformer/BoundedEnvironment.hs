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

type Alloc var addr val c = BoundedEnv var addr val c var addr
newtype BoundedEnv var addr val c x y = BoundedEnv ( ReaderArrow (Alloc var addr val c, HashMap var addr) (StateArrow (Store addr val) c) x y )
  deriving (Category,Arrow,ArrowChoice)

runBoundedEnv :: (Eq var, Hashable var, Eq addr, Hashable addr, Complete val, ArrowChoice c, ArrowApply c)
              => BoundedEnv var addr val c x y -> c (Alloc var addr val c,HashMap var val,x) y
runBoundedEnv f =
  let BoundedEnv (ReaderArrow (StateArrow f')) = proc (bs,x) -> do
       env <- getEnv -< ()
       env' <- bindings -< (bs,env)
       localEnv f -< (env',x)
  in (\(al,env,x) -> (S.empty,((al,H.empty),(H.toList env,x)))) ^>> f' >>^ snd

liftBoundedEnv :: Arrow c => c x y -> BoundedEnv var addr vaal c x y
liftBoundedEnv f = BoundedEnv (liftReader (liftState f))

instance (Eq var, Hashable var, Eq addr, Hashable addr, Complete val, ArrowApply c) =>
  ArrowEnv var val (HashMap var addr) (BoundedEnv var addr val c) where
  lookup = proc x -> do
    env <- getEnv -< ()
    store <- getStore -< ()
    returnA -< do
      addr <- H.lookup x env
      S.lookup addr store
  getEnv = BoundedEnv (pi2 <<< askA)
  extendEnv = proc (x,y,env) -> do
    addr <- localEnv alloc -< (env,x)
    store <- getStore -< ()
    putStore -< S.insertWith (⊔) addr y store
    returnA -< H.insert x addr env
  localEnv (BoundedEnv (ReaderArrow f)) = BoundedEnv (ReaderArrow ((\((al,_),(env,a)) -> ((al,env),a)) ^>> f))

instance ArrowReader r c => ArrowReader r (BoundedEnv var addr val c) where
  askA = liftBoundedEnv askA
  localA (BoundedEnv (ReaderArrow (StateArrow f))) = BoundedEnv $ ReaderArrow $ StateArrow $ 
    (\(e,(s,(r,x))) -> (r,(e,(s,x)))) ^>> localA f

instance ArrowState s c => ArrowState s (BoundedEnv var addr val c) where
  getA = liftBoundedEnv getA
  putA = liftBoundedEnv putA

instance ArrowFail e c => ArrowFail e (BoundedEnv var addr val c) where
  failA = liftBoundedEnv failA

instance ArrowApply c => ArrowApply (BoundedEnv var addr val c) where
  app = BoundedEnv $ (\(BoundedEnv f,x) -> (f,x)) ^>> app

getStore :: Arrow c => BoundedEnv var addr val c () (Store addr val)
getStore = BoundedEnv getA
{-# INLINE getStore #-}

putStore :: Arrow c => BoundedEnv var addr val c (Store addr val) ()
putStore = BoundedEnv putA
{-# INLINE putStore #-}

alloc :: ArrowApply c => BoundedEnv var addr val c var addr
alloc = proc v -> do
  (a,_) <- BoundedEnv askA -< ()
  a -<< v
{-# INLINE alloc #-}

instance (ArrowApply c, ArrowFix (HashMap var addr,Store addr val,x) (Store addr val,y) c) => ArrowFix x y (BoundedEnv var addr val c) where
  fixA f = BoundedEnv $ ReaderArrow $ StateArrow $ proc (s,((a,e),x)) -> do
    fixA (unlift a . f . lift) -<< (e,s,x)
    where
      lift :: Arrow c => c (HashMap var addr,Store addr val,x) (Store addr val,y) -> BoundedEnv var addr val c x y
      lift g = BoundedEnv (ReaderArrow (StateArrow ((\(s,((_,e),x)) -> (e,s,x)) ^>> g)))

      unlift :: Arrow c => Alloc var addr val c -> BoundedEnv var addr val c x y -> c (HashMap var addr,Store addr val,x) (Store addr val,y)
      unlift a (BoundedEnv (ReaderArrow (StateArrow g))) = (\(s,e,x) -> (e,((a,s),x))) ^>> g


deriving instance PreOrd (c (Store addr val,((Alloc var addr val c,HashMap var addr),x)) (Store addr val,y)) => PreOrd (BoundedEnv var addr val c x y)
deriving instance Complete (c (Store addr val,((Alloc var addr val c,HashMap var addr),x)) (Store addr val,y)) => Complete (BoundedEnv var addr val c x y)
deriving instance CoComplete (c (Store addr val,((Alloc var addr val c,HashMap var addr),x)) (Store addr val,y)) => CoComplete (BoundedEnv var addr val c x y)
deriving instance LowerBounded (c (Store addr val,((Alloc var addr val c,HashMap var addr),x)) (Store addr val,y)) => LowerBounded (BoundedEnv var addr val c x y)
deriving instance UpperBounded (c (Store addr val,((Alloc var addr val c,HashMap var addr),x)) (Store addr val,y)) => UpperBounded (BoundedEnv var addr val c x y)
