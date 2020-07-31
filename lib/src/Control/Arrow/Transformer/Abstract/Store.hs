{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.Store where

import           Prelude hiding (Maybe(..))
import qualified Prelude as P 

import           Control.Arrow
import           Control.Arrow.Cont
import           Control.Arrow.Const
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Trans
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Arrow.Except
import           Control.Arrow.Environment
import           Control.Arrow.Closure
import           Control.Arrow.Transformer.State
import           Control.Arrow.Utils
import           Control.Category
import           Control.Arrow.Fix.Context
import           Control.Arrow.Order

import           Data.Abstract.Maybe
import           Data.Abstract.Map (Map)
import qualified Data.Abstract.Map as M
import qualified Data.HashMap.Lazy as HM 
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashSet as HS
import           Data.Identifiable
import           Data.Profunctor
import           Data.Profunctor.Unsafe((.#))
import           Data.Coerce
import           Data.Order

newtype StoreT store c x y = StoreT (StateT store c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowLift,ArrowTrans,
            ArrowCont, ArrowConst r, ArrowReader r,
            ArrowEnv var' val', ArrowClosure expr cls,
            ArrowFail e, ArrowExcept e, ArrowState store,
            ArrowLowerBounded a, ArrowRun, ArrowJoin, 
            ArrowContext ctx)

runStoreT :: StoreT store c x y -> c (store, x) (store, y)
runStoreT = coerce
{-# INLINE runStoreT #-}

evalStoreT :: Profunctor c => StoreT store c x y -> c (store, x) y
evalStoreT f = rmap pi2 (runStoreT f)

execStoreT :: Profunctor c => StoreT store c x y -> c (store, x) store
execStoreT f = rmap pi1 (runStoreT f)

instance (Identifiable var, ArrowChoice c, Profunctor c) => ArrowStore var val (StoreT (Map var val) c) where
  type Join y (StoreT (Map var val) c) = ArrowComplete (Map var val,y) c
  read (StoreT f) (StoreT g) = StoreT $ proc (var,x) -> do
    s <- get -< ()
    case M.lookup var s of
      Just val        -> f -< (val,x)
      Nothing         -> g -< x
      JustNothing val -> (f -< (val,x)) <⊔> (g -< x)
  write = StoreT $ modify $ arr $ \((var,val),st) -> ((),M.insert var val st)
  remove = undefined 
  keys = undefined
  store = undefined
  {-# INLINE read #-}
  {-# INLINE write #-}

instance (Identifiable addr, ArrowChoice c, Profunctor c, Complete val) 
    => ArrowStore addr val (StoreT (HashMap addr val) c) where
  type Join y (StoreT (HashMap addr val) c) = ArrowComplete (HashMap addr val,y) c
  read (StoreT f) (StoreT g) = StoreT $ proc (addr,x) -> do
    s <- get -< ()
    case HM.lookup addr s of
      P.Just val -> f -< (val,x)
      P.Nothing -> g -< x
  write = StoreT $ proc (addr, val) -> do
    s <- get -< ()
    put -< HM.insertWith (\oldVal newVal -> oldVal ⊔ newVal) addr val s
  remove = StoreT $ modify $ arr (\(var,s) -> ((), HM.delete var s))
  keys = StoreT $ proc _ -> do 
    s <- get -< () 
    returnA -< HS.fromList $ HM.keys s 
  store = StoreT $ proc _ -> get -< () 
  {-# INLINE read #-}
  {-# INLINE write #-}
  {-# SCC read #-}
  {-# SCC write #-}

deriving instance (ArrowComplete (store,y) c) => ArrowComplete y (StoreT store c)
instance (ArrowApply c, Profunctor c) => ArrowApply (StoreT store c) where
  app = StoreT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowFix (Underlying (StoreT store c) x y) => ArrowFix (StoreT store c x y) where
  type Fix (StoreT store c x y) = Fix (Underlying (StoreT store c) x y)
