{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Control.Arrow.Transformer.Concrete.FiniteEnvStore where

import Prelude hiding ((.),read,Maybe(..),lookup,map)
import qualified Prelude as P

import Control.Category
import Control.Arrow
import Control.Arrow.Cont
import Control.Arrow.Const
import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.Reader
import Control.Arrow.Reader as Reader
import Control.Arrow.Transformer.State
import Control.Arrow.State as State
import Control.Arrow.Store
import Control.Arrow.Fail
import Control.Arrow.Except
import Control.Arrow.Trans
import Control.Arrow.Fix.Context
import Control.Arrow.Environment as Env
import Control.Arrow.Closure
import Control.Arrow.Fix
import Control.Arrow.Order
import Control.Arrow.Utils

-- import Data.Abstract.Widening (Widening)
-- import Data.Abstract.Closure (Closure)
-- import qualified Data.Abstract.Closure as Cls
import Data.Concrete.Closure (Closure)
import qualified Data.Concrete.Closure as Cls

import Data.HashSet()
-- import qualified Data.HashSet as Set
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import Data.Order()
import Data.Identifiable
import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce

type Alloc var addr val c = EnvStoreT var addr val c (var,val) addr
newtype EnvStoreT var addr val c x y = EnvStoreT (ConstT (Alloc var addr val c) (ReaderT (HashMap var addr) (StateT (HashMap addr val) c)) x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans, ArrowLowerBounded,
            ArrowFail e, ArrowExcept e, ArrowRun, ArrowCont,
            ArrowContext ctx, ArrowState (HashMap addr val))

-- read (StoreT f) (StoreT g) = StoreT $ proc (var,x) -> do
--   s <- get -< ()
--   case S.lookup var s of
--     Just v -> f -< (v,x)
--     Nothing -> g -< x

instance (Identifiable var, Identifiable addr, ArrowChoice c, Profunctor c) => ArrowEnv var val (EnvStoreT var addr val c) where
  type Join y (EnvStoreT var addr val c) = ()
  lookup (EnvStoreT f) (EnvStoreT g) = EnvStoreT $ proc (var,x) -> do
    env <- Reader.ask -< ()
    store <- State.get -< ()
    case do { addr <- Map.lookup var env; Map.lookup addr store } of
      P.Just val -> f -< (val,x)
      P.Nothing -> g -< x
  extend (EnvStoreT f) = EnvStoreT $ askConst $ \(EnvStoreT alloc) -> proc (var,val,x) -> do
    env <- Reader.ask -< ()
    store <- State.get -< ()
    addr <- alloc -< (var, val)
    State.put -< Map.insert addr val store
    Reader.local f -< (Map.insert var addr env, x)  
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance (Identifiable var, Identifiable addr, ArrowChoice c, Profunctor c) => ArrowStore var val (EnvStoreT var addr val c) where
  type Join y (EnvStoreT var addr val c) = ()
  read (EnvStoreT f) (EnvStoreT g) = EnvStoreT $ proc (var,x) -> do
    env <- Reader.ask -< ()
    store <- State.get -< ()
    case do { addr <- Map.lookup var env; Map.lookup addr store } of
      P.Just val -> f -< (val,x)
      P.Nothing -> g -< x
  write = EnvStoreT $ proc (var, val) -> do
    env <- Reader.ask -< ()
    store <- State.get -< ()
    case Map.lookup var env of 
      P.Just addr -> State.put -< Map.insert addr val store
      P.Nothing -> returnA -< ()
    returnA -< ()
  {-# INLINE read #-}
  {-# INLINE write #-}
    

instance (Identifiable var, Identifiable addr, Identifiable expr, ArrowChoice c, Profunctor c) =>
  ArrowClosure expr (Closure expr (HashMap var addr)) (EnvStoreT var addr val c) where
  type Join y (Closure expr (HashMap var addr)) (EnvStoreT var addr val c) = ()
  closure = EnvStoreT $ proc expr -> do
    env <- Reader.ask -< ()
    returnA -< Cls.Closure expr env
  apply (EnvStoreT f) = EnvStoreT $ proc (Cls.Closure expr env,x) ->
    Reader.local f -< (env,(expr,x))
  {-# INLINE closure #-}
  {-# INLINE apply #-}


instance (Identifiable var, Identifiable addr, IsClosure val (HashMap var addr), ArrowChoice c, Profunctor c) => ArrowLetRec var val (EnvStoreT var addr val c) where
  letRec (EnvStoreT f) = EnvStoreT $ askConst $ \(EnvStoreT alloc) -> proc (bindings,x) -> do
    env <- Reader.ask -< ()
    addrs <- map alloc -< bindings
    let env' = Map.fromList [ (var,addr) | ((var,_), addr) <- zip bindings addrs ] `Map.union` env
        vals = Map.fromList [ (addr, setEnvironment env' val) | (addr, (_,val)) <- zip addrs bindings ]
    -- changed Map.union store vals to Map.union vals store, because for equal keys left store is prioritized
    -- Probably not sound because data can be lost from store
    -- State.modify' (\(vals,store) -> ((), Map.union vals store)) -< vals
    State.modify' (\(vals,store) -> ((), Map.union store vals)) -< vals

    Reader.local f -< (env',x)
  {-# INLINE letRec #-}


instance (ArrowApply c, Profunctor c) => ArrowApply (EnvStoreT var addr val c) where
  app = EnvStoreT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowLift (EnvStoreT var addr val) where
  lift' = EnvStoreT . lift' . lift' . lift'
  {-# INLINE lift' #-}

instance ArrowState s c => ArrowState s (EnvStoreT var addr val c) where
  get = lift' State.get
  put = lift' State.put


-- instance (Complete y, ArrowEffectCommutative c, Arrow c, Profunctor c) => ArrowComplete y (EnvStoreT var addr val c) where
--   EnvStoreT f <⊔> EnvStoreT g = EnvStoreT (rmap (uncurry (⊔)) (f &&& g))

type instance Fix (EnvStoreT var addr val c) x y = EnvStoreT var addr val (Fix c (HashMap addr val,(HashMap var addr,x)) (HashMap addr val,y))
deriving instance (Profunctor c, Arrow c, ArrowFix (c (HashMap addr val,(HashMap var addr,x)) (HashMap addr val,y))) => ArrowFix (EnvStoreT var addr val c x y)
