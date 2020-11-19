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
module Control.Arrow.Transformer.Abstract.FiniteEnvStore where

import           Prelude hiding ((.),read,Maybe(..),lookup,map)
import qualified Prelude as P

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Cont
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Reader as Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.State as State
import           Control.Arrow.Store
import           Control.Arrow.Fail
import           Control.Arrow.Except
import           Control.Arrow.Trans
import           Control.Arrow.Fix.Context hiding (Widening)
import           Control.Arrow.Environment as Env
import           Control.Arrow.Closure
import           Control.Arrow.Fix
import           Control.Arrow.Order
import           Control.Arrow.Fix.ControlFlow
import           Control.Arrow.LetRec

import           Data.Abstract.MonotoneStore(Store)
import qualified Data.Abstract.MonotoneStore as Store
import           Data.Abstract.Closure (Closure)
import qualified Data.Abstract.Closure as Cls

import           Data.Hashed.Lazy
import           Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Order
import           Data.Identifiable
import           Data.Profunctor
import           Data.Profunctor.Unsafe ((.#))
import           Data.Coerce

type Env a b = Hashed (HashMap a b)

newtype EnvStoreT var addr val c x y =
  EnvStoreT
    (ReaderT (Env var addr)
      (StateT (Store addr val)
         c) x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowLowerBounded a,
            ArrowFail e, ArrowExcept e, ArrowRun, ArrowCont,
            ArrowCallSite ctx, ArrowControlFlow stmt)

instance ArrowLift (EnvStoreT var addr val c) where
  type Underlying (EnvStoreT var addr val c) x y =
    ReaderT (Env var addr) (StateT (Store addr val) c) x y

instance (Identifiable var, Identifiable addr, Complete val, ArrowChoice c, Profunctor c)
    => ArrowEnv var addr (EnvStoreT var addr val c) where
  type Join y (EnvStoreT var addr val c) = ()
  lookup (EnvStoreT f) (EnvStoreT g) = EnvStoreT $ proc (var,x) -> do
    env <- Reader.ask -< ()
    case Map.lookup var (unhashed env) of
      P.Just addr -> f -< (addr,x)
      P.Nothing   -> g -< x
  extend (EnvStoreT f) = EnvStoreT $ proc (var,addr,x) -> do
    env <- Reader.ask -< ()
    Reader.local f -< (mapHashed (Map.insert var addr) env, x)
  {-# INLINE lookup #-}
  {-# INLINE extend #-}
  {-# SCC lookup #-}
  {-# SCC extend #-}

instance (Identifiable var, Identifiable addr, Identifiable val, Complete val, ArrowChoice c, Profunctor c) 
    => ArrowStore addr val (EnvStoreT var addr val c) where
  type Join y (EnvStoreT var addr val c) = ()
  read (EnvStoreT f) (EnvStoreT g) = EnvStoreT $ proc (addr,x) -> do
    store <- State.get -< ()
    case Store.lookup addr store of
      P.Just val -> f -< (val,x)
      P.Nothing -> g -< x
  write = EnvStoreT $ proc (addr, val) -> do
    store <- State.get -< ()
    State.put -< Store.insert addr val store
  {-# INLINE read #-}
  {-# INLINE write #-}
  {-# SCC read #-}
  {-# SCC write #-}


instance (Identifiable var, Identifiable addr, Identifiable expr, ArrowChoice c, Profunctor c) =>
  ArrowClosure expr (Closure expr (HashSet (Env var addr))) (EnvStoreT var addr val c) where
  type Join y (Closure expr (HashSet (Env var addr))) (EnvStoreT var addr val c) = Complete y
  closure = EnvStoreT $ proc expr -> do
    env <- Reader.ask -< ()
    returnA -< Cls.closure expr (Set.singleton env)
  apply (EnvStoreT f) = Cls.apply $ proc ((expr,envs),x) ->
    (| joinList (error "encountered an empty set of environments" -< ())
                (\env -> EnvStoreT (Reader.local f) -< (env,(expr,x))) |) (Set.toList envs)
  {-# INLINE closure #-}
  {-# INLINE apply #-}
  {-# SCC closure #-}
  {-# SCC apply #-}

instance (Identifiable var, Identifiable addr, Identifiable val, Complete val, IsClosure val (HashSet (Env var addr)), ArrowChoice c, Profunctor c)
    => ArrowLetRec addr val (EnvStoreT var addr val c) where
  letRec (EnvStoreT f) = EnvStoreT $ proc (bindings,x) -> do
    go -< bindings
    f -< x
    where
      go = proc bindings -> case bindings of
        (addr,val):bs -> do
          env <- Reader.ask -< ()
          State.modify' (\((addr,val,env),store) ->
                           ((),Store.insert addr (setEnvironment (Set.singleton env) val) store))
            -< (addr,val,env)
          go -< bs
        [] -> returnA -< []
  {-# INLINE letRec #-}
  {-# SCC letRec #-}


instance (ArrowApply c, Profunctor c) => ArrowApply (EnvStoreT var addr val c) where
  app = EnvStoreT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowTrans (EnvStoreT var addr val) where
  lift' = EnvStoreT . lift' . lift'
  {-# INLINE lift' #-}

instance (Complete y, Arrow c, Profunctor c) => ArrowComplete y (EnvStoreT var addr val c) where
  EnvStoreT f <⊔> EnvStoreT g = EnvStoreT (rmap (uncurry (⊔)) (f &&& g))
  {-# INLINE (<⊔>) #-}

instance (Profunctor c, Arrow c, ArrowFix (Underlying (EnvStoreT var addr val c) x y)) => ArrowFix (EnvStoreT var addr val c x y) where
  type Fix (EnvStoreT var addr val c x y) = Fix (Underlying (EnvStoreT var addr val c) x y)
