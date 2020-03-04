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

import Prelude hiding ((.),read,Maybe(..),lookup,map,fail)
import qualified Prelude as P

import Control.Category
import Control.Arrow
import Control.Arrow.Cont
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
import Control.Arrow.LetRec

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
import Data.Maybe(mapMaybe)

newtype EnvStoreT var addr val c x y = EnvStoreT (ReaderT (HashMap var addr) (StateT (HashMap addr val) c) x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans, ArrowLowerBounded a,
            ArrowFail e, ArrowExcept e, ArrowRun, ArrowCont,
            ArrowContext ctx)

instance (Identifiable var, Identifiable addr, ArrowChoice c, Profunctor c) 
    => ArrowEnv var addr (EnvStoreT var addr val c) where
  type Join y (EnvStoreT var addr val c) = ()
  lookup (EnvStoreT f) (EnvStoreT g) = EnvStoreT $ proc (var,x) -> do
    env <- Reader.ask -< ()
    case Map.lookup var env of
      P.Just addr -> f -< (addr,x)
      P.Nothing   -> g -< x
  extend (EnvStoreT f) = EnvStoreT $ proc (var,addr,x) -> do
    env <- Reader.ask -< ()
    Reader.local f -< (Map.insert var addr env, x)
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance (Identifiable var, Identifiable addr, ArrowChoice c, Profunctor c) 
    => ArrowStore addr val (EnvStoreT var addr val c) where
  type Join y (EnvStoreT var addr val c) = ()
  read (EnvStoreT f) (EnvStoreT g) = EnvStoreT $ proc (addr,x) -> do
    store <- State.get -< ()
    case Map.lookup addr store of
      P.Just val -> f -< (val,x)
      P.Nothing -> g -< x
  write = EnvStoreT $ proc (addr, val) -> do
    store <- State.get -< ()
    State.put -< Map.insert addr val store
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


instance (Identifiable var, Identifiable addr, IsClosure val (HashMap var addr), ArrowChoice c, Profunctor c) 
    => ArrowLetRec var val (EnvStoreT var addr val c) where
  letRec (EnvStoreT f) = EnvStoreT $ proc (bindings,x) -> do
    env <- Reader.ask -< ()
    let addrs = mapMaybe (\var -> Map.lookup var env) [var | (var,_) <- bindings]
    let env' = Map.fromList [(var, addr) | ((var,_),addr) <- zip bindings addrs] `Map.union` env
        val' = Map.fromList [(addr, setEnvironment env' val) | ((_,val),addr) <- zip bindings addrs]
    State.modify' (\(val,store) -> ((), Map.union val store)) -< val'
    Reader.local f -< (env',x)

instance (ArrowApply c, Profunctor c) => ArrowApply (EnvStoreT var addr val c) where
  app = EnvStoreT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowLift (EnvStoreT var addr val) where
  lift' = EnvStoreT . lift' . lift'
  {-# INLINE lift' #-}

instance ArrowState s c => ArrowState s (EnvStoreT var addr val c) where
  get = lift' State.get
  put = lift' State.put

type instance Fix (EnvStoreT var addr val c) x y = EnvStoreT var addr val (Fix c (HashMap addr val,(HashMap var addr,x)) (HashMap addr val,y))
deriving instance (Profunctor c, Arrow c, ArrowFix (c (HashMap addr val,(HashMap var addr,x)) (HashMap addr val,y))) => ArrowFix (EnvStoreT var addr val c x y)
