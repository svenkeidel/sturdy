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

newtype EnvStoreT var addr val c x y = EnvStoreT (ReaderT (HashMap var addr) (StateT (HashMap addr val) c) x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowLift, ArrowLowerBounded a,
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
  vals = undefined
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance (Identifiable var, Identifiable addr, ArrowChoice c, Profunctor c) 
    => ArrowStore addr val (EnvStoreT var addr val c) where
  type Join y (EnvStoreT var addr val c) = ()
  read (EnvStoreT f) (EnvStoreT g) = EnvStoreT $ proc (addr,x) -> do
    s <- State.get -< ()
    case Map.lookup addr s of
      P.Just val -> f -< (val,x)
      P.Nothing -> g -< x
  write = EnvStoreT $ proc (addr, val) -> do
    s <- State.get -< ()
    State.put -< Map.insert addr val s
  remove = undefined 
  keys = undefined
  store = undefined
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
    => ArrowLetRec addr val (EnvStoreT var addr val c) where
  letRec (EnvStoreT f) = EnvStoreT $ proc (bindings,x) -> do
    go -< bindings
    f -< x
    where
      go = proc bindings -> case bindings of
        (addr,val):bs -> do
          env <- Reader.ask -< ()
          State.modify' (\((addr,val,env),s) ->
                           ((),Map.insert addr (setEnvironment env val) s))
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

instance ArrowState s c => ArrowState s (EnvStoreT var addr val c) where
  get = lift' State.get
  put = lift' State.put

instance ArrowReader r c => ArrowReader r (EnvStoreT var addr val c) where
  ask = lift' Reader.ask
  local (EnvStoreT (ReaderT f)) = EnvStoreT (ReaderT (lmap (\(env,(r,x)) -> (r,(env,x))) (Reader.local f)))
  {-# INLINE ask #-}
  {-# INLINE local #-}

instance (Profunctor c, Arrow c, ArrowFix (Underlying (EnvStoreT var addr val c) x y)) => ArrowFix (EnvStoreT var addr val c x y) where
  type Fix (EnvStoreT var addr val c x y) = Fix (Underlying (EnvStoreT var addr val c) x y)