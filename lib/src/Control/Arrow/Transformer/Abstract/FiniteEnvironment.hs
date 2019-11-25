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
module Control.Arrow.Transformer.Abstract.FiniteEnvironment where

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

import Data.Abstract.Widening (Widening)
import Data.Abstract.Closure (Closure)
import qualified Data.Abstract.Closure as Cls

import Data.HashSet(HashSet)
import qualified Data.HashSet as Set
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import Data.Order
import Data.Identifiable
import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce

type Alloc var addr val c = EnvT var addr val c (var,val) addr
newtype EnvT var addr val c x y = EnvT (ConstT (Alloc var addr val c, Widening val) (ReaderT (HashMap var addr) (StateT (HashMap addr val) c)) x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans, ArrowLowerBounded,
            ArrowFail e, ArrowExcept e, ArrowStore var' val', ArrowRun, ArrowCont,
            ArrowContext ctx)

instance (Identifiable var, Identifiable addr, Complete val, ArrowEffectCommutative c, ArrowChoice c, Profunctor c) => ArrowEnv var val (EnvT var addr val c) where
  type Join y (EnvT var addr val c) = ()
  lookup (EnvT f) (EnvT g) = EnvT $ proc (var,x) -> do
    env <- Reader.ask -< ()
    store <- State.get -< ()
    case do { addr <- Map.lookup var env; Map.lookup addr store } of
      P.Just val -> f -< (val,x)
      P.Nothing   -> g -< x
  extend (EnvT f) = EnvT $ askConst $ \(EnvT alloc,widening) -> proc (var,val,x) -> do
    env <- Reader.ask -< ()
    store <- State.get -< ()
    case Map.lookup var env of
      P.Just addr -> do
        State.put -< Map.insertWith (\old new -> snd (widening old new)) addr val store
        f -< x
      P.Nothing -> do
        addr <- alloc -< (var,val)
        State.put -< Map.insertWith (\old new -> snd (widening old new)) addr val store
        Reader.local f -< (Map.insert var addr env, x)
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance (Identifiable var, Identifiable addr, Identifiable expr, ArrowEffectCommutative c, ArrowChoice c, Profunctor c) =>
  ArrowClosure expr (Closure expr (HashSet (HashMap var addr))) (EnvT var addr val c) where
  type Join y (EnvT var addr val c) = Complete y
  closure = EnvT $ proc expr -> do
    env <- Reader.ask -< ()
    returnA -< Cls.closure expr (Set.singleton env)
  apply (EnvT f) = Cls.apply $ proc ((expr,envs),x) ->
    (| joinList (error "encountered an empty set of environments" -< ())
                (\env -> EnvT (Reader.local f) -< (env,(expr,x))) |) (Set.toList envs)
  {-# INLINE closure #-}
  {-# INLINE apply #-}

instance (Identifiable var, Identifiable addr, Complete val, IsClosure val (HashSet (HashMap var addr)), ArrowEffectCommutative c, ArrowChoice c, Profunctor c) => ArrowLetRec var val (EnvT var addr val c) where
  letRec (EnvT f) = EnvT $ askConst $ \(EnvT alloc,widening) -> proc (bindings,x) -> do
    env <- Reader.ask -< ()
    addrs <- map alloc -< bindings
    let env' = Map.fromList [ (var,addr) | ((var,_), addr) <- zip bindings addrs ] `Map.union` env
        vals = Map.fromList [ (addr, setEnvironment (Set.singleton env') val) | (addr, (_,val)) <- zip addrs bindings ]
    State.modify' (\(vals,store) -> ((), Map.unionWith (\old new -> snd (widening old new)) store vals)) -< vals
    Reader.local f -< (env',x)
  {-# INLINE letRec #-}


instance (ArrowApply c, Profunctor c) => ArrowApply (EnvT var addr val c) where
  app = EnvT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowLift (EnvT var addr val) where
  lift' = EnvT . lift' . lift' . lift'
  {-# INLINE lift' #-}

instance (Complete y, ArrowEffectCommutative c, Arrow c, Profunctor c) => ArrowComplete y (EnvT var addr val c) where
  EnvT f <⊔> EnvT g = EnvT (rmap (uncurry (⊔)) (f &&& g))

type instance Fix (EnvT var addr val c) x y = EnvT var addr val (Fix c (HashMap addr val,(HashMap var addr,x)) (HashMap addr val,y))
deriving instance (Profunctor c, Arrow c, ArrowFix (c (HashMap addr val,(HashMap var addr,x)) (HashMap addr val,y))) => ArrowFix (EnvT var addr val c x y)
