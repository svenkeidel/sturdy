{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC
  -fspecialise-aggressively
  -flate-specialise
  -flate-dmd-anal
  -fspec-constr-keen
  -fspec-constr
  -fspec-constr-threshold=10000
  -fsimpl-tick-factor=50000
  -fmax-simplifier-iterations=10
#-}

-- Expensive:
-- {-# OPTIONS_GHC
-- -fexpose-all-unfoldings
-- -fmax-simplifier-iterations=10
-- #-}
module TypedAnalysis.Chaotic where

import           Prelude hiding (not,Bounded,fail,(.),exp,read)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fix (FixpointAlgorithm,FixpointCombinator,Fix)
import qualified Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Stack
import           Control.Arrow.Fix.Cache
import           Control.Arrow.Fix.Chaotic
import qualified Control.Arrow.Fix.Context as Ctx
import           Control.Arrow.Order(ArrowComplete)
import qualified Control.Arrow.Trans as Trans
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.FiniteEnvStore
import           Control.Arrow.Transformer.Abstract.LogError
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Component as Comp
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack as Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable as Cache
import           Control.Arrow.Transformer.Abstract.Terminating

import           Data.Text (Text)
import qualified Data.HashMap.Lazy as Map
import           Data.HashSet(HashSet)
import           Data.Profunctor
import           Data.Empty

import qualified Data.Abstract.Widening as W
import           Data.Abstract.Terminating(Terminating)

import           Syntax (Expr(App))
import qualified GenericInterpreter as Generic
import           TypedAnalysis

import           GHC.Exts

type Interp =
  ValueT Val
    (TerminatingT
      (LogErrorT Text
        (EnvStoreT Text Addr Val
          (FixT
            (ComponentT Comp.Component In
              (StackT Stack.Stack In
                (CacheT Cache.Monotone In Out
                  (ContextT Ctx
                    (->)))))))))

-- {-# SPECIALIZE if__ :: (ArrowComplete z Interp)
--                     => Interp x z -> Interp y z -> Interp (Val,(x,y)) z #-}
-- {-# SPECIALIZE Generic.eval :: Interp [Expr] Val -> Interp Expr Val #-}
-- {-# SPECIALIZE Generic.run :: Interp Expr Val -> Interp [Expr] Val -> Interp [Expr] Val #-}
-- {-# SPECIALIZE Generic.runFixed :: (?fixpointAlgorithm :: FixpointAlgorithm (Fix (Interp [Expr] Val))) => Interp [Expr] Val #-}

evalInner :: (?sensitivity :: Int) => Expr -> (Errors, Terminating Val)
evalInner e =
  let ?cacheWidening = (storeErrWidening, W.finite) in
  let ?fixpointAlgorithm = transform $
        Fix.fixpointAlgorithm $
        Fix.filter isFunctionBody (chaoticInnermost) in
  snd $ snd $ Trans.run (Generic.runFixed :: Interp [Expr] Val) (empty,(empty,[e]))

evalOuter :: (?sensitivity :: Int) => Expr -> (Errors, Terminating Val)
evalOuter e =
  let ?cacheWidening = (storeErrWidening, W.finite) in
  let ?fixpointAlgorithm = transform $
        Fix.fixpointAlgorithm $
        Fix.filter isFunctionBody (chaotic outermost) in
  snd $ snd $ Trans.run (Generic.runFixed :: Interp [Expr] Val) (empty,(empty,[e]))

