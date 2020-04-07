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
  -fsimpl-tick-factor=50000
  -fmax-simplifier-iterations=10
#-}
module TypedAnalysis.Parallel where

import           Prelude hiding (not,Bounded,fail,(.),exp,read)

import           Control.Category
import           Control.Arrow.Order
import           Control.Arrow.Fix (ArrowFix(..),FixpointAlgorithm)
import qualified Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Parallel as Par
import qualified Control.Arrow.Fix.Context as Ctx
import qualified Control.Arrow.Trans as Trans
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.FiniteEnvStore
import           Control.Arrow.Transformer.Abstract.LogError
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack as Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable as Cache
import           Control.Arrow.Transformer.Abstract.Terminating

import           Data.Text (Text)
import qualified Data.HashMap.Lazy as Map
import           Data.HashSet(HashSet)
import           Data.Empty

import qualified Data.Abstract.Widening as W
import           Data.Abstract.Terminating(Terminating)


import           Syntax (Expr(App))
import           GenericInterpreter as Generic
import           TypedAnalysis

type Interp =
  ValueT Val
    (TerminatingT
      (LogErrorT Text
        (EnvStoreT Text Addr Val
          (FixT
            (StackT Stack.Monotone In
              (CacheT (Parallel Cache.Monotone) In Out
                (ContextT Ctx
                  (->))))))))

{-# SPECIALIZE if__ :: (ArrowComplete z Interp)
                    => Interp x z -> Interp y z -> Interp (Val,(x,y)) z #-}
{-# SPECIALIZE Generic.eval :: Interp [Expr] Val -> Interp Expr Val #-}
{-# SPECIALIZE Generic.run :: Interp Expr Val -> Interp [Expr] Val -> Interp [Expr] Val #-}
{-# SPECIALIZE Generic.runFixed :: FixpointAlgorithm (Fix (Interp [Expr] Val)) -> Interp [Expr] Val #-}

evalParallel :: (?sensitivity :: Int) => Expr -> (Errors, Terminating Val)
evalParallel e =
  snd $ snd $ Trans.run (Generic.runFixed algorithm :: Interp [Expr] Val) (empty,(empty,[e]))
  where
    algorithm :: FixpointAlgorithm (Fix (Interp [Expr] Val))
    algorithm =
      let ?cacheWidening = (W.finite, W.finite) in
      transform $ Par.parallel $ \update_ ->
        Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
        Fix.filter isFunctionBody update_

evalADI :: (?sensitivity :: Int) => Expr -> (Errors, Terminating Val)
evalADI e =
  snd $ snd $ Trans.run (Generic.runFixed algorithm :: Interp [Expr] Val) (empty,(empty,[e]))
  where
    algorithm :: FixpointAlgorithm (Fix (Interp [Expr] Val))
    algorithm =
      let ?cacheWidening = (W.finite, W.finite) in
      transform $ Par.adi $ \update_ ->
        Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
        Fix.filter isFunctionBody update_
