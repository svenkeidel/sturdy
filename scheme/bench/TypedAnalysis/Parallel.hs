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
  -fexpose-all-unfoldings
  -funfolding-use-threshold=10000
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
import           Control.Arrow.Transformer.Abstract.LogError
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack as Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable as Cache
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Environment (EnvT)
import           Control.Arrow.Transformer.Abstract.Store (StoreT)
import           Control.Arrow.Transformer.Reader (ReaderT)

import           Data.Text (Text)
import qualified Data.HashMap.Lazy as Map
import           Data.HashSet(HashSet)
import           Data.Empty

import qualified Data.Abstract.Widening as W
import           Data.Abstract.Powerset(Pow)
import           Data.HashMap.Lazy (HashMap)
import           Data.Hashed.Lazy (Hashed)

import           Syntax (Expr(App))
import           GenericInterpreter as Generic
import           TypedAnalysis

type Interp =
  ValueT (Pow Val)
    (LogErrorT Text
      (EnvT (Hashed (HashMap Text Addr))
        (StoreT (HashMap Addr (Pow Val))
          (FixT
            (CacheT (Parallel Cache.GarbageCollect) In Out
              (ContextT Ctx
                (ReaderT (HashSet Addr)
                (->))))))))

{-# SPECIALIZE if__ :: (ArrowComplete z Interp)
                    => Interp x z -> Interp y z -> Interp (Val,(x,y)) z #-}
{-# SPECIALIZE Generic.eval :: Interp [Expr] (Pow Val) -> Interp Expr (Pow Val) #-}
{-# SPECIALIZE Generic.run :: Interp Expr (Pow Val) -> Interp [Expr] (Pow Val) -> Interp [Expr] (Pow Val) #-}
{-# SPECIALIZE Generic.runFixed :: (?fixpointAlgorithm :: FixpointAlgorithm (Fix (Interp [Expr] (Pow Val)))) => Interp [Expr] (Pow Val) #-}

evalParallel :: (?sensitivity :: Int) => Expr -> (Errors, (Pow Val))
evalParallel e =
  let ?cacheWidening = (storeErrWidening_nm, W.finite) in
  let ?fixpointAlgorithm = transform $ Par.parallel $ \update_ ->
        Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
        Fix.filter isFunctionBody update_ in
  snd $ snd $ Trans.run (Generic.runFixed :: Interp [Expr] (Pow Val)) (empty,(empty,(empty,[e])))

evalADI :: (?sensitivity :: Int) => Expr -> (Errors, (Pow Val))
evalADI e =
  let ?cacheWidening = (storeErrWidening, W.finite) in
  let ?fixpointAlgorithm = transform $ Par.adi $ \update_ ->
        Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
        Fix.filter isFunctionBody update_ in
  snd $ snd $ Trans.run (Generic.runFixed :: Interp [Expr] (Pow Val)) (empty,(empty,(empty,[e])))
