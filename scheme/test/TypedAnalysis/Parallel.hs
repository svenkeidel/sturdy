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
  -fsimpl-tick-factor=500
  -fno-warn-orphans
  -fno-warn-partial-type-signatures
#-}
module TypedAnalysis.Parallel where

import           Prelude hiding (not,Bounded,fail,(.),exp,read)

import           Control.Category
import           Control.Arrow 
import           Control.Arrow.Environment as Env
import           Control.Arrow.Fix(FixpointAlgorithm,FixpointCombinator)
import qualified Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Cache(ArrowCache,ArrowParallelCache,Widening)
import           Control.Arrow.Fix.Parallel as Par
import qualified Control.Arrow.Fix.Context as Ctx
import qualified Control.Arrow.Trans as Trans
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.FiniteEnvStore
import           Control.Arrow.Transformer.Abstract.LogError
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable as Cache
import           Control.Arrow.Transformer.Abstract.Fix.Metrics as Metric
import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow
-- import           Control.Arrow.Transformer.Abstract.Fix.Trace
import           Control.Arrow.Transformer.Abstract.Pow

import           Control.Monad.State hiding (lift,fail)

import           Data.Empty
import           Data.Label
import           Data.Text (Text)
import qualified Data.Abstract.Powerset as Pow 
-- import           Data.Text.Prettyprint.Doc

import qualified Data.Abstract.Widening as W


import           Syntax (Expr(App))
import qualified GenericInterpreter as Generic
import           TypedAnalysis

type Interp x y =
  ValueT Val
    (PowT
      (LogErrorT Text
        (EnvStoreT Text Addr Val
          (FixT
            (MetricsT Metric.Monotone In
              (CacheT (Parallel Cache.MonotoneFactor) In Out
                (ContextT Ctx
                  (ControlFlowT Expr
                    (->))))))))) x y

eval :: (?sensitivity :: Int)
     => (forall c. (?cacheWidening :: Widening c, ArrowChoice c, ArrowCache In Out c, ArrowParallelCache In Out c) =>
                   (FixpointCombinator c In Out -> FixpointCombinator c In Out) -> FixpointAlgorithm (c In Out))
     -> [(Text,Addr)] -> [State Label Expr] -> (CFG Expr, (Metric.Monotone In, Out'))
eval algo env0 e =
  let ?cacheWidening = (storeErrWidening, W.finite) in
  let ?fixpointAlgorithm = transform $ algo $ \update_ ->
        -- Fix.trace printIn printOut .
        -- Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
        -- Fix.recordEvaluated .
        Fix.filter' isFunctionBody update_ in
  second snd $ Trans.run (extend' (Generic.runFixed :: Interp [Expr] Val)) (empty,(empty,Pow.singleton (env0,e0)))
  where
    e0 = generate (sequence e)
{-# INLINE eval #-}

evalParallel :: Eval
evalParallel = eval Par.parallel

evalADI :: Eval
evalADI = eval Par.adi

evalParallel':: Eval'
evalParallel' exprs = let (metrics,(cfg,res)) = evalParallel [] exprs in (metrics,(cfg,snd res))

evalADI':: Eval'
evalADI' exprs = let (metrics,(cfg,res)) = evalADI [] exprs in (metrics,(cfg,snd res))
