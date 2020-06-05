{-# LANGUAGE Arrows #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
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
module TypedAnalysis.Chaotic where

import           Prelude hiding (not,Bounded,fail,(.),exp,read,IO)
import qualified Prelude as P

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Environment as Env
import qualified Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Chaotic(IterationStrategy,chaotic,innermost',outermost')
import qualified Control.Arrow.Fix.Context as Ctx
import           Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.IO
import qualified Control.Arrow.Trans as Trans
import           Control.Arrow.Transformer.IO
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.FiniteEnvStore hiding (Env)
import           Control.Arrow.Transformer.Abstract.LogError
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Component as Comp
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack as Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable as Cache
import           Control.Arrow.Transformer.Abstract.Fix.Metrics as Metric
import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow
import           Control.Arrow.Transformer.Abstract.Terminating

import           Data.Empty
import           Data.Label
import           Data.Text (Text)

import qualified Data.Abstract.Widening as W
import           Data.Abstract.Terminating(Terminating)

import           TypedAnalysis
import           Syntax (LExpr,Expr(App))
import           GenericInterpreter as Generic

type InterpT c x y =
  (ValueT Val
    (TerminatingT
      (LogErrorT Text
        (EnvStoreT Text Addr Val
          (FixT
            (MetricsT Metric.Monotone In
             (ComponentT Comp.Component  In
               (StackT Stack.Stack In
                 (CacheT Cache.Monotone In Out
                   (ContextT Ctx
                     (ControlFlowT Expr
                       c))))))))))) x y

evalChaotic :: (?sensitivity :: Int) => IterationStrategy _ In Out -> [(Text,Addr)] -> [LExpr] -> (CFG Expr, (Metric.Monotone In, Out'))
evalChaotic iterationStrat env0 e =
  let ?cacheWidening = (storeErrWidening, W.finite) in
  let ?fixpointAlgorithm = transform $
        Fix.fixpointAlgorithm $
        -- Fix.trace printIn printOut .
        Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
        Fix.recordEvaluated .
        -- CFlow.recordControlFlowGraph' (\(_,(_,exprs)) -> case exprs of e':_ -> Just e'; _ -> Nothing) .
        -- Fix.filter' isFunctionBody (Fix.trace printIn printOut . chaotic iterationStrat)
        Fix.filter' isFunctionBody (chaotic iterationStrat) in
  second snd $ Trans.run (extend' (Generic.runFixed :: InterpT (->) [Expr] Val)) (empty,(empty,(env0,e0)))
  where
    e0 = generate (sequence e)
{-# INLINE evalChaotic #-}

evalInner :: Eval
evalInner = evalChaotic innermost'

evalOuter :: Eval
evalOuter = evalChaotic outermost'

evalInner' :: Eval'
evalInner' exprs = let (metrics,(cfg,res)) = evalInner [] exprs in (metrics,(cfg,snd res))

evalOuter':: Eval'
evalOuter' exprs = let (metrics,(cfg,res)) = evalOuter [] exprs in (metrics,(cfg,snd res))

eval' :: (?sensitivity :: Int) => [(Text,Addr)] -> [LExpr] -> (Errors,Terminating Val)
eval' env exprs = snd $ snd $ snd $ evalInner env exprs

type Socket = () -- Placeholder
type Breakpoints = [Expr] -- Placeholder
type StackElems = [In] -- Placeholder
data DebugState = DebugState { socket :: Socket, breakpoints :: Breakpoints }

data Message
  = ReachedBreakpoint {breakpoint :: Expr, env :: Env}
  | Continue
  | GetStackRequest
  | GetStackResponse StackElems

-- | Main entry point for the debugger
startDebugger :: [LExpr] -> P.IO ()
startDebugger expr = do
  _setupDebugState -- TODO: Implement
  let ?sensitivity = 0
  let ?debugState = _debugState -- TODO: Implement
  evalDebug expr
  -- socket <- createSocket -< 1234
  -- loop
  -- where
  --   loop = do
  --     msg <- Client.receive -< ()
  --     case msg of
  --       StartProgram file breakpoints -> do
  --         prog <- loadSchemeFile file
  --         let ?sensitivity = 0
  --         let debugState = DebugState socket breakpoints
  --         evalDebug debugState prog
  --         loop
  --       _ ->
  --         closeSocket socket

evalDebug :: (?sensitivity::Int, ?debugState::DebugState) => [LExpr] -> P.IO ()
evalDebug expr =
  let ?cacheWidening = (storeErrWidening, W.finite) in
  let ?fixpointAlgorithm = transform $
       Fix.fixpointAlgorithm $
         debug .
         Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
         Fix.filter' isFunctionBody (chaotic innermost') in
  do _ <- Trans.run (extend' (Generic.runFixed :: InterpT IO [Expr] Val)) (empty,(empty,([],e0)))
     return ()
  where
    e0 = generate (sequence expr)

-- | Send message to debugging client
send :: (?debugState :: DebugState) => Message -> P.IO ()
send = _send -- TODO: Implement

-- | Receive message from debugging client
receive :: (?debugState :: DebugState) => () -> P.IO Message
receive = _receive -- TODO: Implement

-- | Debugging combinator
debug :: (?debugState :: DebugState,
          ArrowChoice c, ArrowIO c, ArrowStackElements In c)
      => Fix.FixpointCombinator c ((Store,Errors),(Env,[Expr]))
                                  ((Store,Errors), Terminating Val)
debug f = proc input@((store,errors),(env,exprs)) -> do
  case exprs of
    expr:_ | expr `P.elem` breakpoints ?debugState -> do
      -- We reached a breakpoint
      liftIO send -< ReachedBreakpoint expr env
      loop -< input
      f -< input
    _ ->
      f -< input
  where
    loop = proc input@((store,errors),(env,expr)) -> do
      msg <- liftIO receive -< ()
      case msg of
        Continue ->
          returnA -< ()
        GetStackRequest -> do
          stack <- Stack.elems -< ()
          liftIO send -< GetStackResponse stack
          loop -< input
