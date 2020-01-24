{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Fix.ControlFlow where

import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Trans
import Data.Profunctor

class (Arrow c, Profunctor c) => ArrowControlFlow stmt c | c -> stmt where
  -- | Adds a control-flow edge between the previously evaluated statement and the next statement.
  -- For example, @(nextStatement -< e1; nextStatement -< e2)@ adds an CFG edge between @e1@ and @e2@.

  -- nextStatement :: c x y -> c (stmt, x) y 
  nextStatement :: c stmt ()

  default nextStatement :: (c ~ t c', ArrowLift t, ArrowControlFlow stmt c') => c stmt ()
  nextStatement = lift' nextStatement

-- | Records the trace of the abstract interpreter as an control-flow graph.
recordControlFlowGraph :: ArrowControlFlow stmt c => (a -> stmt) -> FixpointCombinator c a b
recordControlFlowGraph getStatement f = proc a -> do
  nextStatement -< getStatement a
  f -< a
{-# INLINE recordControlFlowGraph #-}

-- | Records the trace of the abstract interpreter as an control-flow graph.
recordControlFlowGraph' :: (ArrowChoice c, ArrowControlFlow stmt c) => (a -> Maybe stmt) -> FixpointCombinator c a b
recordControlFlowGraph' getStatement f = proc a -> do
  case getStatement a of 
    Just stmt -> do 
      nextStatement -< stmt
      f -< a
    _ -> f -< a
{-# INLINE recordControlFlowGraph' #-}


-- recordCallsite :: forall a lab b c. ArrowContext (CallString lab) c => Int -> (a -> Maybe lab) -> FixpointCombinator c a b
-- recordCallsite k getLabel g = proc a -> do
--   callString <- askContext -< ()
--   let callString' = case getLabel a of
--         Just lab -> CallString.truncate k (CallString.push lab callString)
--         Nothing -> callString
--   localContext g -< (callString',a)
-- {-# INLINE recordCallsite #-}