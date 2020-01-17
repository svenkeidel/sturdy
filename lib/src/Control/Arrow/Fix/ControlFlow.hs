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
  nextStatement :: c stmt ()

  default nextStatement :: (c ~ t c', ArrowLift t, ArrowControlFlow stmt c') => c stmt ()
  nextStatement = lift' nextStatement

-- | Records the trace of the abstract interpreter as an control-flow graph.
recordControlFlowGraph :: ArrowControlFlow stmt c => (a -> stmt) -> FixpointCombinator c a b
recordControlFlowGraph getStatement f = proc a -> do
  nextStatement -< getStatement a
  f -< a
{-# INLINE recordControlFlowGraph #-}
