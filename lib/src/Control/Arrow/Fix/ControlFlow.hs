{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Fix.ControlFlow where

import Control.Arrow
import Control.Arrow.Fix
import Data.Profunctor

class (Arrow c, Profunctor c) => ArrowControlFlow stmt c | c -> stmt where
  -- | Adds a control-flow edge between the previously evaluated statement and the next statement.
  -- For example, @(nextStatement -< e1; nextStatement -< e2)@ adds an CFG edge between @e1@ and @e2@.
  nextStatement :: c x y -> c (Maybe stmt, x) y

-- | Records the trace of the abstract interpreter as an control-flow graph.
recordControlFlowGraph :: ArrowControlFlow stmt c => (a -> stmt) -> FixpointCombinator c a b
recordControlFlowGraph getStatement f = proc a -> nextStatement f -< (Just (getStatement a),a)
{-# INLINE recordControlFlowGraph #-}

-- | Records the trace of the abstract interpreter as an control-flow graph.
recordControlFlowGraph' :: (ArrowChoice c, ArrowControlFlow stmt c) => (a -> Maybe stmt) -> FixpointCombinator c a b
recordControlFlowGraph' getStatement f = proc a -> nextStatement f -< (getStatement a,a)
{-# INLINE recordControlFlowGraph' #-}
