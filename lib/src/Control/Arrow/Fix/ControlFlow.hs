{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Fix.ControlFlow where

import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Trans
import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.Reader
import Control.Arrow.Transformer.State
import Control.Arrow.Transformer.Static
import Control.Arrow.Transformer.Writer

import Data.Profunctor
import Data.Monoidal

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

------------- Instances --------------
instance ArrowControlFlow stmt c => ArrowControlFlow stmt (ConstT r c) where
  nextStatement f = lift $ \r -> nextStatement (unlift f r)
  {-# INLINE nextStatement #-}

instance ArrowControlFlow stmt c => ArrowControlFlow stmt (ReaderT r c) where
  nextStatement f = lift $ lmap shuffle1 (nextStatement (unlift f))
  {-# INLINE nextStatement #-}

instance ArrowControlFlow stmt c => ArrowControlFlow stmt (StateT s c) where
  nextStatement f = lift $ lmap shuffle1 (nextStatement (unlift f))
  {-# INLINE nextStatement #-}

instance (Applicative f, ArrowControlFlow stmt c) => ArrowControlFlow stmt (StaticT f c) where
  nextStatement (StaticT f) = StaticT $ nextStatement <$> f
  {-# INLINE nextStatement #-}
  {-# SPECIALIZE instance ArrowControlFlow stmt c => ArrowControlFlow stmt (StaticT ((->) r) c) #-}

instance (Monoid w, ArrowControlFlow stmt c) => ArrowControlFlow stmt (WriterT w c) where
  nextStatement f = lift (nextStatement (unlift f))
  {-# INLINE nextStatement #-}
