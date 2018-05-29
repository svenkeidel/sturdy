{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Fix(ArrowFix(..),Fix) where

import Control.Arrow

-- | Arrow-based interface for describing fixpoint computations.
class Arrow c => ArrowFix x y c where
  -- | Computes the fixpoint of an arrow computation.
  fixA :: (c x y -> c x y) -> c x y

instance ArrowFix x y (->) where
  fixA f = f (fixA f)

-- | Computes the type of the fixpoint cache used by 'LeastFixPoint'.
--
-- For the concrete interpreter use 'Fix' with '->' as last component of the arrow transformer stack:
-- @
--   Fix Expr Val (State Store (->)) x y = State Store (->) x y
-- @
--
-- For the abstract interpreter use 'Fix' with '~>' as last component of the arrow transformer stack:
-- @
--   Fix Expr Val (State Store (~>)) x y = State Store (LeastFixPoint (Store,Expr) (Store,Val))
-- @
type family Fix x y (c :: * -> * -> *) :: * -> * -> *
