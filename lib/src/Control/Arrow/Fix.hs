{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Fix(ArrowFix(..),Fix,liftFix,liftFix') where

import Control.Arrow

-- | Arrow-based interface for describing fixpoint computations.
class Arrow c => ArrowFix x y c where
  -- | Computes the fixpoint of an arrow computation.
  fix :: (c x y -> c x y) -> c x y

instance ArrowFix x y (->) where
  fix f = f (fix f)

-- | Computes the type of the fixpoint cache used by 'LeastFixPoint'.
--
-- For the concrete interpreter use 'Fix' with '->' as last component of the arrow transformer stack:
-- @
--   Fix Expr Val (State Store (->)) x y = State Store (->) x y
-- @
--
-- For the abstract interpreter use 'Fix' with '~>' as last component of the arrow transformer stack:
-- @
--   Fix Expr Val (State Store (LeastFix () ())) x y = State Store (LeastFixPoint (Store,Expr) (Store,Val))
-- @
type family Fix x y (c :: * -> * -> *) :: * -> * -> *
type instance Fix a b (->) = (->)

-- | Generic lifting operation for the fixpoint operator 'fix'.
-- Example usage: fix = liftFix State runState
liftFix :: ArrowFix (f x) (g y) c
        => (t c x y -> c (f x) (g y))
        -> (c (f x) (g y) -> t c x y)
        -> ((t c x y -> t c x y) -> t c x y)
liftFix unwrap wrap f = wrap $ fix (unwrap . f . wrap)

-- | Generic lifting operation for the fixpoint operator 'fix'.
-- Example usage: fix = liftFix Except runExcept
liftFix' :: ArrowFix x (g y) c
        => (t c x y -> c x (g y))
        -> (c x (g y) -> t c x y)
        -> ((t c x y -> t c x y) -> t c x y)
liftFix' unwrap wrap f = wrap $ fix (unwrap . f . wrap)
