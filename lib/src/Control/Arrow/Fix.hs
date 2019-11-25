{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Fix(Fix,Fix',ArrowFix(..)) where

import Prelude hiding (filter,pred)

import Control.Arrow.Trans

-- | Type family that computes the type of the fixpoint.
type family Fix (c :: * -> * -> *) x y :: * -> * -> *
type Fix' c x y = Fix c x y x y

-- | Interface for describing fixpoint computations.
class ArrowFix c where
  -- | Computes the fixpoint of an arrow computation.
  fix :: (c -> c) -> c

  default fix :: (c ~ c' x y, ArrowTrans c', Underlying c' x y ~ c'' x' y', ArrowFix (c'' x' y')) => (c -> c) -> c
  fix f = lift (fix (unlift . f . lift))
  {-# INLINE fix #-}

type instance Fix (->) x y = (->)
instance ArrowFix (x -> y) where
  fix f = f (fix f)
