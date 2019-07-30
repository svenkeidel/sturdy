{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DefaultSignatures #-}
module Control.Arrow.Fix(Fix,ArrowFix(..),liftFix,IterationStrategy) where

import Control.Arrow
import Control.Arrow.Trans
import Data.Profunctor

-- | Type family that computes the type of the fixpoint.
type family Fix x y (c :: * -> * -> *) :: * -> * -> *

-- | Interface for describing fixpoint computations.
class (Arrow c, Profunctor c) => ArrowFix x y c where
  -- | Computes the fixpoint of an arrow computation.
  fix :: (c x y -> c x y) -> c x y

type instance Fix x y (->) = (->)
instance ArrowFix x y (->) where
  fix f = f (fix f)

liftFix :: (Arrow c, Profunctor c, ArrowFix (Dom t x y) (Cod t x y) c,ArrowTrans t) => (t c x y -> t c x y) -> t c x y
liftFix f = lift $ fix (unlift . f . lift)
{-# INLINE liftFix #-}

type IterationStrategy c a b = c a b -> c a b
