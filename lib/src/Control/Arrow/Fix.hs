{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DefaultSignatures #-}
module Control.Arrow.Fix(ArrowFix(..),liftFix) where

import Control.Arrow
import Control.Arrow.Trans

-- | Arrow-based interface for describing fixpoint computations.
class Arrow c => ArrowFix x y c where

  -- | Computes the fixpoint of an arrow computation.
  fix :: (c x y -> c x y) -> c x y

liftFix :: (ArrowFix (Dom1 t x y) (Cod1 t x y) c,ArrowTrans t) => (t c x y -> t c x y) -> t c x y
liftFix f = lift $ fix (unlift . f . lift)
