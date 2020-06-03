{-# LANGUAGE MultiParamTypeClasses #-}
-- | This module contains the arrowized versions of the 'Functor', 'Monad' and
-- 'Comonad' type classes.
--
-- We use these type classes to implement the arrow instances for
-- 'Control.Arrow.Transformer.Kleisli.KleisliT' and
-- 'Control.Arrow.Transformer.Cokleisli.CokleisliT' in a modular way.
module Control.Arrow.Monad where

import Control.Arrow
import Control.Monad (join)
import Control.Comonad
import Data.Profunctor

-- | Arrowized version of the 'Functor' type class.
class (Functor f, Arrow c, Profunctor c) => ArrowFunctor f c where
  -- | Maps a computation with a functor.
  mapA :: c x y -> c (f x) (f y)

-- | Arrowized version of the 'Monad' type class.
class (Monad f, ArrowFunctor f c) => ArrowMonad f c where
  -- | Returns an element wrapped inside the monad.
  unitA :: c x (f x)
  unitA = arr return

  -- | Joins two layers of the monad into one.
  joinA :: c (f (f x)) (f x)
  joinA = arr join

  -- | Combines mapping and joining a monadic computation. This can be
  -- implemented more efficiently than calling 'mapA' and 'joinA' separately.
  mapJoinA :: c x (f y) -> c (f x) (f y)
  mapJoinA f = rmap join (mapA f) 
  {-# INLINE unitA #-}
  {-# INLINE joinA #-}
  {-# INLINE mapJoinA #-}

-- | Arrowized version of the 'Comonad' type class.
class (Comonad f, ArrowFunctor f c) => ArrowComonad f c where
  -- | Extracts an element from the comonad.
  extractA :: c (f x) x
  extractA = arr extract

  -- | Duplicates two layers of the comonad.
  duplicateA :: c (f x) (f (f x))
  duplicateA = arr duplicate

  -- | Combines mapping and duplicating a comonadic computation. This can be
  -- implemented more efficiently than calling 'mapA' and 'duplicateA' separately.
  mapDuplicateA :: c (f x) y -> c (f x) (f y)
  mapDuplicateA f = lmap duplicate (mapA f) 
  {-# INLINE extractA #-}
  {-# INLINE duplicateA #-}
  {-# INLINE mapDuplicateA #-}
