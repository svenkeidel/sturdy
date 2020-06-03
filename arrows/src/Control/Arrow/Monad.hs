{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Monad where

import Control.Arrow
import Control.Monad (join)
import Control.Comonad
import Data.Profunctor

class (Functor f, Arrow c, Profunctor c) => ArrowFunctor f c where
  mapA :: c x y -> c (f x) (f y)

class (Monad f, ArrowFunctor f c) => ArrowMonad f c where
  unitA :: c x (f x)
  unitA = arr return

  joinA :: c (f (f x)) (f x)
  joinA = arr join

  mapJoinA :: c x (f y) -> c (f x) (f y)
  mapJoinA f = rmap join (mapA f) 
  {-# INLINE unitA #-}
  {-# INLINE joinA #-}
  {-# INLINE mapJoinA #-}


class (Comonad f, ArrowFunctor f c) => ArrowComonad f c where
  extractA :: c (f x) x
  extractA = arr extract

  duplicateA :: c (f x) (f (f x))
  duplicateA = arr duplicate

  mapDuplicateA :: c (f x) y -> c (f x) (f y)
  mapDuplicateA f = lmap duplicate (mapA f) 
  {-# INLINE extractA #-}
  {-# INLINE duplicateA #-}
  {-# INLINE mapDuplicateA #-}
