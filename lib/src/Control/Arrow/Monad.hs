{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Monad where

import Control.Arrow
import Control.Monad (join)
import Data.Profunctor

class (Functor f, Arrow c, Profunctor c, Arrow d, Profunctor d) => ArrowFunctor f c d where
  mapA :: c x y -> d (f x) (f y)

class (Monad f, ArrowFunctor f c c) => ArrowMonad f c where
  unitA :: c x (f x)
  unitA = arr return

  joinA :: c (f (f x)) (f x)
  joinA = arr join

  mapJoinA :: c x (f y) -> c (f x) (f y)
  mapJoinA f = rmap join (mapA f) 
