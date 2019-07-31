{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Monad where

import Control.Arrow
import Control.Monad (join)
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
