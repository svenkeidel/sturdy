{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Random where

import Control.Arrow
import Data.Profunctor

class (Arrow c, Profunctor c) => ArrowRand v c where
  random :: c () v
