{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Random where

import Control.Arrow

class Arrow c => ArrowRand v c where
  random :: c () v
