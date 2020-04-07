{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
module Control.Arrow.Random where

import Control.Arrow
import Control.Arrow.Trans
import Data.Profunctor

class (Arrow c, Profunctor c) => ArrowRand v c where
  random :: c () v

  default random :: (c ~ t c', ArrowLift t, ArrowRand v c') => c () v
  random = lift' random
