{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Frame where

import Control.Arrow
import Control.Arrow.Trans
import Data.Profunctor

class (Arrow c, Profunctor c) => ArrowFrame frame c | c -> frame where
  askFrame :: c () frame
  newFrame :: c x y -> c (frame,x) y

  default askFrame :: (c ~ t c', ArrowLift t, ArrowFrame frame c') => c () frame
  askFrame = lift' askFrame
  {-# INLINE askFrame #-}
