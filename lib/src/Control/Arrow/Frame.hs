{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Frame where

import Control.Arrow
import Data.Profunctor

class (Arrow c, Profunctor c) => ArrowFrame frame c | c -> frame where
  newFrame :: c x y -> c (frame,x) y
  askFrame :: c () frame
