module Control.Arrow.Fix where

class ArrowFix c where
  fixA :: (c x y -> c x y) -> c x y
