module Control.Arrow.Lift where

import Control.Arrow

class ArrowLift c where
  lift :: Arrow d => d x y -> c d x y
