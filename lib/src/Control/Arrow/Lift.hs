module Control.Arrow.Lift where

import Control.Arrow

-- | Lifts an inner computation into an arrow transformer.
class ArrowLift c where
  lift :: Arrow d => d x y -> c d x y
