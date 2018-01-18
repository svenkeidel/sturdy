module Control.Arrow.Fix where

import Control.Arrow

class Arrow c => ArrowFix c where
  fixA :: (c x y -> c x y) -> c x y

