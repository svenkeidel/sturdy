module Control.Arrow.Abstract.Terminating where

import Control.Arrow
import Data.Abstract.Terminating
import Data.Profunctor

class (Arrow c, Profunctor c) => ArrowTerminating c where
  throwTerminating :: c (Terminating x) x
  catchTerminating :: c x y -> c x (Terminating y)
