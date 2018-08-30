module Control.Arrow.Abstract.Join where

import Prelude hiding ((.))

import Control.Arrow

class Arrow c => ArrowJoin c where
  -- | Join two arrow computation with the provided upper bound operator.
  --
  -- Laws:
  -- @
  --   joinWith (⊔) f g = joined f g
  -- @
  joinWith :: (z -> z -> z) -> c x z -> c y z -> c (x,y) z

instance ArrowJoin (->) where
  joinWith lub f g = \(x,y) -> lub (f x) (g y)
