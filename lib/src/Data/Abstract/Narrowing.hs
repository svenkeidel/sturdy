module Data.Abstract.Narrowing where

import Data.Order

class PreOrd a => Narrowing a where
  -- | For △ holds that, i.e. if x ⊑ y then x ⊑ (x △ y) ⊑ y.
  -- Furthermore, iterating ▽ on an descending chain has to stabilize:
  -- Let x1, x2, ... xn be an infinite descending chain, then x1, x1 △ x2, (x1 △ x2) △ x3, ... has a limit.
  -- Notice that widening and narrowing are *not* dual.
  (△) :: a -> a -> a


