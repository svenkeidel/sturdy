{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Conditional where

import Control.Arrow

-- | Arrow based interface to implement conditionals.
class Arrow c => ArrowCond v x y z c where
  -- | Performs a case distinction on the given value 'v'. In one case
  -- the first continuation is called and in the other case the second
  -- continuation. An abstract instance might join on the result type 'z'.
  if_ :: c x z -> c y z -> c (v, (x, y)) z
