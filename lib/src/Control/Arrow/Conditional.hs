{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Conditional where

import Control.Arrow
import GHC.Exts(Constraint)

-- | Arrow based interface to implement conditionals.
class Arrow c => ArrowCond v c | c -> v where
  type family Join (c :: * -> * -> *) x y :: Constraint

  -- | Performs a case distinction on the given value 'v'. In one case
  -- the first continuation is called and in the other case the second
  -- continuation. An abstract instance might join on the result type 'z'.
  if_ :: Join c (x,y) z => c x z -> c y z -> c (v, (x, y)) z
