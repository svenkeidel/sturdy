{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Conditional where

import Control.Arrow
import GHC.Exts(Constraint)

-- | Arrow based interface to implement conditionals.
class Arrow c => ArrowCond v c | c -> v where

  -- | Type class constraint used by the abstract instances to join arrow computations.
  type family Join (c :: * -> * -> *) x y :: Constraint

  -- | @'if_' f g -< (v,(x,y))@ performs a case distinction on the given value @v@ and executes either @(f -< x)@ or @(g -< y)@. Abstract instances might join the results of @f@ and @g@.
  if_ :: Join c (x,y) z => c x z -> c y z -> c (v, (x, y)) z
