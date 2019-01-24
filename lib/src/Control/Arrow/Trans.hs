{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Trans where

import Control.Arrow

class ArrowLift t where
  lift' :: Arrow c => c x y -> t c x y

-- | Lifts an inner computation into an arrow transformer and vice versa.
class ArrowTrans t where
  type Dom t x y :: *
  type Cod t x y :: *

  lift :: Arrow c => c (Dom t x y) (Cod t x y) -> t c x y
  unlift :: Arrow c => t c x y -> c (Dom t x y) (Cod t x y)

type family Rep c x y
