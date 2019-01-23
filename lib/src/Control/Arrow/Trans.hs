{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Trans where

import Control.Arrow

type family Dom (c :: * -> * -> *) x y :: *
type family Cod (c :: * -> * -> *) x y :: *

type instance Dom (->) x y = x
type instance Cod (->) x y = y

class ArrowLift t where
  lift' :: Arrow c => c x y -> t c x y

-- | Lifts an inner computation into an arrow transformer and vice versa.
class ArrowTrans t where
  type Dom1 t x y :: *
  type Cod1 t x y :: *

  lift :: Arrow c => c (Dom1 t x y) (Cod1 t x y) -> t c x y
  unlift :: Arrow c => t c x y -> c (Dom1 t x y) (Cod1 t x y)
