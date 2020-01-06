{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Primitive where

import Control.Arrow
import Control.Arrow.Trans

import Data.Profunctor
import GHC.Exts

class (Arrow c, Profunctor c) => ArrowPrimitive c where
  type PrimState c :: *
  primitive :: ((# State# (PrimState c), x #) -> (# State# (PrimState c), y #)) -> c x y

  default primitive :: (c ~ t c', PrimState c ~ PrimState c', ArrowLift t, ArrowPrimitive c')
                    => ((# State# (PrimState c), x #) -> (# State# (PrimState c), y #)) -> c x y
  primitive f = lift' (primitive f)
  {-# INLINE primitive #-}
