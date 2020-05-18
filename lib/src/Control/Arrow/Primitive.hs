{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Primitive where

import Control.Arrow
import Control.Arrow.Trans

import Data.Coerce
import Data.Profunctor

import GHC.Exts
import GHC.ST(ST(..))

class (Arrow c, Profunctor c) => ArrowPrimitive c where
  type PrimState c
  primitive :: ((# State# (PrimState c), x #) -> (# State# (PrimState c), y #)) -> c x y

  default primitive :: (c ~ t c', PrimState c ~ PrimState c', ArrowLift t, ArrowPrimitive c')
                    => ((# State# (PrimState c), x #) -> (# State# (PrimState c), y #)) -> c x y
  primitive f = lift' (primitive f)
  {-# INLINE primitive #-}

liftST :: ArrowPrimitive c => (x -> ST (PrimState c) y) -> c x y
liftST f = primitive (\(# s,x #) -> coerce f x s)
{-# INLINE liftST #-}
