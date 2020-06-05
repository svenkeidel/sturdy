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

-- | Arrowized version of the @PrimMonad@ type class of the <http://hackage.haskell.org/package/primitive/ primitive> package.
-- This type class is useful for lifting stateful and IO operations into an
-- arrow computation.
class (Arrow c, Profunctor c) => ArrowPrimitive c where
  type PrimState c
  primitive :: ((# State# (PrimState c), x #) -> (# State# (PrimState c), y #)) -> c x y

  default primitive :: (c ~ t c', PrimState c ~ PrimState c', ArrowTrans t, ArrowPrimitive c')
                    => ((# State# (PrimState c), x #) -> (# State# (PrimState c), y #)) -> c x y
  primitive f = lift' (primitive f)
  {-# INLINE primitive #-}

-- | Lifts an stateful operation into an arrow computation.
liftST :: ArrowPrimitive c => (x -> ST (PrimState c) y) -> c x y
liftST f = primitive (\(# s,x #) -> coerce f x s)
{-# INLINE liftST #-}
