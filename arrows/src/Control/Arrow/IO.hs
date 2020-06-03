{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.IO where

import Control.Arrow.Primitive

import Data.Coerce

import GHC.Types(IO(..))
import GHC.Prim(RealWorld)

type ArrowIO c = (ArrowPrimitive c, PrimState c ~ RealWorld)

-- | Lift an IO computation into an arrow computation.
-- Example usage:
-- @
--   proc msg -> do
--     liftIO print -< msg
-- @
liftIO :: ArrowIO c => (x -> IO y) -> c x y
liftIO f = primitive (\(# s,x #) -> coerce f x s)
{-# INLINE liftIO #-}
