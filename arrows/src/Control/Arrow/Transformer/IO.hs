{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Arrow.Transformer.IO (IO(..)) where

import Prelude hiding (IO)

import Control.Category
import Control.Arrow
import Control.Arrow.Primitive
import Control.Arrow.Trans
import Control.Arrow.Transformer.ST (ST(..))

import Data.Profunctor.Unsafe
import Data.Coerce

import GHC.Prim(RealWorld)
import qualified GHC.Types as T

-- | Arrowized version of the IO monad. Use this arrow at the base of an arrow transformer stack to execute IO operations. For example, as in the arrow transformer stack @StateT Int IO@.
newtype IO x y = IO (ST RealWorld x y)
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowPrimitive)

instance ArrowRun IO where
  type Run IO x y = x -> T.IO y
  run (IO (ST f)) x = T.IO $ \s -> f (# s, x #)
  {-# INLINE run #-}

instance ArrowLift IO where
  type Underlying IO x y = ST RealWorld x y

instance ArrowApply IO where
  app = lift (app .# first coerce)
  {-# INLINE app #-}
