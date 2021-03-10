{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Transformer.Abstract.WasmFrame where

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Order
import           Control.Arrow.Reader
import           Control.Arrow.Stack
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Arrow.Trans
import           Control.Arrow.WasmFrame

import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State

import           Control.Category hiding (id)

import           Data.Monoidal (shuffle1)
import           Data.Order
import           Data.Profunctor
import qualified Data.Vector as Vec

newtype Vector v = Vector (Vec.Vector v) deriving (Show,Eq)

instance (PreOrd v) => PreOrd (Vector v) where
    (Vector v1) ⊑ (Vector v2) = all id $ Vec.zipWith (⊑) v1 v2

instance (Complete v) => Complete (Vector v) where
    (Vector v1) ⊔ (Vector v2) = Vector $ Vec.zipWith (⊔) v1 v2

-- | Arrow transformer that adds a frame to a computation.
newtype FrameT fd v c x y = FrameT (ReaderT fd (StateT (Vector v) c) x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowLift,--ArrowTrans,
            ArrowFail e,ArrowExcept e,ArrowConst r,
            ArrowStore var' val', ArrowRun, ArrowStack s, ArrowJoin)

instance (ArrowReader r c) => ArrowReader r (FrameT fd v c) where
    -- ask :: (FrameT fd v c) () r
    ask = FrameT (ReaderT $ proc (fd, ()) -> ask -< ())
    local a = lift $ lmap shuffle1 (local (unlift a))

instance (ArrowChoice c, Profunctor c) => ArrowFrame fd v (FrameT fd v c) where
  inNewFrame (FrameT (ReaderT f)) =
    FrameT $ ReaderT $ proc (_,(fd, vs, x)) -> do
        put -< Vector $ Vec.fromList vs
        f -< (fd, x)
  frameData = FrameT ask
  frameLookup = FrameT $ ReaderT $ proc (_,n) -> do
    (Vector vec) <- get -< ()
    returnA -< vec Vec.! fromIntegral n
  frameUpdate = FrameT $ ReaderT $ proc (_,(n,v)) -> do
    (Vector vec) <- get -< ()
    put -< Vector $ vec Vec.// [(fromIntegral n, v)]

deriving instance (Arrow c, Profunctor c, ArrowComplete (Vector v,y) c) => ArrowComplete y (FrameT fd v c)

instance ArrowFix (Underlying (FrameT fd v c) x y) => ArrowFix (FrameT fd v c x y) where
    type Fix (FrameT fd v c x y) = Fix (Underlying (FrameT fd v c) x y)--FrameT fd v (Fix c (fd,(Vector v,x)) (Vector v,y))
