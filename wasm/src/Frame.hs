{-# LANGUAGE Arrows #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Frame where

import           Prelude hiding ((.),read)

import           Control.Category

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader as Reader
import           Control.Arrow.Stack
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.State

import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.Value

import           Data.Monoidal (shuffle1)
import           Data.Profunctor
import           Data.Coerce
import           Data.Vector

import Numeric.Natural (Natural)


-- | A frame has a fixed number of slots of type `v` and some arbitrar
-- | unchangeable frame data `fd`.
class ArrowFrame fd v c | c -> fd, c -> v where
  -- | Runs a computation in a newly created frame given the frame data
  -- | and the initial slot assignment.
  inNewFrame :: c x y -> c (fd, [v], x) y
  frameData :: c () fd
  frameLookup :: c Natural v
  frameUpdate :: c (Natural, v) ()

instance (Profunctor c, Arrow c, ArrowFrame fd v c) => ArrowFrame fd v (StateT val c) where
    -- inNewFrame :: (StateT val c) x y -> (StateT val c) (fd, [v], x) y
    -- a :: (StateT val c) x y
    -- unlift a :: Underlying (StateT val c) x y = c (val,x) (val,y)
    -- inNewFrame :: c x y -> c (fd, [v], x) y
    -- inNewFrame a :: c (fd, [v], (val,x)) (val,y)
    -- lift :: c (val, (fd, [v], x)) (val, y) -> StateT val c (fd, [v], x) y
    inNewFrame a = lift $ shuffle (inNewFrame (unlift a))
        where shuffle arr = proc (val, (fd, vs, x)) -> arr -< (fd, vs, (val,x))
                                        
    frameData = lift' frameData
    frameLookup = lift' frameLookup
    frameUpdate = lift' frameUpdate

instance (Profunctor c, Arrow c, ArrowFrame fd v c) => ArrowFrame fd v (ReaderT r c) where
    inNewFrame (ReaderT a) = ReaderT $ shuffle (inNewFrame a)
        where shuffle arr = proc (r, (fd, v, x)) -> arr -< (fd, v, (r,x))

    frameData = lift' frameData
    frameLookup = lift' frameLookup
    frameUpdate = lift' frameUpdate

-- | Arrow transformer that adds a frame to a computation.
newtype FrameT fd v c x y = FrameT (ReaderT fd (StateT (Vector v) c) x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowLift,--ArrowTrans,
            ArrowFail e,ArrowExcept e,ArrowConst r,
            ArrowStore var' val', ArrowRun, ArrowStack s)

instance (ArrowReader r c) => ArrowReader r (FrameT fd v c) where
    -- ask :: (FrameT fd v c) () r
    ask = FrameT (ReaderT $ proc (fd, ()) -> ask -< ())
    local a = lift $ lmap shuffle1 (local (unlift a))

instance (ArrowChoice c, Profunctor c) => ArrowFrame fd v (FrameT fd v c) where
  inNewFrame (FrameT (ReaderT f)) =
    FrameT $ ReaderT $ proc (_,(fd, vs, x)) -> do
        put -< fromList vs
        f -< (fd, x)
  frameData = FrameT ask
  frameLookup = FrameT $ ReaderT $ proc (_,n) -> do
    vec <- get -< ()
    returnA -< vec ! fromIntegral n
  frameUpdate = FrameT $ ReaderT $ proc (_,(n,v)) -> do
    vec <- get -< ()
    put -< vec // [(fromIntegral n, v)]

deriving instance (ArrowFrame fd v c) => ArrowFrame fd v (ValueT v2 c)

instance ArrowFix (Underlying (FrameT fd v c) x y) => ArrowFix (FrameT fd v c x y) where
    type Fix (FrameT fd v c x y) = Fix (Underlying (FrameT fd v c) x y)--FrameT fd v (Fix c (fd,(Vector v,x)) (Vector v,y))

