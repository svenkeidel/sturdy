{-# LANGUAGE Arrows #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.WasmFrame where

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

import           Control.Arrow.Transformer.Abstract.Error
import qualified Control.Arrow.Transformer.Abstract.Except as AE
import qualified Control.Arrow.Transformer.Abstract.Store as AbsStore
import qualified Control.Arrow.Transformer.Concrete.Store as ConcStore
import qualified Control.Arrow.Transformer.Concrete.Except as CE
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
--import           Control.Arrow.Transformer.Stack
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Writer

import           Data.Monoidal (shuffle1)
import qualified Data.Order as O
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
    -- inNewFrame (unlift a) :: c (fd, [v], (val,x)) (val,y)
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


deriving instance (ArrowFrame fd v c) => ArrowFrame fd v (ValueT v2 c)
deriving instance (Profunctor c, Arrow c, ArrowFrame fd v c) => ArrowFrame fd v (CE.ExceptT e c)
deriving instance (O.Complete e, Profunctor c, Arrow c, ArrowFrame fd v c) => ArrowFrame fd v (AE.ExceptT e c)
deriving instance (Profunctor c, Arrow c, ArrowFrame fd v c) => ArrowFrame fd v (ErrorT e c)
instance (Monad f, Profunctor c, Arrow c, ArrowFrame fd v c) => ArrowFrame fd v (KleisliT f c) where
    inNewFrame a = lift (inNewFrame (unlift a))
    frameData = lift' frameData
    frameLookup = lift' frameLookup
    frameUpdate = lift' frameUpdate
--deriving instance (Profunctor c, Arrow c, ArrowFrame fd v c) => ArrowFrame fd v (StackT s c)
instance (Profunctor c, Arrow c, ArrowFrame fd v c, Monoid w) => ArrowFrame fd v (WriterT w c) where
    inNewFrame a = lift (inNewFrame (unlift a))
    frameData = lift' frameData
    frameLookup = lift' frameLookup
    frameUpdate = lift' frameUpdate

deriving instance (Profunctor c, Arrow c, ArrowFrame fd v c) => ArrowFrame fd v (AbsStore.StoreT s c)
