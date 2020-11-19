{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Control.Arrow.Transformer.Const where

import Prelude hiding (id,(.),lookup,read)

import Control.Category

import Control.Arrow
import Control.Arrow.Cont
import Control.Arrow.Const
import Control.Arrow.Primitive
import Control.Arrow.Trans
import Control.Arrow.Reader as Reader
import Control.Arrow.State as State
import Control.Arrow.Writer as Writer

import Data.Profunctor.Unsafe
import Unsafe.Coerce

-- | Arrow transformer that passes along constant data.
newtype ConstT r c x y = ConstT (r -> c x y)

constT :: (r -> c x y) -> ConstT r c x y
constT = lift
{-# INLINE constT #-}

runConstT :: r -> ConstT r c x y -> c x y
runConstT r f = unlift f r
{-# INLINE runConstT #-}

liftConstT :: (c x y -> c' x' y') -> ConstT r c x y -> ConstT r c' x' y'
liftConstT f g = lift $ \r -> f (unlift g r)
{-# INLINE liftConstT #-}

unliftConstT :: r -> (ConstT r c x y -> ConstT r c' x' y') -> (c x y -> c' x' y')
unliftConstT r f x = runConstT r (f (constT $ const x))
{-# INLINE unliftConstT #-}

mapConstT :: (r' -> r) -> (ConstT r c x y -> ConstT r' c x y)
mapConstT g f = constT $ \r' -> unlift f (g r')
{-# INLINE mapConstT #-}

setConstT :: r -> (ConstT r c x y -> ConstT r' c x y)
setConstT r = mapConstT (const r)
{-# INLINE setConstT #-}

instance ArrowLift (ConstT r c) where
  type Underlying (ConstT r c) x y = r -> c x y

instance ArrowRun c => ArrowRun (ConstT r c) where
  type Run (ConstT r c) x y = r -> Run c x y
  run f r = run (runConstT r f)
  {-# INLINE run #-}

instance (Arrow c, Profunctor c) => ArrowConst r (ConstT r c) where
  askConst f = lift $ \r -> unlift (f r) r
  {-# INLINE askConst #-}

instance (ArrowApply c, Profunctor c) => ArrowApply (ConstT r c) where
  app = lift $ \r -> lmap (\(f,x) -> (unlift f r,x)) app
  {-# INLINE app #-}

instance ArrowCont c => ArrowCont (ConstT r c) where
  type Cont (ConstT r c) y = Cont c y
  callCC f = lift $ \r -> callCC $ \k -> unlift (f k) r
  jump k = lift $ \_ -> jump k
  {-# INLINE callCC #-}

instance Category c => Category (ConstT r c) where
  id = lift $ \_ -> id
  f . g = lift $ \r -> unlift f r . unlift g r
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance Profunctor c => Profunctor (ConstT r c) where
  dimap f g h = lift $ \r -> dimap f g (unlift h r)
  lmap f h = lift $ \r -> lmap f (unlift h r)
  rmap f h = lift $ \r -> rmap f (unlift h r)
  f .# _ = f `seq` unsafeCoerce f
  _ #. g = g `seq` unsafeCoerce g
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}
  {-# INLINE (.#) #-}
  {-# INLINE ( #.) #-}

instance Arrow c => Arrow (ConstT r c) where
  arr f = lift $ \_ -> arr f
  first f = lift $ \r -> first (unlift f r)
  second f = lift $ \r -> second (unlift f r)
  f *** g = lift $ \r -> unlift f r *** unlift g r
  f &&& g = lift $ \r -> unlift f r &&& unlift g r
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (***) #-}

instance ArrowChoice c => ArrowChoice (ConstT r c) where
  left f = lift $ \r -> left (unlift f r)
  right f = lift $ \r -> right (unlift f r)
  f +++ g = lift $ \r -> unlift f r +++ unlift g r
  f ||| g = lift $ \r -> unlift f r ||| unlift g r
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (+++) #-}
  {-# INLINE (|||) #-}

instance ArrowTrans (ConstT r) where
  lift' f = lift $ \_ -> f
  {-# INLINE lift' #-}

instance ArrowPrimitive c => ArrowPrimitive (ConstT r c) where
  type PrimState (ConstT r c) = PrimState c

instance ArrowState s c => ArrowState s (ConstT r c) where
  get = lift $ \_ -> State.get
  put = lift $ \_ -> State.put
  modify f = lift $ \r -> State.modify (unlift f r)
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE modify #-}

instance ArrowReader r' c => ArrowReader r' (ConstT r c) where
  ask = lift $ \_ -> Reader.ask
  local f = lift $ \r -> Reader.local (unlift f r)
  {-# INLINE ask #-}
  {-# INLINE local #-}

instance ArrowWriter w c => ArrowWriter w (ConstT r c) where
  tell = lift $ \_ -> Writer.tell
  {-# INLINE tell #-}

