{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Kleisli where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Category
import Control.Arrow hiding (ArrowMonad)
import Control.Arrow.Cont
import Control.Arrow.Const
import Control.Arrow.Monad
import Control.Arrow.Primitive as Prim
import Control.Arrow.Reader as Reader
import Control.Arrow.State as State
import Control.Arrow.Trans

import Data.Monoidal
import Data.Profunctor.Unsafe
import Data.Coerce
import Unsafe.Coerce

-- | Arrow transformer that adds a monadic effect to the output of a computation.
-- This transformer can be used with any monad that implements the 'ArrowMonad' interface.
newtype KleisliT f c x y = KleisliT { runKleisliT :: c x (f y) }

instance (ArrowMonad f c, ArrowRun c) => ArrowRun (KleisliT f c) where
  type Run (KleisliT f c) x y = Run c x (f y)

instance ArrowLift (KleisliT f c) where
  type Underlying (KleisliT f c) x y = c x (f y)

instance (ArrowMonad f c, ArrowPrimitive c) => ArrowPrimitive (KleisliT f c) where
  type PrimState (KleisliT f c) = PrimState c

instance Monad f => ArrowTrans (KleisliT f) where
  lift' f = lift $ rmap return f
  {-# INLINE lift' #-}

instance ArrowMonad f c => Profunctor (KleisliT f c) where
  dimap f g h = lift $ dimap f (fmap g) $ unlift h
  lmap f h = lift $ lmap f (unlift h)
  rmap g h = lift $ rmap (fmap g) (unlift h)
  f .# _ = unsafeCoerce f
  _ #. g = unsafeCoerce g
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}
  {-# INLINE (.#) #-}
  {-# INLINE (#.) #-}

instance ArrowMonad f c => Category (KleisliT f c) where
  id = lift unitA
  f . g = lift $ mapJoinA (unlift f) . unlift g
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance ArrowMonad f c => Arrow (KleisliT f c) where
  arr f = lift $ arr (return . f)
  first f = lift $ rmap strength1 (first (unlift f))
  second f = lift $ rmap strength2 (second (unlift f))
  f &&& g = lmap (\x -> (x,x)) (f *** g)
  f *** g = first f >>> second g
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (***) #-}

instance (ArrowMonad f c, ArrowChoice c) => ArrowChoice (KleisliT f c) where
  left f = lift $ rmap strength1 $ left (unlift f)
  right f = lift $ rmap strength2 $ right (unlift f)
  f ||| g = lift $ unlift f ||| unlift g
  f +++ g = left f >>> right g
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (+++) #-}
  {-# INLINE (|||) #-}

instance (ArrowMonad f c, ArrowApply c) => ArrowApply (KleisliT f c) where
  app = lift (app .# first coerce)
  {-# INLINE app #-}

instance (ArrowCont c, Monad f, ArrowMonad f c) => ArrowCont (KleisliT f c) where
  type Cont (KleisliT f c) y = Cont c (f y)
  callCC f = lift $ callCC $ \k -> unlift (f k)
  jump k = lift $ lmap (return @f) (jump k)
  {-# INLINE callCC #-}

instance (ArrowMonad f c, ArrowState s c) => ArrowState s (KleisliT f c) where
  get = lift' State.get
  put = lift' State.put
  {-# INLINE get #-}
  {-# INLINE put #-}

instance (ArrowMonad f c, ArrowReader r c) => ArrowReader r (KleisliT f c) where
  ask = lift' Reader.ask
  local f = lift (Reader.local (unlift f))
  {-# INLINE ask #-}
  {-# INLINE local #-}

instance (ArrowMonad f c, ArrowConst r c) => ArrowConst r (KleisliT f c) where
  askConst f = lift (askConst (unlift . f))
  {-# INLINE askConst #-}
