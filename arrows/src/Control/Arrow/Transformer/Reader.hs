{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Control.Arrow.Transformer.Reader(ReaderT(..)) where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Category
import Control.Arrow
import Control.Arrow.Cont
import Control.Arrow.Const
import Control.Arrow.Primitive
import Control.Arrow.Reader as Reader
import Control.Arrow.State as State
import Control.Arrow.Trans
import Control.Arrow.Writer

import Data.Profunctor
import Data.Profunctor.Unsafe
import Data.Monoidal
import Unsafe.Coerce

-- Due to "Generalising Monads to Arrows", by John Hughes, in Science of Computer Programming 37.
-- | Arrow transformer that adds a read-only value to a computation.
newtype ReaderT r c x y = ReaderT { runReaderT :: c (r,x) y }

instance ArrowRun c => ArrowRun (ReaderT r c) where type Run (ReaderT r c) x y = Run c (r,x) y
instance ArrowLift (ReaderT r c) where type Underlying (ReaderT r c) x y = c (r,x) y
instance (ArrowPrimitive c) => ArrowPrimitive (ReaderT s c) where type PrimState (ReaderT s c) = PrimState c

instance (Profunctor c) => Profunctor (ReaderT r c) where
  dimap f g h = lift $ dimap (second f) g (unlift h)
  lmap f h = lift $ lmap (second f) (unlift h)
  rmap g h = lift $ rmap g (unlift h)
  f .# _ = f `seq` unsafeCoerce f
  _ #. g = g `seq` unsafeCoerce g
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}
  {-# INLINE (.#) #-}
  {-# INLINE (#.) #-}

instance ArrowTrans (ReaderT r) where
  lift' f = lift $ lmap snd f
  {-# INLINE lift' #-}

instance (Arrow c, Profunctor c) => Category (ReaderT r c) where
  id    = lift' id
  f . g = lift $ lmap (\(r,x) -> (r,(r,x))) (unlift f . second (unlift g))
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance (Arrow c, Profunctor c) => Arrow (ReaderT r c) where
  arr f    = lift' (arr f)
  first f  = lift $ lmap assoc1 $ first (unlift f)
  second f = lift $ lmap shuffle1 $ second (unlift f)
  f &&& g  = lift $ unlift f &&& unlift g
  f *** g  = lift $ lmap (\(r,(b,d)) -> ((r,b),(r,d))) $ unlift f *** unlift g
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (***) #-}

instance (ArrowChoice c, Profunctor c) => ArrowChoice (ReaderT r c) where
  left f  = lift $ lmap (\(r,e) -> left (r,) e) $ left (unlift f)
  right f = lift $ lmap (\(r,e) -> right (r,) e) $ right (unlift f)
  f +++ g = lift $ lmap distribute1 $ unlift f +++ unlift g
  f ||| g = lift $ lmap distribute1 $ unlift f ||| unlift g
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (+++) #-}
  {-# INLINE (|||) #-}

instance (ArrowApply c, Profunctor c) => ArrowApply (ReaderT r c) where
  app = lift $ lmap (\(r,(f,b)) -> (unlift f,(r,b))) app
  {-# INLINE app #-}

instance (ArrowCont c, Profunctor c) => ArrowCont (ReaderT r c) where
  type Cont (ReaderT r c) y = Cont c y
  callCC f = lift $ callCC $ \k -> unlift (f k)
  jump k = lift $ lmap snd $ jump k
  {-# INLINE callCC #-}

instance (Arrow c, Profunctor c) => ArrowReader r (ReaderT r c) where
  ask = lift (arr fst)
  local f = lift $ lmap snd (unlift f)
  {-# INLINE ask #-}
  {-# INLINE local #-}

instance ArrowState s c => ArrowState s (ReaderT r c) where
  get = lift' State.get
  put = lift' State.put
  modify f = lift (modify (lmap assoc2 (unlift f)))
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE modify #-}

instance ArrowWriter w c => ArrowWriter w (ReaderT r c) where
  tell = lift' tell
  {-# INLINE tell #-}

instance ArrowConst x c => ArrowConst x (ReaderT r c) where
  askConst f = lift (askConst (unlift . f))
  {-# INLINE askConst #-}
