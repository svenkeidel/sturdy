{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Static where

import Prelude hiding (id,(.),lookup,read,fail,elem)

import Control.Category

import Control.Arrow
import Control.Arrow.Primitive
import Control.Arrow.Reader as Reader
import Control.Arrow.State as State
import Control.Arrow.Trans
import Control.Arrow.Writer

import Data.Profunctor
import Data.Profunctor.Unsafe
import Unsafe.Coerce

-- Due to https://hackage.haskell.org/package/arrows/docs/Control-Arrow-Transformer-StaticT.html
newtype StaticT f c x y = StaticT { runStaticT :: f (c x y) }

instance (Applicative f, ArrowRun c) =>  ArrowRun (StaticT f c) where
  type Run (StaticT f c) x y = f (Run c x y)
  run = fmap run . runStaticT
  {-# INLINE run #-}
  {-# SPECIALIZE instance (ArrowRun c) => ArrowRun (StaticT ((->) r) c) #-}

instance (Applicative f, ArrowPrimitive c) => ArrowPrimitive (StaticT f c) where
  type PrimState (StaticT f c) = PrimState c
  {-# SPECIALIZE instance (ArrowPrimitive c) => ArrowPrimitive (StaticT ((->) r) c) #-}

instance (Applicative f) =>  ArrowLift (StaticT f c) where
  type Underlying (StaticT f c) x y = f (c x y)

instance (Applicative f, Profunctor c) => Profunctor (StaticT f c) where
  dimap f g (StaticT h) = StaticT $ dimap f g <$> h
  lmap f (StaticT h) = StaticT $ lmap f <$> h
  rmap g (StaticT h) = StaticT $ rmap g <$> h
  f .# _ = f `seq` unsafeCoerce f
  _ #. g = g `seq` unsafeCoerce g
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}
  {-# INLINE (.#) #-}
  {-# INLINE (#.) #-}
  {-# SPECIALIZE instance (Profunctor c) => Profunctor (StaticT ((->) r) c) #-}

instance Applicative f => ArrowTrans (StaticT f) where
  lift' = StaticT . pure
  {-# INLINE lift' #-}
  {-# SPECIALIZE instance ArrowTrans (StaticT ((->) r)) #-}

instance (Applicative f, Category c) => Category (StaticT f c) where
  id = StaticT (pure id)
  StaticT f . StaticT g = StaticT $ (.) <$> f <*> g
  {-# INLINE id #-}
  {-# INLINE (.) #-}
  {-# SPECIALIZE instance Arrow c => Category (StaticT ((->) r) c) #-}

instance (Applicative f, Arrow c, Profunctor c) => Arrow (StaticT f c) where
  arr = lift' . arr
  first (StaticT f) = StaticT $ first <$> f
  second (StaticT f) = StaticT $ second <$> f
  StaticT f *** StaticT g = StaticT $ (***) <$> f <*> g
  StaticT f &&& StaticT g = StaticT $ (&&&) <$> f <*> g
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (***) #-}
  {-# SPECIALIZE instance (Arrow c, Profunctor c) => Arrow (StaticT ((->) r) c) #-}

instance (Applicative f, ArrowChoice c, Profunctor c) => ArrowChoice (StaticT f c) where
  left (StaticT f) = StaticT $ left <$> f
  right (StaticT f) = StaticT $ right <$> f
  StaticT f +++ StaticT g = StaticT $ (+++) <$> f <*> g
  StaticT f ||| StaticT g = StaticT $ (|||) <$> f <*> g
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (+++) #-}
  {-# INLINE (|||) #-}
  {-# SPECIALIZE instance (ArrowChoice c, Profunctor c) => ArrowChoice (StaticT ((->) r) c) #-}

instance (Applicative f, ArrowState s c) => ArrowState s (StaticT f c) where
  get = lift' State.get
  put = lift' State.put
  modify (StaticT f) = StaticT $ State.modify <$> f
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE modify #-}
  {-# SPECIALIZE instance ArrowState s c => ArrowState s (StaticT ((->) r) c) #-}

instance (Applicative f, ArrowReader r c) => ArrowReader r (StaticT f c) where
  ask = lift' Reader.ask
  local (StaticT f) = StaticT $ Reader.local <$> f
  {-# INLINE ask #-}
  {-# INLINE local #-}
  {-# SPECIALIZE instance ArrowReader r c => ArrowReader r (StaticT ((->) r) c) #-}

instance (Applicative f, ArrowWriter w c) => ArrowWriter w (StaticT f c) where
  tell = lift' tell
  {-# INLINE tell #-}
  {-# SPECIALIZE instance ArrowWriter e c => ArrowWriter e (StaticT ((->) r) c) #-}
