{-# LANGUAGE CPP #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Static where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Category

import Control.Arrow
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Except as Exc
import Control.Arrow.Trans
import Control.Arrow.Reader as Reader
import Control.Arrow.State as State
import Control.Arrow.Store as Store
import Control.Arrow.Writer
import Control.Arrow.Order

import Data.Profunctor
import Data.Profunctor.Unsafe
import Unsafe.Coerce

-- Due to https://hackage.haskell.org/package/arrows/docs/Control-Arrow-Transformer-StaticT.html
newtype StaticT f c x y = StaticT { runStaticT :: f (c x y) }

instance (Applicative f, ArrowRun c) =>  ArrowRun (StaticT f c) where
  type Rep (StaticT f c) x y = f (Rep c x y)
  run = fmap run . runStaticT
  {-# INLINE run #-}
  {-# SPECIALIZE instance (ArrowRun c) => ArrowRun (StaticT ((->) r) c) #-}

instance (Applicative f, Arrow c, Profunctor c) => Profunctor (StaticT f c) where
#ifdef PROFUNCTOR
  dimap f g (StaticT h) = StaticT $ dimap f g <$> h
  lmap f (StaticT h) = StaticT $ lmap f <$> h
  rmap g (StaticT h) = StaticT $ rmap g <$> h
#else
  dimap f g h = f ^>> h >>^ g
  lmap f h = f ^>> h
  rmap g h = h >>^ g
#endif
  f .# _ = f `seq` unsafeCoerce f
  _ #. g = g `seq` unsafeCoerce g
#ifdef _INLINE
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}
  {-# INLINE (.#) #-}
  {-# INLINE (#.) #-}
  {-# SPECIALIZE instance (Arrow c, Profunctor c) => Profunctor (StaticT ((->) r) c) #-}
#else
  {-# NOINLINE dimap #-}
  {-# NOINLINE lmap #-}
  {-# NOINLINE rmap #-}
  {-# NOINLINE (.#) #-}
  {-# NOINLINE (#.) #-}
#endif

instance Applicative f => ArrowLift (StaticT f) where
  lift' = StaticT . pure
#ifdef _INLINE
  {-# INLINE lift' #-}
  {-# SPECIALIZE instance ArrowLift (StaticT ((->) r)) #-}
#else
  {-# NOINLINE lift' #-}
#endif

instance (Applicative f, Category c, Profunctor c) => Category (StaticT f c) where
  id = StaticT (pure id)
  StaticT f . StaticT g = StaticT $ (.) <$> f <*> g
#ifdef _INLINE
  {-# INLINE id #-}
  {-# INLINE (.) #-}
  {-# SPECIALIZE instance (Arrow c, Profunctor c) => Category (StaticT ((->) r) c) #-}
#else
  {-# NOINLINE id #-}
  {-# NOINLINE (.) #-}
#endif

instance (Applicative f, Arrow c, Profunctor c) => Arrow (StaticT f c) where
  arr = lift' . arr
  first (StaticT f) = StaticT $ first <$> f
  second (StaticT f) = StaticT $ second <$> f
  StaticT f *** StaticT g = StaticT $ (***) <$> f <*> g
  StaticT f &&& StaticT g = StaticT $ (&&&) <$> f <*> g
#ifdef _INLINE
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (***) #-}
  {-# SPECIALIZE instance (Arrow c, Profunctor c) => Arrow (StaticT ((->) r) c) #-}
#else
  {-# NOINLINE arr #-}
  {-# NOINLINE first #-}
  {-# NOINLINE second #-}
  {-# NOINLINE (&&&) #-}
  {-# NOINLINE (***) #-}
#endif

instance (Applicative f, ArrowChoice c, Profunctor c) => ArrowChoice (StaticT f c) where
  left (StaticT f) = StaticT $ left <$> f
  right (StaticT f) = StaticT $ right <$> f
  StaticT f +++ StaticT g = StaticT $ (+++) <$> f <*> g
  StaticT f ||| StaticT g = StaticT $ (|||) <$> f <*> g
#ifdef _INLINE
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (+++) #-}
  {-# INLINE (|||) #-}
  {-# SPECIALIZE instance (ArrowChoice c, Profunctor c) => ArrowChoice (StaticT ((->) r) c) #-}
#else
  {-# NOINLINE left #-}
  {-# NOINLINE right #-}
  {-# NOINLINE (+++) #-}
  {-# NOINLINE (|||) #-}
#endif

instance (Applicative f, ArrowState s c) => ArrowState s (StaticT f c) where
  get = lift' State.get
  put = lift' State.put
  modify (StaticT f) = StaticT $ State.modify <$> f
#ifdef _INLINE
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE modify #-}
  {-# SPECIALIZE instance ArrowState s c => ArrowState s (StaticT ((->) r) c) #-}
#else
  {-# NOINLINE get #-}
  {-# NOINLINE put #-}
  {-# NOINLINE modify #-}
#endif

instance (Applicative f, ArrowReader r c) => ArrowReader r (StaticT f c) where
  ask = lift' Reader.ask
  local (StaticT f) = StaticT $ Reader.local <$> f
#ifdef _INLINE
  {-# INLINE ask #-}
  {-# INLINE local #-}
  {-# SPECIALIZE instance ArrowReader r c => ArrowReader r (StaticT ((->) r) c) #-}
#else
  {-# NOINLINE ask #-}
  {-# NOINLINE local #-}
#endif

instance (Applicative f, ArrowWriter w c) => ArrowWriter w (StaticT f c) where
  tell = lift' tell
#ifdef _INLINE
  {-# INLINE tell #-}
  {-# SPECIALIZE instance ArrowWriter e c => ArrowWriter e (StaticT ((->) r) c) #-}
#else
  {-# NOINLINE tell #-}
#endif

instance (Applicative f, ArrowFail e c) => ArrowFail e (StaticT f c) where
  fail = lift' fail
#ifdef _INLINE
  {-# INLINE fail #-}
  {-# SPECIALIZE instance ArrowFail e c => ArrowFail e (StaticT ((->) r) c) #-}
#else
  {-# NOINLINE fail #-}
#endif

instance (Applicative f, ArrowExcept e c) => ArrowExcept e (StaticT f c) where
  type Join y (StaticT f c) = Exc.Join y c
  throw = lift' throw
  try (StaticT f) (StaticT g) (StaticT h) = StaticT $ try <$> f <*> g <*> h
#ifdef _INLINE
  {-# INLINE throw #-}
  {-# INLINE try #-}
  {-# SPECIALIZE instance ArrowExcept e c => ArrowExcept e (StaticT ((->) r) c) #-}
#else
  {-# NOINLINE throw #-}
  {-# NOINLINE try #-}
#endif

instance (Applicative f, ArrowEnv var val c) => ArrowEnv var val (StaticT f c) where
  type Join y (StaticT f c) = Env.Join y c
  lookup (StaticT f) (StaticT g) = StaticT $ Env.lookup <$> f <*> g
  extend (StaticT f) = StaticT $ Env.extend <$> f
#ifdef _INLINE
  {-# INLINE lookup #-}
  {-# INLINE extend #-}
  {-# SPECIALIZE instance ArrowEnv var val c => ArrowEnv var val (StaticT ((->) r) c) #-}
#else
  {-# NOINLINE lookup #-}
  {-# NOINLINE extend #-}
#endif

instance (Applicative f, ArrowClosure var val env c) => ArrowClosure var val env (StaticT f c) where
  ask = lift' Env.ask
  local (StaticT f) = StaticT $ Env.local <$> f
#ifdef _INLINE
  {-# INLINE ask #-}
  {-# INLINE local #-}
  {-# SPECIALIZE instance ArrowClosure var val env c => ArrowClosure var val env (StaticT ((->) r) c) #-}
#else
  {-# NOINLINE ask #-}
  {-# NOINLINE local #-}
#endif

instance (Applicative f, ArrowStore var val c) => ArrowStore var val (StaticT f c) where
  type Join y (StaticT f c) = Store.Join y c
  read (StaticT f) (StaticT g) = StaticT $ read <$> f <*> g
  write = lift' write
#ifdef _INLINE
  {-# INLINE read #-}
  {-# INLINE write #-}
  {-# SPECIALIZE instance ArrowStore var val c => ArrowStore var val (StaticT ((->) r) c) #-}
#else
  {-# NOINLINE read #-}
  {-# NOINLINE write #-}
#endif

instance (Applicative f, ArrowLowerBounded c) => ArrowLowerBounded (StaticT f c) where
  bottom = StaticT (pure bottom)
#ifdef _INLINE
  {-# INLINE bottom #-}
  {-# SPECIALIZE instance ArrowLowerBounded c => ArrowLowerBounded (StaticT ((->) r) c) #-}
#else
  {-# NOINLINE bottom #-}
#endif

instance (Applicative f, ArrowJoin c) => ArrowJoin (StaticT f c) where
  joinSecond (StaticT g) = StaticT $ joinSecond <$> g
#ifdef _INLINE
  {-# INLINE joinSecond #-}
  {-# SPECIALIZE instance ArrowJoin c => ArrowJoin (StaticT ((->) r) c) #-}
#else
  {-# NOINLINE joinSecond #-}
#endif

instance (Applicative f, ArrowComplete y c) => ArrowComplete y (StaticT f c) where
  StaticT f <⊔> StaticT g = StaticT $ (<⊔>) <$> f <*> g 
#ifdef _INLINE
  {-# INLINE (<⊔>) #-}
  {-# SPECIALIZE instance ArrowComplete y c => ArrowComplete y (StaticT ((->) r) c) #-}
#else
  {-# NOINLINE (<⊔>) #-}
#endif
