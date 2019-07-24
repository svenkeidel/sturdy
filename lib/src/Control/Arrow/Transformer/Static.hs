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

instance Applicative f => ArrowLift (StaticT f) where
  lift' = StaticT . pure
  {-# INLINE lift' #-}

instance (Applicative f, Arrow c, Profunctor c) => Category (StaticT f c) where
  id = lift' id
  StaticT f . StaticT g = StaticT $ (.) <$> f <*> g
  {-# INLINE id #-}
  {-# INLINE (.) #-}

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

instance (Applicative f, ArrowChoice c, Profunctor c) => ArrowChoice (StaticT f c) where
  left (StaticT f) = StaticT $ left <$> f
  right (StaticT f) = StaticT $ right <$> f
  StaticT f +++ StaticT g = StaticT $ (+++) <$> f <*> g
  StaticT f ||| StaticT g = StaticT $ (|||) <$> f <*> g
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (+++) #-}
  {-# INLINE (|||) #-}

instance (Applicative f, ArrowState s c) => ArrowState s (StaticT f c) where
  get = lift' State.get
  put = lift' State.put
  modify (StaticT f) = StaticT $ State.modify <$> f
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE modify #-}

instance (Applicative f, ArrowReader r c) => ArrowReader r (StaticT f c) where
  ask = lift' Reader.ask
  local (StaticT f) = StaticT $ Reader.local <$> f
  {-# INLINE ask #-}
  {-# INLINE local #-}

instance (Applicative f, ArrowWriter w c) => ArrowWriter w (StaticT f c) where
  tell = lift' tell
  {-# INLINE tell #-}

instance (Applicative f, ArrowFail e c) => ArrowFail e (StaticT f c) where
  fail = lift' fail
  {-# INLINE fail #-}

instance (Applicative f, ArrowExcept e c) => ArrowExcept e (StaticT f c) where
  type Join y (StaticT f c) = Exc.Join y c
  throw = lift' throw
  try (StaticT f) (StaticT g) (StaticT h) = StaticT $ try <$> f <*> g <*> h
  {-# INLINE throw #-}
  {-# INLINE try #-}

instance (Applicative f, ArrowEnv var val c) => ArrowEnv var val (StaticT f c) where
  type Join y (StaticT f c) = Env.Join y c
  lookup (StaticT f) (StaticT g) = StaticT $ Env.lookup <$> f <*> g
  extend (StaticT f) = StaticT $ Env.extend <$> f
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance (Applicative f, ArrowClosure var val env c) => ArrowClosure var val env (StaticT f c) where
  ask = lift' Env.ask
  local (StaticT f) = StaticT $ Env.local <$> f
  {-# INLINE ask #-}
  {-# INLINE local #-}

instance (Applicative f, ArrowStore var val c) => ArrowStore var val (StaticT f c) where
  type Join y (StaticT f c) = Store.Join y c
  read (StaticT f) (StaticT g) = StaticT $ read <$> f <*> g
  write = lift' write
  {-# INLINE read #-}
  {-# INLINE write #-}

instance (Applicative f, ArrowLowerBounded c) => ArrowLowerBounded (StaticT f c) where
  bottom = StaticT (pure bottom)
  {-# INLINE bottom #-}

instance (Applicative f, ArrowJoin c) => ArrowJoin (StaticT f c) where
  join lub (StaticT f) (StaticT g) = StaticT $ join lub <$> f <*> g
  {-# INLINE join #-}

instance (Applicative f, ArrowComplete y c) => ArrowComplete y (StaticT f c) where
  StaticT f <⊔> StaticT g = StaticT $ (<⊔>) <$> f <*> g 
