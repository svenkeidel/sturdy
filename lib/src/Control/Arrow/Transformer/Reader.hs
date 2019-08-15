{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Control.Arrow.Transformer.Reader(ReaderT(..)) where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Reader as Reader
import Control.Arrow.Store as Store
import Control.Arrow.State as State
import Control.Arrow.Except as Exc
import Control.Arrow.Trans
import Control.Arrow.Writer
import Control.Arrow.Order
import Control.Category

import Data.Profunctor
import Data.Profunctor.Unsafe
import Data.Monoidal
import Data.Coerce
import Unsafe.Coerce

-- Due to "Generalising Monads to Arrows", by John Hughes, in Science of Computer Programming 37.
newtype ReaderT r c x y = ReaderT { runReaderT :: c (r,x) y }

instance ArrowRun c => ArrowRun (ReaderT r c) where
  type Rep (ReaderT r c) x y = Rep c (r,x) y
  run = run . runReaderT

instance (Profunctor c, Arrow c) => Profunctor (ReaderT r c) where
#ifdef PROFUNCTOR
  dimap f g h = lift $ dimap (second f) g (unlift h)
  lmap f h = lift $ lmap (second f) (unlift h)
  rmap g h = lift $ rmap g (unlift h)
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
#else
  {-# NOINLINE dimap #-}
  {-# NOINLINE lmap #-}
  {-# NOINLINE rmap #-}
  {-# NOINLINE (.#) #-}
  {-# NOINLINE (#.) #-}
#endif

instance ArrowTrans (ReaderT r) where
  type Dom (ReaderT r) x y = (r,x)
  type Cod (ReaderT r) x y = y
  lift = coerce
  unlift = coerce
#ifdef _INLINE
  {-# INLINE lift #-}
  {-# INLINE unlift #-}
#else
  {-# NOINLINE lift #-}
  {-# NOINLINE unlift #-}
#endif

instance ArrowLift (ReaderT r) where
  lift' f = lift $ lmap snd f
#ifdef _INLINE
  {-# INLINE lift' #-}
#else
  {-# NOINLINE lift' #-}
#endif

instance (Arrow c, Profunctor c) => Category (ReaderT r c) where
  id    = lift' id
  f . g = lift $ lmap (\(r,x) -> (r,(r,x))) (unlift f . second (unlift g))
#ifdef _INLINE
  {-# INLINE id #-}
  {-# INLINE (.) #-}
#else
  {-# NOINLINE id #-}
  {-# NOINLINE (.) #-}
#endif

instance (Arrow c, Profunctor c) => Arrow (ReaderT r c) where
  arr f    = lift' (arr f)
  first f  = lift $ lmap assoc1 $ first (unlift f)
  second f = lift $ lmap shuffle1 $ second (unlift f)
  f &&& g  = lift $ unlift f &&& unlift g
  f *** g  = lift $ lmap (\(r,(b,d)) -> ((r,b),(r,d))) $ unlift f *** unlift g
#ifdef _INLINE
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (***) #-}
#else
  {-# NOINLINE arr #-}
  {-# NOINLINE first #-}
  {-# NOINLINE second #-}
  {-# NOINLINE (&&&) #-}
  {-# NOINLINE (***) #-}
#endif

instance (ArrowChoice c, Profunctor c) => ArrowChoice (ReaderT r c) where
  left f  = lift $ lmap (\(r,e) -> left (r,) e) $ left (unlift f)
  right f = lift $ lmap (\(r,e) -> right (r,) e) $ right (unlift f)
  f +++ g = lift $ lmap distribute1 $ unlift f +++ unlift g
  f ||| g = lift $ lmap distribute1 $ unlift f ||| unlift g
#ifdef _INLINE
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (+++) #-}
  {-# INLINE (|||) #-}
#else
  {-# NOINLINE left #-}
  {-# NOINLINE right #-}
  {-# NOINLINE (+++) #-}
  {-# NOINLINE (|||) #-}
#endif

instance (ArrowApply c, Profunctor c) => ArrowApply (ReaderT r c) where
  app = lift $ lmap (\(r,(f,b)) -> (unlift f,(r,b))) app
#ifdef _INLINE
  {-# INLINE app #-}
#else
  {-# NOINLINE app #-}
#endif

instance (Arrow c, Profunctor c) => ArrowReader r (ReaderT r c) where
  ask = lift (arr fst)
  local f = lift $ lmap snd (unlift f)
#ifdef _INLINE
  {-# INLINE ask #-}
  {-# INLINE local #-}
#else
  {-# NOINLINE ask #-}
  {-# NOINLINE local #-}
#endif

instance ArrowState s c => ArrowState s (ReaderT r c) where
  get = lift' State.get
  put = lift' State.put
  modify f = lift (modify (lmap assoc2 (unlift f)))
#ifdef _INLINE
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE modify #-}
#else
  {-# NOINLINE get #-}
  {-# NOINLINE put #-}
  {-# NOINLINE modify #-}
#endif

instance ArrowWriter w c => ArrowWriter w (ReaderT r c) where
  tell = lift' tell
#ifdef _INLINE
  {-# INLINE tell #-}
#else
  {-# NOINLINE tell #-}
#endif

instance ArrowFail e c => ArrowFail e (ReaderT r c) where
  fail = lift' fail
#ifdef _INLINE
  {-# INLINE fail #-}
#else
  {-# NOINLINE fail #-}
#endif

instance ArrowEnv var val c => ArrowEnv var val (ReaderT r c) where
  type instance Join y (ReaderT r c) = Env.Join y c
  lookup f g = lift $ lmap shuffle1
                    $ lookup (lmap shuffle1 (unlift f)) (unlift g)
  extend f = lift $ lmap (\(r,(var,val,x)) -> (var,val,(r,x))) (Env.extend (unlift f))
#ifdef _INLINE
  {-# INLINE lookup #-}
  {-# INLINE extend #-}
#else
  {-# NOINLINE lookup #-}
  {-# NOINLINE extend #-}
#endif

instance ArrowClosure var val env c => ArrowClosure var val env (ReaderT r c) where
  ask     = lift' Env.ask
  local f = lift $ lmap shuffle1 $ Env.local (unlift f)
#ifdef _INLINE
  {-# INLINE ask #-}
  {-# INLINE local #-}
#else
  {-# NOINLINE ask #-}
  {-# NOINLINE local #-}
#endif

instance ArrowStore var val c => ArrowStore var val (ReaderT r c) where
  type instance Join y (ReaderT r c) = Store.Join y c
  read f g = lift $ lmap shuffle1 
                  $ read (lmap shuffle1 (unlift f)) (unlift g)
  write = lift' write
#ifdef _INLINE
  {-# INLINE read #-}
  {-# INLINE write #-}
#else
  {-# NOINLINE read #-}
  {-# NOINLINE write #-}
#endif

type instance Fix x y (ReaderT r c) = ReaderT r (Fix (Dom (ReaderT r) x y) (Cod (ReaderT r) x y) c)
instance ArrowFix (Dom (ReaderT r) x y) (Cod (ReaderT r) x y) c => ArrowFix x y (ReaderT r c) where
  fix = liftFix
#ifdef _INLINE
  {-# INLINE fix #-}
#else
  {-# NOINLINE fix #-}
#endif

instance ArrowExcept e c => ArrowExcept e (ReaderT r c) where
  type instance Join z (ReaderT r c) = Exc.Join z c
  throw = lift' throw
  try f g h = lift $ try (lmap (\(r,x) -> (r,(r,x))) (second (unlift f))) (unlift g) (lmap assoc2 (unlift h))
#ifdef _INLINE
  {-# INLINE throw #-}
  {-# INLINE try #-}
#else
  {-# NOINLINE throw #-}
  {-# NOINLINE try #-}
#endif

instance ArrowLowerBounded c => ArrowLowerBounded (ReaderT r c) where
  bottom = ReaderT bottom
#ifdef _INLINE
  {-# INLINE bottom #-}
#else
  {-# NOINLINE bottom #-}
#endif

instance ArrowJoin c => ArrowJoin (ReaderT r c) where
  joinSecond g = lift $ lmap shuffle1 (joinSecond (unlift g))
#ifdef _INLINE
  {-# INLINE joinSecond #-}
#else
  {-# NOINLINE joinSecond #-}
#endif

instance ArrowComplete y c => ArrowComplete y (ReaderT r c) where
  f <⊔> g = lift $ unlift f <⊔> unlift g
#ifdef _INLINE
  {-# INLINE (<⊔>) #-}
#else
  {-# NOINLINE (<⊔>) #-}
#endif

instance ArrowConst x c => ArrowConst x (ReaderT r c) where
  askConst f = lift (askConst (unlift . f))
#ifdef _INLINE
  {-# INLINE askConst #-}
#else
  {-# NOINLINE askConst #-}
#endif

instance ArrowEffectCommutative c => ArrowEffectCommutative (ReaderT r c)
