{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Arrow.Transformer.Writer where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Category
import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Environment as Env
import Control.Arrow.Except as Exc
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Order
import Control.Arrow.Random
import Control.Arrow.Reader as Reader
import Control.Arrow.State as State
import Control.Arrow.Store as Store
import Control.Arrow.Trans
import Control.Arrow.Writer

import Data.Monoidal
import qualified Data.Order as O
import Data.Profunctor
import Data.Profunctor.Unsafe
import Data.Coerce
import Unsafe.Coerce

newtype WriterT w c x y = WriterT { runWriterT :: c x (w,y) }

censor :: (Arrow c,Profunctor c) => (x -> w -> w) -> WriterT w c x y -> WriterT w c x y
censor f (WriterT g) = WriterT (dimap (\x -> (x,x)) (\(x,(w,y)) -> (f x w,y)) (second g))

instance (Monoid w,ArrowRun c) => ArrowRun (WriterT w c) where
  type Rep (WriterT w c) x y = Rep c x (w,y)
  run = run . runWriterT
  {-# INLINE run #-}

instance (Monoid w,Profunctor c, Arrow c) => Profunctor (WriterT w c) where
#ifdef PROFUNCTOR
  dimap f g h = lift $ dimap f (second g) (unlift h)
  lmap f h = lift $ lmap f (unlift h)
  rmap g h = lift $ rmap (second g) (unlift h)
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

instance ArrowTrans (WriterT w) where
  type Dom (WriterT w) x y = x
  type Cod (WriterT w) x y = (w,y)
  lift = coerce
  unlift = coerce
#ifdef _INLINE
  {-# INLINE lift #-}
  {-# INLINE unlift #-}
#else
  {-# NOINLINE lift #-}
  {-# NOINLINE unlift #-}
#endif

instance Monoid w => ArrowLift (WriterT w) where
  lift' f = lift (rmap (\y -> (mempty,y)) f)
#ifdef _INLINE
  {-# INLINE lift' #-}
#else
  {-# NOINLINE lift' #-}
#endif

instance (Monoid w, Arrow c, Profunctor c) => Category (WriterT w c) where
  id = lift' id
  g . f = lift $ rmap (\(w1,(w2,z)) -> (w1 <> w2,z)) (unlift f >>> second (unlift g)) 
  -- proc x -> do
  --   (w1,y) <- f -< x
  --   (w2,z) <- g -< y
  --   returnA -< (w1 <> w2,z)
#ifdef _INLINE
  {-# INLINE id #-}
  {-# INLINE (.) #-}
#else
  {-# NOINLINE id #-}
  {-# NOINLINE (.) #-}
#endif

instance (Monoid w, Arrow c, Profunctor c) => Arrow (WriterT w c) where
  arr f = lift' (arr f)
  first f = lift $ rmap assoc2 (first (unlift f)) 
  second g = lift $ rmap shuffle2 (second (unlift g)) 
  f *** g = lift $ rmap (\((w1,b),(w2,d)) -> (w1 <> w2,(b,d))) (unlift f *** unlift g) 
  f &&& g = lift $ rmap (\((w1,b),(w2,d)) -> (w1 <> w2,(b,d))) (unlift f &&& unlift g)
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

instance (Monoid w, ArrowChoice c, Profunctor c) => ArrowChoice (WriterT w c) where
  left f = lift $ rmap (\e -> case e of Left (w,x) -> (w,Left x); Right y -> (mempty,Right y)) (left (unlift f)) 
  right f = lift $ rmap (\e -> case e of Left x -> (mempty,Left x); Right (w,y) -> (w,Right y)) (right (unlift f))
  f ||| g = lift $ unlift f ||| unlift g
  f +++ g = lift $ rmap distribute2 (unlift f +++ unlift g) 
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

instance (Monoid w, ArrowApply c, Profunctor c) => ArrowApply (WriterT w c) where
  app = lift (app .# first coerce)
#ifdef _INLINE
  {-# INLINE app #-}
#else
  {-# NOINLINE app #-}
#endif

instance (Monoid w, ArrowState s c) => ArrowState s (WriterT w c) where
  get = lift' get
  put = lift' put
  modify f = lift $ modify (rmap assoc1 (unlift f))
#ifdef _INLINE
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE modify #-}
#else
  {-# NOINLINE get #-}
  {-# NOINLINE put #-}
  {-# NOINLINE modify #-}
#endif

instance (Monoid w, Arrow c, Profunctor c) => ArrowWriter w (WriterT w c) where
  tell = lift (arr (\w -> (w,())))
#ifdef _INLINE
  {-# INLINE tell #-}
#else
  {-# NOINLINE tell #-}
#endif

instance (Monoid w, ArrowFail e c) => ArrowFail e (WriterT w c) where
  fail = lift' fail
#ifdef _INLINE
  {-# INLINE fail #-}
#else
  {-# NOINLINE fail #-}
#endif

instance (Monoid w, ArrowExcept e c) => ArrowExcept e (WriterT w c) where
  type Join y (WriterT w c) = Exc.Join (w,y) c
  throw = lift' throw
  try f g h = lift $ try (unlift f) (rmap (\(w1,(w2,z)) -> (w1 <> w2,z)) (second (unlift g))) (unlift h)
#ifdef _INLINE
  {-# INLINE throw #-}
  {-# INLINE try #-}
#else
  {-# NOINLINE throw #-}
  {-# NOINLINE try #-}
#endif

instance (Monoid w, ArrowReader r c) => ArrowReader r (WriterT w c) where
  ask = lift' Reader.ask
  local f = lift (Reader.local (unlift f))
#ifdef _INLINE
  {-# INLINE ask #-}
  {-# INLINE local #-}
#else
  {-# NOINLINE ask #-}
  {-# NOINLINE local #-}
#endif

instance (Monoid w, ArrowEnv var val c) => ArrowEnv var val (WriterT w c) where
  type Join y (WriterT w c) = Env.Join (w,y) c
  lookup f g = lift $ lookup (unlift f) (unlift g)
  extend f = lift $ extend (unlift f)
#ifdef _INLINE
  {-# INLINE lookup #-}
  {-# INLINE extend #-}
#else
  {-# NOINLINE lookup #-}
  {-# NOINLINE extend #-}
#endif

instance (Monoid w, ArrowClosure var val env c) => ArrowClosure var val env (WriterT w c) where
  ask = lift' Env.ask
  local f = lift (Env.local (unlift f))
#ifdef _INLINE
  {-# INLINE ask #-}
  {-# INLINE local #-}
#else
  {-# NOINLINE ask #-}
  {-# NOINLINE local #-}
#endif

instance (Monoid w, ArrowStore var val c) => ArrowStore var val (WriterT w c) where
  type Join y (WriterT w c) = Store.Join (w,y) c
  read f g = lift $ read (unlift f) (unlift g)
  write = lift' write
#ifdef _INLINE
  {-# INLINE read #-}
  {-# INLINE write #-}
#else
  {-# NOINLINE read #-}
  {-# NOINLINE write #-}
#endif

type instance Fix x y (WriterT w c) = WriterT w (Fix (Dom (WriterT w) x y) (Cod (WriterT w) x y) c)
instance (Monoid w, ArrowFix x (w,y) c) => ArrowFix x y (WriterT w c) where
  fix = liftFix
#ifdef _INLINE
  {-# INLINE fix #-}
#else
  {-# NOINLINE fix #-}
#endif

instance (Monoid w, ArrowLowerBounded c) => ArrowLowerBounded (WriterT w c) where
  bottom = lift bottom
#ifdef _INLINE
  {-# INLINE bottom #-}
#else
  {-# NOINLINE bottom #-}
#endif

instance (Monoid w, O.Complete w, ArrowJoin c) => ArrowJoin (WriterT w c) where
  joinSecond g = lift $ rmap shuffle1 (joinSecond (unlift g))
#ifdef _INLINE
  {-# INLINE joinSecond #-}
#else
  {-# NOINLINE joinSecond #-}
#endif

instance (Monoid w, ArrowComplete (w,y) c) => ArrowComplete y (WriterT w c) where
  f <⊔> g = lift $ unlift f <⊔> unlift g
#ifdef _INLINE
  {-# INLINE (<⊔>) #-}
#else
  {-# NOINLINE (<⊔>) #-}
#endif

instance (Monoid w, ArrowRand v c) => ArrowRand v (WriterT w c) where
  random = lift' random
#ifdef _INLINE
  {-# INLINE random #-}
#else
  {-# NOINLINE random #-}
#endif

instance (Monoid w, ArrowConst x c) => ArrowConst x (WriterT w c) where
  askConst f = lift (askConst (unlift . f))
#ifdef _INLINE
  {-# INLINE askConst #-}
#else
  {-# NOINLINE askConst #-}
#endif
