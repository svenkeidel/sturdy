{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DataKinds #-}
module Control.Arrow.Transformer.State(StateT(..),evalStateT,execStateT) where

import           Prelude hiding (id,(.),lookup,read,fail)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Environment as Env
import           Control.Arrow.Fail as Fail
import           Control.Arrow.Fix
import           Control.Arrow.Trans
import           Control.Arrow.Random
import           Control.Arrow.Reader as Reader
import           Control.Arrow.State as State
import           Control.Arrow.Store as Store
import           Control.Arrow.Except as Exc
import           Control.Arrow.Writer
import           Control.Arrow.Order

import           Data.Coerce
import           Unsafe.Coerce
import qualified Data.Order as O
import           Data.Monoidal
import           Data.Profunctor hiding (Strong(..))
import           Data.Profunctor.Unsafe

import           GHC.TypeLits

-- Due to "Generalising Monads to Arrows", by John Hughes, in Science of Computer Programming 37.
newtype StateT s c x y = StateT { runStateT :: c (s,x) (s,y) }

evalStateT :: (Arrow c, Profunctor c) => StateT s c x y -> c (s,x) y
evalStateT f = rmap snd $ runStateT f
{-# INLINE evalStateT #-}

execStateT :: (Arrow c, Profunctor c) => StateT s c x y -> c (s,x) s
execStateT f = rmap fst $ runStateT f
{-# INLINE execStateT #-}

instance ArrowRun c => ArrowRun (StateT s c) where
  type Rep (StateT s c) x y = Rep c (s,x) (s,y)
  run = run . runStateT
  {-# INLINE run #-}

instance (Profunctor c, Arrow c) => Profunctor (StateT s c) where
#ifdef PROFUNCTOR
  dimap f g h = lift $ dimap (second' f) (second' g) (unlift h)
  lmap f h = lift $ lmap (second' f) (unlift h)
  rmap g h = lift $ rmap (second' g) (unlift h)
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
 
instance ArrowTrans (StateT s) where
  type Dom (StateT s) x y = (s,x)
  type Cod (StateT s) x y = (s,y)
  lift = coerce
  unlift = coerce
#ifdef _INLINE
  {-# INLINE lift #-}
  {-# INLINE unlift #-}
#else
  {-# NOINLINE lift #-}
  {-# NOINLINE unlift #-}
#endif

instance ArrowLift (StateT s) where
  lift' f = lift (second f)
#ifdef _INLINE
  {-# INLINE lift' #-}
#else
  {-# NOINLINE lift' #-}
#endif

instance (Arrow c, Profunctor c) => Category (StateT s c) where
  id = lift id
  f . g = lift (unlift f . unlift g)
#ifdef _INLINE
  {-# INLINE id #-}
  {-# INLINE (.) #-}
#else
  {-# NOINLINE id #-}
  {-# NOINLINE (.) #-}
#endif

instance (Arrow c, Profunctor c) => Arrow (StateT s c) where
  arr f = lift (arr (second' f))
  first f = lift $ dimap assoc1 assoc2 (first (unlift f)) 
  second f = lift $ dimap shuffle1 shuffle2 (second (unlift f))
  f &&& g = lmap (\x -> (x,x)) (f *** g)
  f *** g = lift $ dimap assoc1 shuffle3 (first (unlift f))
                   >>> rmap shuffle2 (second (unlift g))
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

instance (ArrowChoice c, Profunctor c) => ArrowChoice (StateT s c) where
  left f = lift $ dimap distribute1 distribute2 (left (unlift f)) 
  right f = lift $ dimap distribute1 distribute2 (right (unlift f))
  f ||| g = lift $ lmap distribute1 (unlift f ||| unlift g)
  f +++ g = lift $ dimap distribute1 distribute2 (unlift f +++ unlift g)
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

instance (ArrowApply c, Profunctor c) => ArrowApply (StateT s c) where
  app = lift $ lmap (\(s,(f,b)) -> (unlift f,(s,b))) app
#ifdef _INLINE
  {-# INLINE app #-}
#else
  {-# NOINLINE app #-}
#endif

instance (Arrow c, Profunctor c) => ArrowState s (StateT s c) where
  get = lift (arr (\(a,()) -> (a,a)))
  put = lift (arr (\(_,s) -> (s,())))
  modify f = lift (dimap (\(s,x) -> (s,(x,s))) (\(_,(y,s)) -> (s,y)) (unlift f))
#ifdef _INLINE
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE modify #-}
#else
  {-# NOINLINE get #-}
  {-# NOINLINE put #-}
  {-# NOINLINE modify #-}
#endif

instance (ArrowFail e c, Profunctor c) => ArrowFail e (StateT s c) where
  fail = lift (lmap snd fail)
#ifdef _INLINE
  {-# INLINE fail #-}
#else
  {-# NOINLINE fail #-}
#endif

instance ArrowReader r c => ArrowReader r (StateT s c) where
  ask = lift' Reader.ask
  local f = lift $ lmap (\(s,(r,x)) -> (r,(s,x))) (Reader.local (unlift f))
#ifdef _INLINE
  {-# INLINE ask #-}
  {-# INLINE local #-}
#else
  {-# NOINLINE ask #-}
  {-# NOINLINE local #-}
#endif

instance ArrowWriter w c => ArrowWriter w (StateT s c) where
  tell = lift' tell
#ifdef _INLINE
  {-# INLINE tell #-}
#else
  {-# NOINLINE tell #-}
#endif

instance (ArrowEnv var val c) => ArrowEnv var val (StateT s c) where
  type instance Join y (StateT s c) = Env.Join (s,y) c 
  lookup f g = lift $ lmap (\(s,(v,a)) -> (v,(s,a)))
                    $ lookup (lmap (\(v,(s,a)) -> (s,(v,a))) (unlift f))
                             (unlift g)
  extend f = lift $ lmap (\(s,(var,val,x)) -> (var,val,(s,x))) (extend (unlift f))
#ifdef _INLINE
  {-# INLINE lookup #-}
  {-# INLINE extend #-}
#else
  {-# NOINLINE lookup #-}
  {-# NOINLINE extend #-}
#endif

instance (ArrowClosure var val env c) => ArrowClosure var val env (StateT s c) where
  ask = lift' Env.ask
  local f = lift $ lmap (\(r,(env,a)) -> (env,(r,a))) (Env.local (unlift f))
#ifdef _INLINE
  {-# INLINE ask #-}
  {-# INLINE local #-}
#else
  {-# NOINLINE ask #-}
  {-# NOINLINE local #-}
#endif

instance (ArrowStore var val c) => ArrowStore var val (StateT s c) where
  type instance Join y (StateT s c) = Store.Join (s,y) c 
  read f g = lift $ lmap (\(s,(v,a)) -> (v,(s,a)))
                  $ read (lmap (\(v,(s,a)) -> (s,(v,a))) (unlift f))
                         (unlift g)
  write = lift' write
#ifdef _INLINE
  {-# INLINE read #-}
  {-# INLINE write #-}
#else
  {-# NOINLINE read #-}
  {-# NOINLINE write #-}
#endif

type instance Fix x y (StateT s c) = StateT s (Fix (Dom (StateT s) x y) (Cod (StateT s) x y) c)
instance ArrowFix (s,x) (s,y) c => ArrowFix x y (StateT s c) where
  fix = liftFix
#ifdef _INLINE
  {-# INLINE fix #-}
#else
  {-# NOINLINE fix #-}
#endif

instance (ArrowExcept e c) => ArrowExcept e (StateT s c) where
  type instance Join y (StateT s c) = Exc.Join (s,y) c 
  throw = lift (lmap snd throw)
  try f g h = lift $ try (unlift f) (unlift g) (lmap assoc2 (unlift h))
#ifdef _INLINE
  {-# INLINE throw #-}
  {-# INLINE try #-}
#else
  {-# NOINLINE throw #-}
  {-# NOINLINE try #-}
#endif

instance (ArrowLowerBounded c) => ArrowLowerBounded (StateT s c) where
  bottom = lift $ bottom
#ifdef _INLINE
  {-# INLINE bottom #-}
#else
  {-# NOINLINE bottom #-}
#endif

instance (ArrowJoin c, O.Complete s) => ArrowJoin (StateT s c) where
  joinSecond g = lift $ dimap (\(s,(z,x)) -> ((s,z),(s,x))) (\((s,z),(s',x)) -> (s O.⊔ s',(z,x))) (joinSecond (unlift g))
#ifdef _INLINE
  {-# INLINE joinSecond #-}
#else
  {-# NOINLINE joinSecond #-}
#endif

instance (ArrowComplete (s,y) c) => ArrowComplete y (StateT s c) where
  f <⊔> g = lift $ unlift f <⊔> unlift g
#ifdef _INLINE
  {-# INLINE (<⊔>) #-}
#else
  {-# NOINLINE (<⊔>) #-}
#endif

instance ArrowConst x c => ArrowConst x (StateT s c) where
  askConst f = lift (askConst (unlift . f))
#ifdef _INLINE
  {-# INLINE askConst #-}
#else
  {-# NOINLINE askConst #-}
#endif

instance ArrowRand v c => ArrowRand v (StateT s c) where
  random = lift' random
#ifdef _INLINE
  {-# INLINE random #-}
#else
  {-# INLINE random #-}
#endif

instance (TypeError ('Text "StateT is not effect commutative since it allows non-monotonic changes to the state."), Arrow c, Profunctor c)
  => ArrowEffectCommutative (StateT s c)

second' :: (x -> y) -> ((z,x) -> (z,y))
second' f (x,y) = (x,f y)
{-# INLINE second' #-}
