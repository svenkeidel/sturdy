{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DataKinds #-}
module Control.Arrow.Transformer.State(StateT(..),evalStateT,execStateT,withStateT) where

import           Prelude hiding (id,(.),lookup,read,fail)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Environment as Env
import           Control.Arrow.Except as Exc
import           Control.Arrow.Fail as Fail
import           Control.Arrow.Fix
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Fix.Context
import           Control.Arrow.Fix.Widening
import           Control.Arrow.Order
import           Control.Arrow.Random
import           Control.Arrow.Reader as Reader
import           Control.Arrow.State as State
import           Control.Arrow.Store as Store
import           Control.Arrow.Trans
import           Control.Arrow.Writer

import           Unsafe.Coerce
import qualified Data.Order as O
import           Data.Monoidal
import           Data.Profunctor hiding (Strong(..))
import           Data.Profunctor.Unsafe

import           GHC.TypeLits

-- Due to "Generalising Monads to Arrows", by John Hughes, in Science of Computer Programming 37.
newtype StateT s c x y = StateT { runStateT :: c (s,x) (s,y) }

evalStateT :: Profunctor c => StateT s c x y -> c (s,x) y
evalStateT f = rmap snd $ runStateT f
{-# INLINE evalStateT #-}

execStateT :: Profunctor c => StateT s c x y -> c (s,x) s
execStateT f = rmap fst $ runStateT f
{-# INLINE execStateT #-}

withStateT :: Arrow c => StateT s' c x y -> StateT s c (s',x) (s',y)
withStateT f = lift (second (unlift f))
{-# INLINE withStateT #-}

instance ArrowRun c => ArrowRun (StateT s c) where type Run (StateT s c) x y = Run c (s,x) (s,y)
instance ArrowTrans (StateT s c) where type Underlying (StateT s c) x y = c (s,x) (s,y)

instance (Profunctor c) => Profunctor (StateT s c) where
  dimap f g h = lift $ dimap (second' f) (second' g) (unlift h)
  lmap f h = lift $ lmap (second' f) (unlift h)
  rmap g h = lift $ rmap (second' g) (unlift h)
  f .# _ = f `seq` unsafeCoerce f
  _ #. g = g `seq` unsafeCoerce g
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}
  {-# INLINE (.#) #-}
  {-# INLINE (#.) #-}
 
instance ArrowLift (StateT s) where
  lift' f = lift (second f)
  {-# INLINE lift' #-}

instance (Arrow c) => Category (StateT s c) where
  id = lift id
  f . g = lift (unlift f . unlift g)
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance (Arrow c, Profunctor c) => Arrow (StateT s c) where
  arr f = lift (arr (second' f))
  first f = lift $ dimap assoc1 assoc2 (first (unlift f)) 
  second f = lift $ dimap shuffle1 shuffle2 (second (unlift f))
  f &&& g = lmap (\x -> (x,x)) (f *** g)
  f *** g = lift $ dimap assoc1 shuffle3 (first (unlift f))
                   >>> rmap shuffle2 (second (unlift g))
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (***) #-}

instance (ArrowChoice c, Profunctor c) => ArrowChoice (StateT s c) where
  left f = lift $ dimap distribute1 distribute2 (left (unlift f)) 
  right f = lift $ dimap distribute1 distribute2 (right (unlift f))
  f ||| g = lift $ lmap distribute1 (unlift f ||| unlift g)
  f +++ g = lift $ dimap distribute1 distribute2 (unlift f +++ unlift g)
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (+++) #-}
  {-# INLINE (|||) #-}

instance (ArrowApply c, Profunctor c) => ArrowApply (StateT s c) where
  app = lift $ lmap (\(s,(f,b)) -> (unlift f,(s,b))) app
  {-# INLINE app #-}

instance (Arrow c, Profunctor c) => ArrowState s (StateT s c) where
  get = lift (arr (\(a,()) -> (a,a)))
  put = lift (arr (\(_,s) -> (s,())))
  modify f = lift (dimap (\(s,x) -> (s,(x,s))) (\(_,(y,s)) -> (s,y)) (unlift f))
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE modify #-}

instance (ArrowFail e c, Profunctor c) => ArrowFail e (StateT s c) where
  fail = lift (lmap snd fail)
  {-# INLINE fail #-}

instance ArrowReader r c => ArrowReader r (StateT s c) where
  ask = lift' Reader.ask
  local f = lift $ lmap (\(s,(r,x)) -> (r,(s,x))) (Reader.local (unlift f))
  {-# INLINE ask #-}
  {-# INLINE local #-}

instance ArrowWriter w c => ArrowWriter w (StateT s c) where
  tell = lift' tell
  {-# INLINE tell #-}

instance (ArrowEnv var val c) => ArrowEnv var val (StateT s c) where
  type Join y (StateT s c) = Env.Join (s,y) c
  lookup f g = lift $ lmap (\(s,(v,a)) -> (v,(s,a)))
                    $ Env.lookup (lmap (\(v,(s,a)) -> (s,(v,a))) (unlift f))
                             (unlift g)
  extend f = lift $ lmap (\(s,(var,val,x)) -> (var,val,(s,x))) (Env.extend (unlift f))
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance (ArrowClosure var val env c) => ArrowClosure var val env (StateT s c) where
  ask = lift' Env.ask
  local f = lift $ lmap (\(r,(env,a)) -> (env,(r,a))) (Env.local (unlift f))
  {-# INLINE ask #-}
  {-# INLINE local #-}

instance (ArrowStore var val c) => ArrowStore var val (StateT s c) where
  type Join y (StateT s c) = Store.Join (s,y) c
  read f g = lift $ lmap (\(s,(v,a)) -> (v,(s,a)))
                  $ Store.read (lmap (\(v,(s,a)) -> (s,(v,a))) (unlift f))
                               (unlift g)
  write = lift' Store.write
  {-# INLINE read #-}
  {-# INLINE write #-}

type instance Fix (StateT s c) x y = StateT s (Fix c (s,x) (s,y))
instance ArrowFix (Underlying (StateT s c) x y) => ArrowFix (StateT s c x y)

instance (ArrowExcept e c) => ArrowExcept e (StateT s c) where
  type Join y (StateT s c) = Exc.Join (s,y) c
  throw = lift (lmap snd throw)
  try f g h = lift $ try (unlift f) (unlift g) (lmap assoc2 (unlift h))
  {-# INLINE throw #-}
  {-# INLINE try #-}

instance (ArrowLowerBounded c) => ArrowLowerBounded (StateT s c) where
  bottom = lift bottom
  {-# INLINE bottom #-}

instance (ArrowJoin c, O.Complete s) => ArrowJoin (StateT s c) where
  joinSecond g = lift $ dimap (\(s,(z,x)) -> ((s,z),(s,x))) (\((s,z),(s',x)) -> (s O.⊔ s',(z,x))) (joinSecond (unlift g))
  {-# INLINE joinSecond #-}

instance (ArrowComplete (s,y) c) => ArrowComplete y (StateT s c) where
  f <⊔> g = lift $ unlift f <⊔> unlift g
  {-# INLINE (<⊔>) #-}

instance ArrowConst x c => ArrowConst x (StateT s c) where
  askConst f = lift (askConst (unlift . f))
  {-# INLINE askConst #-}

instance ArrowRand v c => ArrowRand v (StateT s c) where
  random = lift' random
  {-# INLINE random #-}

instance ArrowContext ctx a c => ArrowContext ctx a (StateT s c) where
  type Widening (StateT s c) a = Widening c a
  askContext = lift' askContext
  localContext f = lift (lmap shuffle1 (localContext (unlift f)))
  joinByContext widen = lift' $ joinByContext widen
  {-# INLINE askContext #-}
  {-# INLINE localContext #-}
  {-# INLINE joinByContext #-}

instance ArrowWidening y c => ArrowWidening y (StateT s c) where
  widening = lift' widening
  {-# INLINE widening #-}

instance (ArrowCache a b c) => ArrowCache a b (StateT s c) where
  lookup = lift' Cache.lookup
  write = lift' Cache.write
  update = lift' Cache.update
  setStable = lift' Cache.setStable
  {-# INLINE lookup #-}
  {-# INLINE write #-}
  {-# INLINE update #-}
  {-# INLINE setStable #-}

instance (TypeError ('Text "StateT is not effect commutative since it allows non-monotonic changes to the state."), Arrow c, Profunctor c)
  => ArrowEffectCommutative (StateT s c)

second' :: (x -> y) -> ((z,x) -> (z,y))
second' f (x,y) = (x,f y)
{-# INLINE second' #-}
