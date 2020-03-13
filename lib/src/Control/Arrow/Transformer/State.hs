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
import           Control.Arrow.Strict
import           Control.Arrow.Cont
import           Control.Arrow.Const
import           Control.Arrow.LetRec
import           Control.Arrow.Environment as Env
import           Control.Arrow.Closure as Cls
import           Control.Arrow.Except as Exc
import           Control.Arrow.Fail as Fail
import           Control.Arrow.Fix
import           Control.Arrow.Fix.Metrics
import           Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Fix.ControlFlow as CF
import           Control.Arrow.Fix.Chaotic as Chaotic
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Fix.Context as Context
import           Control.Arrow.Fix.Widening
import           Control.Arrow.Order
import           Control.Arrow.Primitive
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
instance (ArrowPrimitive c) => ArrowPrimitive (StateT s c) where type PrimState (StateT s c) = PrimState c

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

instance (ArrowCont c, Profunctor c) => ArrowCont (StateT s c) where
  type Cont (StateT s c) x = Cont c (s,x)
  callCC f = lift $ callCC $ \k -> unlift (f k)
  jump k = lift $ jump k
  {-# INLINE callCC #-}
  {-# INLINE jump #-}

instance (Arrow c, Profunctor c) => ArrowState s (StateT s c) where
  get = lift (arr (\(a,()) -> (a,a)))
  put = lift (arr (\(_,s) -> (s,())))
  modify f = lift (dimap (\(s,x) -> (s,(x,s))) (\(_,(y,s)) -> (s,y)) (unlift f))
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE modify #-}

instance (ArrowFail e c, Profunctor c) => ArrowFail e (StateT s c) where
  type Join x (StateT s c) = Fail.Join (s,x) c
  fail = lift (lmap snd fail)
  {-# INLINE fail #-}

instance ArrowReader r c => ArrowReader r (StateT s c) where
  ask = lift' Reader.ask
  local f = lift $ lmap shuffle1 (Reader.local (unlift f))
  {-# INLINE ask #-}
  {-# INLINE local #-}

instance ArrowWriter w c => ArrowWriter w (StateT s c) where
  tell = lift' tell
  {-# INLINE tell #-}

instance (ArrowEnv var val c) => ArrowEnv var val (StateT s c) where
  type Join y (StateT s c) = Env.Join (s,y) c
  lookup f g = lift $ lmap shuffle1
                    $ Env.lookup (lmap shuffle1 (unlift f))
                             (unlift g)
  extend f = lift $ lmap (\(s,(var,val,x)) -> (var,val,(s,x))) (Env.extend (unlift f))
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance (Profunctor c, ArrowLetRec var val c) => ArrowLetRec var val (StateT s c) where
  letRec f = lift $ lmap (\(s,(vs,x)) -> (vs,(s,x))) (letRec (unlift f))
  {-# INLINE letRec #-}

instance ArrowClosure expr cls c => ArrowClosure expr cls (StateT s c) where
  type Join y cls (StateT s c) = Cls.Join (s,y) cls c
  apply f = lift $ lmap shuffle1 (Cls.apply (lmap shuffle1 (unlift f)))
  {-# INLINE apply #-}

instance (ArrowStore var val c) => ArrowStore var val (StateT s c) where
  type Join y (StateT s c) = Store.Join (s,y) c
  read f g = lift $ lmap (\(s,(v,a)) -> (v,(s,a)))
                  $ Store.read (lmap (\(v,(s,a)) -> (s,(v,a))) (unlift f))
                               (unlift g)
  write = lift' Store.write
  {-# INLINE read #-}
  {-# INLINE write #-}

instance ArrowFix (Underlying (StateT s c) x y) => ArrowFix (StateT s c x y) where
  type Fix (StateT s c x y) = Fix (Underlying (StateT s c) x y)


instance (ArrowExcept e c) => ArrowExcept e (StateT s c) where
  type Join y (StateT s c) = Exc.Join (s,y) c
  throw = lift (lmap snd throw)
  try f g h = lift $ try (unlift f) (unlift g) (lmap assoc2 (unlift h))
  {-# INLINE throw #-}
  {-# INLINE try #-}

instance (ArrowLowerBounded y c) => ArrowLowerBounded y (StateT s c) where
  bottom = lift' bottom
  {-# INLINE bottom #-}

instance (ArrowJoin c, O.Complete s) => ArrowJoin (StateT s c) where
  joinSecond lub f g = lift $ joinSecond (\(s1,y1) (s2,y2) -> (s1 O.⊔ s2, lub y1 y2)) (second f) (unlift g)
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

instance ArrowContext ctx c => ArrowContext ctx (StateT s c) where
  localContext f = lift (lmap shuffle1 (localContext (unlift f)))
  {-# INLINE localContext #-}

instance ArrowWidening y c => ArrowWidening y (StateT s c) where
  widening = lift' widening
  {-# INLINE widening #-}

instance ArrowControlFlow stmt c => ArrowControlFlow stmt (StateT s c) where
  nextStatement f = lift $ lmap shuffle1 (nextStatement (unlift f))
  {-# INLINE nextStatement #-}

instance ArrowStack a c => ArrowStack a (StateT s c) where
  push f = lift $ lmap shuffle1 (push (unlift f))
  {-# INLINE push #-}

instance ArrowCache a b c => ArrowCache a b (StateT s c) where
  type Widening (StateT s c) = Cache.Widening c

instance ArrowJoinContext a c => ArrowJoinContext a (StateT s c) where
  type Widening (StateT s c) = Context.Widening c

instance ArrowParallelCache a b c => ArrowParallelCache a b (StateT s c)
instance ArrowIterateCache c => ArrowIterateCache (StateT s c)
instance ArrowFiltered a c => ArrowFiltered a (StateT s c)
instance ArrowStackDepth c => ArrowStackDepth (StateT s c)
instance ArrowStackElements a c => ArrowStackElements a (StateT s c)
instance ArrowTopLevel c => ArrowTopLevel (StateT s c)
instance ArrowComponent a c => ArrowComponent a (StateT s c)
instance ArrowInComponent a c => ArrowInComponent a (StateT s c)
instance ArrowStrict c => ArrowStrict (StateT s c)

second' :: (x -> y) -> ((z,x) -> (z,y))
second' f (x,y) = (x,f y)
{-# INLINE second' #-}
