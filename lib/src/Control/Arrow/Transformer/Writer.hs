{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Arrow.Transformer.Writer where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Category
import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Fix.Cache as Cache
import Control.Arrow.Fix.Stack as Stack
import Control.Arrow.Fix.Context as Context
import Control.Arrow.Environment as Env
import Control.Arrow.Closure as Cls
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

import qualified Data.Order as O
import Data.Monoidal
import Data.Profunctor
import Data.Profunctor.Unsafe
import Data.Coerce
import Unsafe.Coerce

newtype WriterT w c x y = WriterT { runWriterT :: c x (w,y) }

censor :: (Arrow c,Profunctor c) => (x -> w -> w) -> WriterT w c x y -> WriterT w c x y
censor f (WriterT g) = WriterT (dimap (\x -> (x,x)) (\(x,(w,y)) -> (f x w,y)) (second g))

instance (Monoid w,ArrowRun c) => ArrowRun (WriterT w c) where
  type Run (WriterT w c) x y = Run c x (w,y)

instance ArrowTrans (WriterT w c) where
  type Underlying (WriterT w c) x y = c x (w,y)

instance (Profunctor c) => Profunctor (WriterT w c) where
  dimap f g h = lift $ dimap f (second g) (unlift h)
  lmap f h = lift $ lmap f (unlift h)
  rmap g h = lift $ rmap (second g) (unlift h)
  f .# _ = f `seq` unsafeCoerce f
  _ #. g = g `seq` unsafeCoerce g
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}
  {-# INLINE (.#) #-}
  {-# INLINE (#.) #-}

instance Monoid w => ArrowLift (WriterT w) where
  lift' f = lift (rmap (mempty,) f)
  {-# INLINE lift' #-}

instance (Monoid w, Arrow c, Profunctor c) => Category (WriterT w c) where
  id = lift' id
  g . f = lift $ rmap (\(w1,(w2,z)) -> (w1 <> w2,z)) (unlift f >>> second (unlift g)) 
  -- proc x -> do
  --   (w1,y) <- f -< x
  --   (w2,z) <- g -< y
  --   returnA -< (w1 <> w2,z)
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance (Monoid w, Arrow c, Profunctor c) => Arrow (WriterT w c) where
  arr f = lift' (arr f)
  first f = lift $ rmap assoc2 (first (unlift f)) 
  second g = lift $ rmap shuffle2 (second (unlift g)) 
  f *** g = lift $ rmap (\((w1,b),(w2,d)) -> (w1 <> w2,(b,d))) (unlift f *** unlift g) 
  f &&& g = lift $ rmap (\((w1,b),(w2,d)) -> (w1 <> w2,(b,d))) (unlift f &&& unlift g)
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (***) #-}

instance (Monoid w, ArrowChoice c, Profunctor c) => ArrowChoice (WriterT w c) where
  left f = lift $ rmap (\e -> case e of Left (w,x) -> (w,Left x); Right y -> (mempty,Right y)) (left (unlift f)) 
  right f = lift $ rmap (\e -> case e of Left x -> (mempty,Left x); Right (w,y) -> (w,Right y)) (right (unlift f))
  f ||| g = lift $ unlift f ||| unlift g
  f +++ g = lift $ rmap distribute2 (unlift f +++ unlift g) 
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (+++) #-}
  {-# INLINE (|||) #-}

instance (Monoid w, ArrowApply c, Profunctor c) => ArrowApply (WriterT w c) where
  app = lift (app .# first coerce)
  {-# INLINE app #-}

instance (Monoid w, ArrowState s c) => ArrowState s (WriterT w c) where
  get = lift' get
  put = lift' put
  modify f = lift $ modify (rmap assoc1 (unlift f))
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE modify #-}

instance (Monoid w, Arrow c, Profunctor c) => ArrowWriter w (WriterT w c) where
  tell = lift (arr (\w -> (w,())))
  {-# INLINE tell #-}

instance (Monoid w, ArrowFail e c) => ArrowFail e (WriterT w c) where
  fail = lift' fail
  {-# INLINE fail #-}

instance (Monoid w, ArrowExcept e c) => ArrowExcept e (WriterT w c) where
  type Join y (WriterT w c) = Exc.Join (w,y) c
  throw = lift' throw
  try f g h = lift $ try (unlift f) (rmap (\(w1,(w2,z)) -> (w1 <> w2,z)) (second (unlift g))) (unlift h)
  {-# INLINE throw #-}
  {-# INLINE try #-}

instance (Monoid w, ArrowReader r c) => ArrowReader r (WriterT w c) where
  ask = lift' Reader.ask
  local f = lift (Reader.local (unlift f))
  {-# INLINE ask #-}
  {-# INLINE local #-}

instance (Monoid w, ArrowEnv var val c) => ArrowEnv var val (WriterT w c) where
  type Join y (WriterT w c) = Env.Join (w,y) c
  lookup f g = lift $ Env.lookup (unlift f) (unlift g)
  extend f = lift $ Env.extend (unlift f)
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance (Monoid w, ArrowClosure expr cls c) => ArrowClosure expr cls (WriterT w c) where
  type Join y (WriterT w c) = Cls.Join (w,y) c
  apply f = lift (Cls.apply (unlift f))
  {-# INLINE apply #-}

instance (Monoid w, ArrowStore var val c) => ArrowStore var val (WriterT w c) where
  type Join y (WriterT w c) = Store.Join (w,y) c
  read f g = lift $ Store.read (unlift f) (unlift g)
  write = lift' Store.write
  {-# INLINE read #-}
  {-# INLINE write #-}

type instance Fix (WriterT w c) x y  = WriterT w (Fix c x (w,y))
deriving instance ArrowFix (Underlying (WriterT w c) x y) => ArrowFix (WriterT w c x y)

instance (Monoid w, ArrowLowerBounded c) => ArrowLowerBounded (WriterT w c) where
  bottom = lift bottom
  {-# INLINE bottom #-}

instance (Monoid w, O.Complete w, ArrowJoin c) => ArrowJoin (WriterT w c) where
  joinSecond lub f g = lift $ joinSecond (\(w1,y1) (w2,y2) -> (w1 O.⊔ w2, lub y1 y2) ) (\x -> (mempty,f x)) (unlift g)
  {-# INLINE joinSecond #-}

instance (Monoid w, ArrowComplete (w,y) c) => ArrowComplete y (WriterT w c) where
  f <⊔> g = lift $ unlift f <⊔> unlift g
  {-# INLINE (<⊔>) #-}

instance (Monoid w, ArrowRand v c) => ArrowRand v (WriterT w c) where
  random = lift' random
  {-# INLINE random #-}

instance (Monoid w, ArrowConst x c) => ArrowConst x (WriterT w c) where
  askConst f = lift (askConst (unlift . f))
  {-# INLINE askConst #-}

instance (Monoid w, ArrowStack a c) => ArrowStack a (WriterT w c) where
  push f = lift $ Stack.push (unlift f)
  {-# INLINE push #-}

instance (Monoid w, ArrowContext ctx c) => ArrowContext ctx (WriterT w c) where
  localContext f = lift (Context.localContext (unlift f))
  {-# INLINE localContext #-}

instance (Monoid w, ArrowJoinContext a c) => ArrowJoinContext a (WriterT w c)
instance (Monoid w, ArrowCache a b c) => ArrowCache a b (WriterT w c)
