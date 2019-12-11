{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Static where

import Prelude hiding (id,(.),lookup,read,fail,elem)

import Control.Category

import Control.Arrow
import Control.Arrow.Fix.Chaotic as Chaotic
import Control.Arrow.Fix.Cache as Cache
import Control.Arrow.Fix.Context as Ctx
import Control.Arrow.Fix.Stack as Stack
import Control.Arrow.Environment as Env
import Control.Arrow.Closure as Cls
import Control.Arrow.Except as Exc
import Control.Arrow.Fail
import Control.Arrow.Order
import Control.Arrow.Reader as Reader
import Control.Arrow.State as State
import Control.Arrow.Store as Store
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

instance (Applicative f) =>  ArrowTrans (StaticT f c) where
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

instance Applicative f => ArrowLift (StaticT f) where
  lift' = StaticT . pure
  {-# INLINE lift' #-}
  {-# SPECIALIZE instance ArrowLift (StaticT ((->) r)) #-}

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

instance (Applicative f, ArrowFail e c) => ArrowFail e (StaticT f c) where
  fail = lift' fail
  {-# INLINE fail #-}
  {-# SPECIALIZE instance ArrowFail e c => ArrowFail e (StaticT ((->) r) c) #-}

instance (Applicative f, ArrowExcept e c) => ArrowExcept e (StaticT f c) where
  type Join y (StaticT f c) = Exc.Join y c
  throw = lift' throw
  try (StaticT f) (StaticT g) (StaticT h) = StaticT $ try <$> f <*> g <*> h
  {-# INLINE throw #-}
  {-# INLINE try #-}
  {-# SPECIALIZE instance ArrowExcept e c => ArrowExcept e (StaticT ((->) r) c) #-}

instance (Applicative f, ArrowEnv var val c) => ArrowEnv var val (StaticT f c) where
  type Join y (StaticT f c) = Env.Join y c
  lookup (StaticT f) (StaticT g) = StaticT $ Env.lookup <$> f <*> g
  extend (StaticT f) = StaticT $ Env.extend <$> f
  {-# INLINE lookup #-}
  {-# INLINE extend #-}
  {-# SPECIALIZE instance ArrowEnv var val c => ArrowEnv var val (StaticT ((->) r) c) #-}

instance (Applicative f, ArrowClosure expr cls c) => ArrowClosure expr cls (StaticT f c) where
  type Join y (StaticT f c) = Cls.Join y c
  apply (StaticT f) = StaticT $ Cls.apply <$> f
  {-# INLINE apply #-}
  {-# SPECIALIZE instance ArrowClosure expr cls c => ArrowClosure expr cls (StaticT ((->) r) c) #-}

instance (Applicative f, ArrowLetRec var val c) => ArrowLetRec var val (StaticT f c) where
  letRec (StaticT f) = StaticT $ Env.letRec <$> f
  {-# INLINE letRec #-}
  {-# SPECIALIZE instance ArrowLetRec var val c => ArrowLetRec var val (StaticT ((->) r) c) #-}

instance (Applicative f, ArrowStore var val c) => ArrowStore var val (StaticT f c) where
  type Join y (StaticT f c) = Store.Join y c
  read (StaticT f) (StaticT g) = StaticT $ Store.read <$> f <*> g
  write = lift' Store.write
  {-# INLINE read #-}
  {-# INLINE write #-}
  {-# SPECIALIZE instance ArrowStore var val c => ArrowStore var val (StaticT ((->) r) c) #-}

instance (Applicative f, ArrowLowerBounded c) => ArrowLowerBounded (StaticT f c) where
  bottom = StaticT (pure bottom)
  {-# INLINE bottom #-}
  {-# SPECIALIZE instance ArrowLowerBounded c => ArrowLowerBounded (StaticT ((->) r) c) #-}

instance (Applicative f, ArrowJoin c) => ArrowJoin (StaticT f c) where
  joinSecond lub f (StaticT g) = StaticT $ joinSecond lub f <$> g
  {-# INLINE joinSecond #-}
  {-# SPECIALIZE instance ArrowJoin c => ArrowJoin (StaticT ((->) r) c) #-}

instance (Applicative f, ArrowComplete y c) => ArrowComplete y (StaticT f c) where
  StaticT f <⊔> StaticT g = StaticT $ (<⊔>) <$> f <*> g 
  {-# INLINE (<⊔>) #-}
  {-# SPECIALIZE instance ArrowComplete y c => ArrowComplete y (StaticT ((->) r) c) #-}

instance (Applicative f, ArrowContext ctx c) => ArrowContext ctx (StaticT f c) where
  localContext (StaticT f) = StaticT $ localContext <$> f
  {-# INLINE localContext #-}
  {-# SPECIALIZE instance ArrowContext ctx c => ArrowContext ctx (StaticT ((->) r) c) #-}

instance (Applicative f, ArrowStack a c) => ArrowStack a (StaticT f c) where
  peek = lift' peek
  size = lift' size
  push (StaticT f) = StaticT $ push <$> f
  {-# INLINE peek #-}
  {-# INLINE size #-}
  {-# INLINE push #-}
  {-# SPECIALIZE instance ArrowStack a c => ArrowStack a (StaticT ((->) r) c) #-}

instance (Applicative f, ArrowCache a b c) => ArrowCache a b (StaticT f c) where
  {-# SPECIALIZE instance ArrowCache a b c => ArrowCache a b (StaticT ((->) r) c) #-}

instance (Applicative f, ArrowChaotic a c) => ArrowChaotic a (StaticT f c) where
  getComponent (StaticT f) = StaticT $ Chaotic.getComponent <$> f
  {-# INLINE getComponent #-}
  {-# SPECIALIZE instance ArrowChaotic a c => ArrowChaotic a (StaticT ((->) r) c) #-}

