{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.NoInline where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Category
import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Environment as Env
import Control.Arrow.Closure as Cls
import Control.Arrow.Except as Exc
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Fix.Cache as Cache
import Control.Arrow.Fix.Context as Context
import Control.Arrow.Order
import Control.Arrow.Reader as Reader
import Control.Arrow.State as State
import Control.Arrow.Store as Store
import Control.Arrow.Trans
import Control.Arrow.Writer

import Data.Profunctor
import Data.Profunctor.Unsafe
import Unsafe.Coerce

newtype NoInlineT c x y = NoInlineT { runNoInlineTT :: c x y }

instance ArrowRun c => ArrowRun (NoInlineT c) where
  type Run (NoInlineT c) x y = Run c x y

instance ArrowTrans (NoInlineT c) where
  type Underlying (NoInlineT c) x y = c x y

instance ArrowLift NoInlineT where
  lift' = NoInlineT
  {-# INLINE lift' #-}

instance Profunctor c => Profunctor (NoInlineT c) where
  dimap f g h = lift $ dimap f g (unlift h)
  lmap f h = lift $ lmap f (unlift h)
  rmap g h = lift $ rmap g (unlift h)
  f .# _ = f `seq` unsafeCoerce f
  _ #. g = g `seq` unsafeCoerce g
  {-# NOINLINE dimap #-}
  {-# NOINLINE lmap #-}
  {-# NOINLINE rmap #-}
  {-# NOINLINE (.#) #-}
  {-# NOINLINE (#.) #-}

-- instance ArrowLift NoInlineT where
--   lift' = lift
--   {-# NOINLINE lift' #-}

instance Category c => Category (NoInlineT c) where
  id    = lift id
  f . g = lift (unlift f . unlift g)
  {-# NOINLINE id #-}
  {-# NOINLINE (.) #-}

instance Arrow c => Arrow (NoInlineT c) where
  arr f    = lift (arr f)
  first f  = lift $ first (unlift f)
  second f = lift $ second (unlift f)
  f &&& g  = lift $ unlift f &&& unlift g
  f *** g  = lift $ unlift f *** unlift g
  {-# NOINLINE arr #-}
  {-# NOINLINE first #-}
  {-# NOINLINE second #-}
  {-# NOINLINE (&&&) #-}
  {-# NOINLINE (***) #-}

instance (ArrowChoice c) => ArrowChoice (NoInlineT c) where
  left f  = lift $ left (unlift f)
  right f = lift $ right (unlift f)
  f +++ g = lift $ unlift f +++ unlift g
  f ||| g = lift $ unlift f ||| unlift g
  {-# NOINLINE left #-}
  {-# NOINLINE right #-}
  {-# NOINLINE (+++) #-}
  {-# NOINLINE (|||) #-}

instance (ArrowApply c, Profunctor c) => ArrowApply (NoInlineT c) where
  app = lift $ lmap (\(f,b) -> (unlift f,b)) app
  {-# NOINLINE app #-}

instance ArrowReader r c => ArrowReader r (NoInlineT c) where
  ask = lift ask
  local f = lift $ local (unlift f)
  {-# NOINLINE ask #-}
  {-# NOINLINE local #-}

instance ArrowState s c => ArrowState s (NoInlineT c) where
  get = lift State.get
  put = lift State.put
  modify f = lift (State.modify (unlift f))
  {-# NOINLINE get #-}
  {-# NOINLINE put #-}
  {-# NOINLINE modify #-}

instance ArrowWriter w c => ArrowWriter w (NoInlineT c) where
  tell = lift tell
  {-# NOINLINE tell #-}

instance ArrowFail e c => ArrowFail e (NoInlineT c) where
  fail = lift fail
  {-# NOINLINE fail #-}

instance ArrowEnv var val c => ArrowEnv var val (NoInlineT c) where
  type Join y (NoInlineT c) = Env.Join y c
  lookup f g = lift (Env.lookup (unlift f) (unlift g))
  extend f = lift (Env.extend (unlift f))
  {-# NOINLINE lookup #-}
  {-# NOINLINE extend #-}

instance ArrowLetRec var val c => ArrowLetRec var val (NoInlineT c) where
  letRec f = lift (letRec (unlift f))
  {-# NOINLINE letRec #-}

instance ArrowClosure expr cls c => ArrowClosure expr cls (NoInlineT c) where
  type Join y (NoInlineT c) = Cls.Join y c
  closure = lift Cls.closure
  apply f = lift $ Cls.apply (unlift f)
  {-# NOINLINE closure #-}
  {-# NOINLINE apply #-}

instance ArrowStore var val c => ArrowStore var val (NoInlineT c) where
  type Join y (NoInlineT c) = Store.Join y c
  read f g = lift $ Store.read (unlift f) (unlift g)
  write = lift Store.write
  {-# NOINLINE read #-}
  {-# NOINLINE write #-}

type instance Fix (NoInlineT c) x y = NoInlineT (Fix c x y)
instance ArrowFix (Underlying (NoInlineT c) x y) => ArrowFix (NoInlineT c x y)

instance ArrowExcept e c => ArrowExcept e (NoInlineT c) where
  type Join z (NoInlineT c) = Exc.Join z c
  throw = lift throw
  try f g h = lift $ try (unlift f) (unlift g) (unlift h)
  {-# NOINLINE throw #-}
  {-# NOINLINE try #-}

instance ArrowLowerBounded c => ArrowLowerBounded (NoInlineT c) where
  bottom = lift bottom
  {-# NOINLINE bottom #-}

instance ArrowJoin c => ArrowJoin (NoInlineT c) where
  joinSecond lub f g = lift $ joinSecond lub f (unlift g)
  {-# NOINLINE joinSecond #-}

instance ArrowComplete y c => ArrowComplete y (NoInlineT c) where
  f <⊔> g = lift $ unlift f <⊔> unlift g
  {-# NOINLINE (<⊔>) #-}

instance ArrowConst x c => ArrowConst x (NoInlineT c) where
  askConst f = lift (askConst (unlift . f))
  {-# NOINLINE askConst #-}

instance ArrowEffectCommutative c => ArrowEffectCommutative (NoInlineT c)

instance ArrowContext ctx c => ArrowContext ctx (NoInlineT c) where
  localContext f = lift (localContext (unlift f))
  {-# NOINLINE localContext #-}

instance ArrowCache a b c => ArrowCache a b (NoInlineT c) where
  initialize = lift Cache.initialize
  lookup = lift Cache.lookup
  write = lift Cache.write
  update = lift Cache.update
  setStable = lift Cache.setStable
  {-# NOINLINE initialize #-}
  {-# NOINLINE lookup #-}
  {-# NOINLINE write #-}
  {-# NOINLINE update #-}
  {-# NOINLINE setStable #-}
