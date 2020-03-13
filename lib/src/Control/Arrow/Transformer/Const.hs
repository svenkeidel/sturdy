{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Control.Arrow.Transformer.Const where

import Prelude hiding (id,(.),lookup,read)

import Control.Category

import Control.Arrow
import Control.Arrow.Strict
import Control.Arrow.Cont
import Control.Arrow.Const
import Control.Arrow.Environment as Env
import Control.Arrow.Closure as Cls
import Control.Arrow.Except as Exc
import Control.Arrow.Fail as Fail
import Control.Arrow.Fix
import Control.Arrow.Fix.ControlFlow
import Control.Arrow.Fix.Cache
import Control.Arrow.Fix.Chaotic
import Control.Arrow.Fix.Context
import Control.Arrow.Fix.Stack
import Control.Arrow.Order
import Control.Arrow.Primitive
import Control.Arrow.Store as Store
import Control.Arrow.Trans
import Control.Arrow.Reader as Reader
import Control.Arrow.State as State
import Control.Arrow.Writer as Writer
import Control.Arrow.LetRec as LetRec

import Data.Profunctor.Unsafe
import Unsafe.Coerce

-- | Arrow transformer that passes along constant data.
newtype ConstT r c x y = ConstT (r -> c x y)
  -- deriving (Category,Profunctor,Arrow,ArrowChoice,ArrowLowerBounded a,
  --           ArrowLift,ArrowJoin,ArrowPrimitive,ArrowStrict,
  --           ArrowState s,ArrowReader r',ArrowWriter w, ArrowLetRec var val,
  --           ArrowEnv var val, ArrowClosure expr cls, ArrowStore var val,
  --           ArrowFail e, ArrowExcept e,
  --           ArrowContext ctx, ArrowStack a, ArrowCache a b, ArrowComponent a,ArrowControlFlow stmt)

constT :: (r -> c x y) -> ConstT r c x y
constT = lift
{-# INLINE constT #-}

runConstT :: r -> ConstT r c x y -> c x y
runConstT r f = unlift f r
{-# INLINE runConstT #-}

liftConstT :: (c x y -> c' x' y') -> ConstT r c x y -> ConstT r c' x' y'
liftConstT f g = lift $ \r -> f (unlift g r)
{-# INLINE liftConstT #-}

unliftConstT :: r -> (ConstT r c x y -> ConstT r c' x' y') -> (c x y -> c' x' y')
unliftConstT r f x = runConstT r (f (constT $ const x))
{-# INLINE unliftConstT #-}

mapConstT :: (r' -> r) -> (ConstT r c x y -> ConstT r' c x y)
mapConstT g f = constT $ \r' -> unlift f (g r')
{-# INLINE mapConstT #-}

setConstT :: r -> (ConstT r c x y -> ConstT r' c x y)
setConstT r = mapConstT (const r)
{-# INLINE setConstT #-}

instance ArrowTrans (ConstT r c) where
  type Underlying (ConstT r c) x y = r -> c x y

instance ArrowRun c => ArrowRun (ConstT r c) where
  type Run (ConstT r c) x y = r -> Run c x y
  run f r = run (runConstT r f)
  {-# INLINE run #-}

instance (Arrow c, Profunctor c) => ArrowConst r (ConstT r c) where
  askConst f = lift $ \r -> unlift (f r) r
  {-# INLINE askConst #-}

instance (Arrow c, Profunctor c, ArrowFix (c x y)) => ArrowFix (ConstT r c x y) where
  type Fix (ConstT r c x y) = Fix (c x y)
  fix f = lift $ \r -> fix (runConstT r . f . lift')
  {-# INLINE fix #-}

instance (ArrowApply c, Profunctor c) => ArrowApply (ConstT r c) where
  app = lift $ \r -> lmap (\(f,x) -> (unlift f r,x)) app
  {-# INLINE app #-}

instance ArrowCont c => ArrowCont (ConstT r c) where
  type Cont (ConstT r c) y = Cont c y
  callCC f = lift $ \r -> callCC $ \k -> unlift (f k) r
  jump k = lift $ \_ -> jump k
  {-# INLINE callCC #-}

instance Category c => Category (ConstT r c) where
  id = lift $ \_ -> id
  f . g = lift $ \r -> unlift f r . unlift g r
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance Profunctor c => Profunctor (ConstT r c) where
  dimap f g h = lift $ \r -> dimap f g (unlift h r)
  lmap f h = lift $ \r -> lmap f (unlift h r)
  rmap f h = lift $ \r -> rmap f (unlift h r)
  f .# _ = f `seq` unsafeCoerce f
  _ #. g = g `seq` unsafeCoerce g
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}
  {-# INLINE (.#) #-}
  {-# INLINE ( #.) #-}

instance Arrow c => Arrow (ConstT r c) where
  arr f = lift $ \_ -> arr f
  first f = lift $ \r -> first (unlift f r)
  second f = lift $ \r -> second (unlift f r)
  f *** g = lift $ \r -> unlift f r *** unlift g r
  f &&& g = lift $ \r -> unlift f r &&& unlift g r
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (&&&) #-}
  {-# INLINE (***) #-}

instance ArrowChoice c => ArrowChoice (ConstT r c) where
  left f = lift $ \r -> left (unlift f r)
  right f = lift $ \r -> right (unlift f r)
  f +++ g = lift $ \r -> unlift f r +++ unlift g r
  f ||| g = lift $ \r -> unlift f r ||| unlift g r
  {-# INLINE left #-}
  {-# INLINE right #-}
  {-# INLINE (+++) #-}
  {-# INLINE (|||) #-}

instance ArrowLowerBounded y c => ArrowLowerBounded y (ConstT r c) where
  bottom = lift $ \_ -> bottom
  {-# INLINE bottom #-}

instance ArrowLift (ConstT r) where
  lift' f = lift $ \_ -> f
  {-# INLINE lift' #-}

instance ArrowJoin c => ArrowJoin (ConstT r c) where
  joinSecond lub f g = lift $ \r -> joinSecond lub f (unlift g r)
  {-# INLINE joinSecond #-}

instance ArrowComplete y c => ArrowComplete y (ConstT r c) where
  f <⊔> g = lift $ \r -> unlift f r <⊔> unlift g r
  {-# INLINE (<⊔>) #-}

instance ArrowPrimitive c => ArrowPrimitive (ConstT r c) where
  type PrimState (ConstT r c) = PrimState c

instance ArrowState s c => ArrowState s (ConstT r c) where
  get = lift $ \_ -> State.get
  put = lift $ \_ -> State.put
  modify f = lift $ \r -> State.modify (unlift f r)
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE modify #-}

instance ArrowReader r' c => ArrowReader r' (ConstT r c) where
  ask = lift $ \_ -> Reader.ask
  local f = lift $ \r -> Reader.local (unlift f r)
  {-# INLINE ask #-}
  {-# INLINE local #-}

instance ArrowWriter w c => ArrowWriter w (ConstT r c) where
  tell = lift $ \_ -> Writer.tell
  {-# INLINE tell #-}

instance ArrowFail e c => ArrowFail e (ConstT r c) where
  type Join x (ConstT r c) = Fail.Join x c
  fail = lift $ \_ -> Fail.fail
  {-# INLINE fail #-}

instance ArrowExcept e c => ArrowExcept e (ConstT r c) where
  type Join y (ConstT r c) = Exc.Join y c
  throw = lift $ \_ -> throw
  try f g h = lift $ \r -> try (unlift f r) (unlift g r) (unlift h r)
  {-# INLINE throw #-}
  {-# INLINE try #-}

instance ArrowEnv var val c => ArrowEnv var val (ConstT r c) where
  type Join y (ConstT r c) = Env.Join y c
  lookup f g = lift $ \r -> Env.lookup (unlift f r) (unlift g r)
  extend f = lift $ \r -> Env.extend (unlift f r)
  {-# INLINE lookup #-}
  {-# INLINE extend #-}

instance ArrowClosure expr cls c => ArrowClosure expr cls (ConstT r c) where
  type Join y cls (ConstT r c) = Cls.Join y cls c
  apply f = lift $ \r -> Cls.apply (unlift f r)
  {-# INLINE apply #-}

instance ArrowLetRec addr val c => ArrowLetRec addr val (ConstT r c) where
  letRec f = lift $ \r -> LetRec.letRec (unlift f r)
  {-# INLINE letRec #-}

instance (ArrowStore var val c) => ArrowStore var val (ConstT r c) where
  type Join y (ConstT r c) = Store.Join y c
  read f g = lift $ \r -> Store.read (unlift f r) (unlift g r)
  write = lift $ \_ -> Store.write
  {-# INLINE read #-}
  {-# INLINE write #-}

instance ArrowContext ctx c => ArrowContext ctx (ConstT r c) where
  localContext f = lift $ \r -> localContext (unlift f r)
  {-# INLINE localContext #-}

instance ArrowStack a c => ArrowStack a (ConstT r c) where
  push f = lift $ \r -> push (unlift f r)
  {-# INLINE push #-}

instance ArrowControlFlow stmt c => ArrowControlFlow stmt (ConstT r c) where
  nextStatement f = lift $ \r -> nextStatement (unlift f r)
  {-# INLINE nextStatement #-}

instance (ArrowStrict c) => ArrowStrict (ConstT r c) where
  force f = lift $ \r -> force (unlift f r)
  {-# INLINE force #-}

instance ArrowCache a b c => ArrowCache a b (ConstT r c)
instance ArrowComponent comp c => ArrowComponent comp (ConstT r c)
