{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Control.Arrow.Transformer.Const where

import Prelude hiding (id,(.),lookup,read)

import Control.Category

import Control.Arrow
import Control.Arrow.Cont
import Control.Arrow.Const
import Control.Arrow.Environment
import Control.Arrow.Closure
import Control.Arrow.Except
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Fix.Cache
import Control.Arrow.Fix.Chaotic
import Control.Arrow.Fix.Context
import Control.Arrow.Fix.Stack
import Control.Arrow.Order
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store
import Control.Arrow.Trans
import Control.Arrow.Writer

import Control.Arrow.Transformer.Static

import Data.Profunctor
import Data.Coerce

-- | Passes along constant data.
newtype ConstT r c x y = ConstT (StaticT ((->) r) c x y)
  deriving (Category,Profunctor,Arrow,ArrowChoice,ArrowLowerBounded,ArrowLift,ArrowJoin,
            ArrowState s,ArrowReader r',ArrowWriter w, ArrowLetRec var val,
            ArrowEnv var val, ArrowClosure expr cls, ArrowStore var val,
            ArrowFail e, ArrowExcept e,
            ArrowContext ctx, ArrowStack a, ArrowCache a b, ArrowChaotic a)

constT :: (r -> c x y) -> ConstT r c x y
constT f = ConstT (StaticT f)
{-# INLINE constT #-}

runConstT :: r -> ConstT r c x y -> c x y
runConstT r (ConstT (StaticT f)) = f r
{-# INLINE runConstT #-}

liftConstT :: (c x y -> c' x' y') -> ConstT r c x y -> ConstT r c' x' y'
liftConstT f g = lift $ \r -> f (unlift g r)
{-# INLINE liftConstT #-}

mapConstT :: (r' -> r) -> (ConstT r c x y -> ConstT r' c x y)
mapConstT g (ConstT (StaticT f)) = constT $ \r' -> f (g r')
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
  askConst f = ConstT $ StaticT $ \r -> runConstT r (f r)
  {-# INLINE askConst #-}

instance (Arrow c, Profunctor c, ArrowFix (c x y)) => ArrowFix (ConstT r c x y) where
  fix f = ConstT $ StaticT $ \r -> fix (runConstT r . f . lift')
  {-# INLINE fix #-}

instance (ArrowApply c, Profunctor c) => ArrowApply (ConstT r c) where
  app = ConstT $ StaticT $ \r -> lmap (\(f,x) -> (coerce f r,x)) app
  {-# INLINE app #-}

instance ArrowCont c => ArrowCont (ConstT r c) where
  type Cont (ConstT r c) y = Cont c y
  callCC f = lift $ \r -> callCC $ \k -> unlift (f k) r
  jump k = lift $ \_ -> jump k
  {-# INLINE callCC #-}

deriving instance ArrowComplete y c =>  ArrowComplete y (ConstT r c)

type instance Fix (ConstT r c) x y = ConstT r (Fix c x y)
