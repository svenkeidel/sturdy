{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
module Control.Arrow.Transformer.Abstract.Except(ExceptT,runExceptT) where

import Prelude hiding (id,lookup,(.),read,fail)

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Deduplicate
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store as Store
import Control.Arrow.Except
import Control.Arrow.Fix
import Control.Arrow.Abstract.Join
import Control.Arrow.Transformer.Kleisli
import Control.Category

import Data.Identifiable
import Data.Order
import Data.Profunctor
import Data.Abstract.Except
import Data.Abstract.Widening (toJoin2)
import Data.Coerce

newtype ExceptT e c x y = ExceptT (KleisliT (Except e) c x y)
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, ArrowState s, ArrowReader r,
            ArrowConst r, ArrowEnv a b e', ArrowStore a b, ArrowFail e')

runExceptT :: ExceptT e c x y -> c x (Except e y)
runExceptT = coerce

instance (ArrowChoice c, Complete e, ArrowJoin c) => ArrowExcept e (ExceptT e c) where
  type Join (ExceptT e c) (y,(x,e)) z = Complete (c (y,(x,e)) (Except e z))
  throw = lift $ arr Fail
  try f g h = lift $ proc x -> do
    e <- unlift f -< x
    case e of
      Success y          -> unlift g -< y
      Fail er            -> unlift h -< (x,er)
      SuccessOrFail er y -> joined (unlift g) (unlift h) -< (y,(x,er))

instance (Complete e, ArrowJoin c, ArrowChoice c, ArrowApply c, Profunctor c) => ArrowApply (ExceptT e c) where app = lift $ lmap (first unlift) app
type instance Fix x y (ExceptT e c) = ExceptT e (Fix (Dom (ExceptT e) x y) (Cod (ExceptT e) x y) c)
deriving instance (Complete e, ArrowJoin c, ArrowChoice c, ArrowFix (Dom (ExceptT e) x y) (Cod (ExceptT e) x y) c) => ArrowFix x y (ExceptT e c)
deriving instance (Complete e, ArrowJoin c, Identifiable (Cod (ExceptT e) x y), ArrowChoice c, ArrowDeduplicate (Dom (ExceptT e) x y) (Cod (ExceptT e) x y) c) => ArrowDeduplicate x y (ExceptT e c)

deriving instance PreOrd (c x (Cod (ExceptT e) x y)) => PreOrd (ExceptT e c x y)
deriving instance LowerBounded (c (Dom (ExceptT e) x y) (Cod (ExceptT e) x y)) => LowerBounded (ExceptT e c x y)
deriving instance Complete (c (Dom (ExceptT e) x y) (Cod (ExceptT e) x y)) => Complete (ExceptT e c x y)
deriving instance CoComplete (c (Dom (ExceptT e) x y) (Cod (ExceptT e) x y)) => CoComplete (ExceptT e c x y)
deriving instance UpperBounded (c (Dom (ExceptT e) x y) (Cod (ExceptT e) x y)) => UpperBounded (ExceptT e c x y)

instance (Complete e, ArrowJoin c, ArrowChoice c) => ArrowJoin (ExceptT e c) where
  joinWith lub' f g = lift $ joinWith (toJoin2 widening (⊔) lub') (unlift f) (unlift g)

