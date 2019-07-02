{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
module Control.Arrow.Transformer.Abstract.Error(ErrorT,runErrorT) where

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
import Control.Arrow.Except as Exc
import Control.Arrow.Fix
import Control.Arrow.Abstract.Join
import Control.Arrow.Transformer.Kleisli
import Control.Category

import Data.Order
import Data.Profunctor
import Data.Identifiable
import Data.Abstract.Error
import Data.Abstract.Widening (toJoin2)

newtype ErrorT e c x y = ErrorT { unErrorT :: KleisliT (Error e) c x y }
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, ArrowState s, ArrowReader r,
            ArrowConst r, ArrowEnv a b e', ArrowStore a b, ArrowExcept e')

runErrorT :: ErrorT e c x y -> c x (Error e y)
runErrorT = runKleisliT . unErrorT

instance (ArrowChoice c, Profunctor c) => ArrowFail e (ErrorT e c) where
  fail = lift $ arr Fail

instance (ArrowChoice c, ArrowApply c, Profunctor c) => ArrowApply (ErrorT e c) where app = lift $ lmap (first unlift) app
type instance Fix x y (ErrorT e c) = ErrorT e (Fix (Dom (ErrorT e) x y) (Cod (ErrorT e) x y) c)
deriving instance (ArrowChoice c, ArrowFix (Dom (ErrorT e) x y) (Cod (ErrorT e) x y) c) => ArrowFix x y (ErrorT e c)
deriving instance (Identifiable (Cod (ErrorT e) x y), ArrowChoice c, ArrowDeduplicate (Dom (ErrorT e) x y) (Cod (ErrorT e) x y) c) => ArrowDeduplicate x y (ErrorT e c)

instance (Complete e, ArrowJoin c, ArrowChoice c) => ArrowJoin (ErrorT e c) where
  joinWith lub' f g = lift $ joinWith (toJoin2 widening (⊔) lub') (unlift f) (unlift g)

