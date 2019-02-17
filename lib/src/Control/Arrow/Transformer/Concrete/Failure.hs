{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.Failure(FailureT,runFailureT) where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Deduplicate
import Control.Arrow.Environment as Env
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.Store as Store
import Control.Arrow.State
import Control.Arrow.Except as Exc
import Control.Arrow.Transformer.Kleisli
import Control.Category

import Data.Profunctor
import Data.Concrete.Error
import Data.Identifiable

-- | Arrow transformer that adds failure to the result of a computation
newtype FailureT e c x y = FailureT { unFailureT :: KleisliT (Error e) c x y }

runFailureT :: FailureT e c x y -> c x (Error e y)
runFailureT = runKleisliT . unFailureT

instance (ArrowChoice c, Profunctor c) => ArrowFail e (FailureT e c) where
  fail = lift $ arr Fail

deriving instance (ArrowChoice c, Profunctor c) => Profunctor (FailureT e c)
deriving instance (ArrowChoice c, Profunctor c) => Category (FailureT e c)
deriving instance (ArrowChoice c, Profunctor c) => Arrow (FailureT e c)
deriving instance (ArrowChoice c, Profunctor c) => ArrowChoice (FailureT e c)
instance (ArrowChoice c, ArrowApply c, Profunctor c) => ArrowApply (FailureT e c) where app = lift $ lmap (first unlift) app
deriving instance ArrowTrans (FailureT e)
deriving instance ArrowLift (FailureT e)
deriving instance (ArrowChoice c, ArrowState s c) => ArrowState s (FailureT e c)
deriving instance (ArrowChoice c, ArrowReader r c) => ArrowReader r (FailureT e c)
deriving instance (ArrowChoice c, ArrowEnv x y env c) => ArrowEnv x y env (FailureT e c)
deriving instance (ArrowChoice c, ArrowStore var val c) => ArrowStore var val (FailureT e c)
deriving instance (ArrowChoice c, ArrowExcept e c) => ArrowExcept e (FailureT e' c)
type instance Fix x y (FailureT e c) = FailureT e (Fix (Dom (FailureT e) x y) (Cod (FailureT e) x y) c)
deriving instance (ArrowChoice c, ArrowFix (Dom (FailureT e) x y) (Cod (FailureT e) x y) c) => ArrowFix x y (FailureT e c)
deriving instance (Identifiable (Error e y), ArrowChoice c, ArrowDeduplicate x (Error e y) c) => ArrowDeduplicate x y (FailureT e c)
deriving instance (ArrowChoice c, ArrowConst r c) => ArrowConst r (FailureT e c)
