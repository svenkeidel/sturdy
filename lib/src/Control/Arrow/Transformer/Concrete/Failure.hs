{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Data.Concrete.Error
import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce

-- | Arrow transformer that adds failure to the result of a computation
newtype FailureT e c x y = FailureT (KleisliT (Error e) c x y) 
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowLift,ArrowRun,
            ArrowState s,ArrowReader r,ArrowExcept exc,ArrowEnv a b env,ArrowStore var val,ArrowConst r)

runFailureT :: FailureT e c x y -> c x (Error e y)
runFailureT = coerce
{-# INLINE runFailureT #-}

instance (ArrowChoice c, Profunctor c) => ArrowFail e (FailureT e c) where
  fail = lift (arr Fail)

instance (ArrowChoice c, ArrowApply c, Profunctor c) => ArrowApply (FailureT e c) where
  app = lift (app .# first coerce)

type instance Fix x y (FailureT e c) = FailureT e (Fix (Dom (FailureT e) x y) (Cod (FailureT e) x y) c)
deriving instance (ArrowChoice c, ArrowFix (Dom (FailureT e) x y) (Cod (FailureT e) x y) c) => ArrowFix x y (FailureT e c)
