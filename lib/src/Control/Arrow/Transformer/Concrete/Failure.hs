{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Concrete.Failure(FailureT,runFailureT) where

import Prelude hiding (id,(.),lookup,read,fail)

import Control.Category
import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Environment as Env
import Control.Arrow.Closure as Cls
import Control.Arrow.Except as Exc
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store as Store
import Control.Arrow.Trans
import Control.Arrow.LetRec

import Control.Arrow.Transformer.Kleisli

import Data.Concrete.Error
import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce

-- | Arrow transformer that adds failure to the result of a computation
newtype FailureT e c x y = FailureT (KleisliT (Error e) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowLift,ArrowTrans,ArrowRun,
            ArrowConst r,ArrowState s,ArrowReader r,ArrowExcept exc,
            ArrowEnv var addr, ArrowLetRec var val, ArrowClosure expr cls,ArrowStore addr val)

runFailureT :: FailureT e c x y -> c x (Error e y)
runFailureT = coerce
{-# INLINE runFailureT #-}

instance (ArrowChoice c, Profunctor c) => ArrowFail e (FailureT e c) where
  type Join x (FailureT e c) = ()
  fail = lift (arr Fail)

instance (ArrowChoice c, ArrowApply c, Profunctor c) => ArrowApply (FailureT e c) where
  app = lift (app .# first coerce)

instance (ArrowChoice c, ArrowFix (Underlying (FailureT e c) x y)) => ArrowFix (FailureT e c x y) where
  type Fix (FailureT e c x y) = Fix (Underlying (FailureT e c) x y)
