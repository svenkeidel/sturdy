{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Control.Arrow.Transformer.Abstract.LogError(LogErrorT,runLogErrorT) where

import Prelude hiding (id,(.),lookup,read)

import Control.Category
import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Environment as Env
import Control.Arrow.Closure as Cls
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Trans
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Writer
import Control.Arrow.Store as Store
import Control.Arrow.Except as Exc
import Control.Arrow.Transformer.Writer
import Control.Arrow.Fix.Context

import Data.Profunctor
import Data.Profunctor.Unsafe((.#))
import Data.Coerce
import Data.Order

-- | Describes computations that can fail.
newtype LogErrorT e c x y = LogErrorT (WriterT e c x y)
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, ArrowRun,
            ArrowConst r, ArrowState s, ArrowReader r,
            ArrowEnv var val, ArrowClosure expr cls, ArrowStore a b,
            ArrowExcept e', ArrowContext ctx)

runLogErrorT :: LogErrorT e c x y -> c x (e,y)
runLogErrorT = coerce
{-# INLINE runLogErrorT #-}

instance (Monoid e, Arrow c, Profunctor c) => ArrowFail e (LogErrorT e c) where
  type Join x (LogErrorT e c) = LowerBounded x
  fail = LogErrorT $ proc e -> do
    tell -< e
    returnA -< bottom

instance (Monoid e, ArrowApply c, Profunctor c) => ArrowApply (LogErrorT e c) where
  app = lift (app .# first coerce)

type instance Fix (LogErrorT e c) x y = LogErrorT e (Fix c x y)
instance (ArrowChoice c, ArrowFix (Underlying (LogErrorT e c) x y)) => ArrowFix (LogErrorT e c x y)
