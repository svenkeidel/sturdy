{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Completion(CompletionT,runCompletionT) where

import Prelude hiding ((.),id,lookup,fail)

import Control.Arrow
import Control.Arrow.Deduplicate
import Control.Arrow.Environment
import Control.Arrow.Except
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Store
import Control.Arrow.Trans
import Control.Arrow.Const
import Control.Arrow.Abstract.Join
import Control.Arrow.Transformer.Kleisli
import Control.Category

import Data.Profunctor
import Data.Abstract.FreeCompletion
import Data.Identifiable

-- | Allows to describe computations over non-completely ordered types.
-- E.g. allows to join a computation of type 'c x [y]'.
newtype CompletionT c x y = CompletionT { unCompletionT :: KleisliT FreeCompletion c x y }
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, ArrowState s, ArrowReader r,
            ArrowConst r, ArrowEnv a b e, ArrowStore a b, ArrowFail e, ArrowExcept e)

runCompletionT :: CompletionT c x y -> c x (FreeCompletion y)
runCompletionT = runKleisliT . unCompletionT

instance (ArrowChoice c, ArrowApply c, Profunctor c) => ArrowApply (CompletionT c) where app = lift $ lmap (first unlift) app
type instance Fix x y (CompletionT c) = CompletionT (Fix (Dom (CompletionT) x y) (Cod (CompletionT) x y) c)
deriving instance (ArrowChoice c, ArrowFix (Dom (CompletionT) x y) (Cod (CompletionT) x y) c) => ArrowFix x y (CompletionT c)
deriving instance (Identifiable (Cod CompletionT x y), ArrowChoice c, ArrowDeduplicate (Dom CompletionT x y) (Cod CompletionT x y) c) => ArrowDeduplicate x y (CompletionT c)

instance (ArrowChoice c, ArrowJoin c) => ArrowJoin (CompletionT c) where
  joinWith lub f g = lift $ joinWith join (unlift f) (unlift g)
    where join (Lower x) (Lower y) = Lower (lub x y)
          join Top _ = Top
          join _ Top = Top
