{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Abstract.Terminating(TerminatingT,runTerminatingT) where

import Prelude hiding (id,(.),lookup,fail)

import Control.Arrow
import Control.Arrow.Const
import Control.Arrow.Deduplicate
import Control.Arrow.Fix
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Trans
import Control.Arrow.Abstract.Join
import Control.Arrow.Transformer.Kleisli
import Control.Category

import Data.Identifiable
import Data.Profunctor
import Data.Abstract.Terminating
import Data.Abstract.Widening (toJoin)

-- | Arrow that propagates non-terminating computations.
newtype TerminatingT c x y = TerminatingT { unTerminatingT :: KleisliT Terminating c x y }
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowTrans, ArrowLift, ArrowState s, ArrowReader r,
            ArrowConst r)

runTerminatingT :: TerminatingT c x y -> c x (Terminating y)
runTerminatingT = runKleisliT . unTerminatingT

instance (ArrowChoice c, Profunctor c, ArrowApply c) => ArrowApply (TerminatingT c) where
  app = lift $ lmap (first unlift) app

type instance Fix x y (TerminatingT c) = TerminatingT (Fix (Dom TerminatingT x y) (Cod TerminatingT x y) c)
deriving instance (ArrowChoice c, ArrowFix (Dom TerminatingT x y) (Cod TerminatingT x y) c) => ArrowFix x y (TerminatingT c)
deriving instance (Identifiable (Cod TerminatingT x y), ArrowChoice c, ArrowDeduplicate (Dom TerminatingT x y) (Cod TerminatingT x y) c) => ArrowDeduplicate x y (TerminatingT c)

instance (ArrowChoice c, ArrowJoin c) => ArrowJoin (TerminatingT c) where
  joinWith lub' f g = lift $ joinWith (toJoin widening lub') (unlift f) (unlift g)
