{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Control.Arrow.Transformer.Abstract.Fix.Component(ComponentT,runComponentT) where

import           Prelude hiding (id,pred,lookup,map,head,iterate,(.),elem)

import           Control.Category
import           Control.Arrow hiding (loop)
import           Control.Arrow.Fix.Chaotic
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Fix.ControlFlow
import           Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Fix.Context as Context
import           Control.Arrow.State
import           Control.Arrow.Trans
import           Control.Arrow.Order

import           Control.Arrow.Transformer.Writer

import           Data.Profunctor
import           Data.Identifiable
import           Data.Coerce

newtype ComponentT a c x y = ComponentT (WriterT (Component a) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowStack a,ArrowStackDepth,ArrowStackElements a,ArrowCache a b,
            ArrowState s,ArrowContext ctx, ArrowJoinContext u, ArrowControlFlow stmt)

instance (Identifiable a, Arrow c, Profunctor c) => ArrowComponent (Component a) (ComponentT a c) where
  setComponent f = lift (rmap snd (unlift f))
  getComponent f = lift $ proc a -> do
    (component,b) <- unlift f -< a
    returnA -< (mempty,(component,b))
  {-# INLINE setComponent #-}
  {-# INLINE getComponent #-}

runComponentT :: Profunctor c => ComponentT a c x y -> c x y
runComponentT (ComponentT f) = rmap snd (runWriterT f)
{-# INLINE runComponentT #-}

instance (Identifiable a, ArrowRun c) => ArrowRun (ComponentT a c) where
  type Run (ComponentT a c) x y = Run c x y
  run f = run (runComponentT f)
  {-# INLINE run #-}

instance ArrowTrans (ComponentT a c) where
  type Underlying (ComponentT a c) x y = c x (Component a,y)

instance (Identifiable a, Profunctor c,ArrowApply c) => ArrowApply (ComponentT a c) where
  app = ComponentT (lmap (first coerce) app)
  {-# INLINE app #-}

deriving instance (Identifiable a, ArrowJoin c) => ArrowJoin (ComponentT a c)
deriving instance (Identifiable a, ArrowComplete (Component a,y) c) => ArrowComplete y (ComponentT a c)
instance (Identifiable a, ArrowEffectCommutative c) => ArrowEffectCommutative (ComponentT a c)
