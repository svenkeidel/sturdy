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
module Control.Arrow.Transformer.Abstract.Fix.Chaotic(ChaoticT,runChaoticT) where

import           Prelude hiding (id,pred,lookup,map,head,iterate,(.),elem)

import           Control.Category
import           Control.Arrow hiding (loop)
import           Control.Arrow.Fix.Chaotic
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Fix.Context as Context
import           Control.Arrow.State
import           Control.Arrow.Trans
import           Control.Arrow.Order

import           Control.Arrow.Transformer.Writer

import           Data.Profunctor
import           Data.Identifiable
import           Data.Coerce

newtype ChaoticT a c x y = ChaoticT (WriterT (Component a) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowStack a,ArrowCache a b,ArrowState s,ArrowContext ctx, ArrowJoinContext u)

instance (Identifiable a, Arrow c, Profunctor c) => ArrowChaotic a (ChaoticT a c) where
  setComponent = lift id
  getComponent f = lift $ proc a -> do
    (component,b) <- unlift f -< a
    returnA -< (mempty,(component,b))
  {-# INLINE setComponent #-}
  {-# INLINE getComponent #-}

runChaoticT :: Profunctor c => ChaoticT a c x y -> c x y
runChaoticT (ChaoticT f) = rmap snd (runWriterT f)
{-# INLINE runChaoticT #-}

instance (Identifiable a, ArrowRun c) => ArrowRun (ChaoticT a c) where
  type Run (ChaoticT a c) x y = Run c x y
  run f = run (runChaoticT f)
  {-# INLINE run #-}

instance ArrowTrans (ChaoticT a c) where
  type Underlying (ChaoticT a c) x y = c x (Component a,y)

instance (Identifiable a, Profunctor c,ArrowApply c) => ArrowApply (ChaoticT a c) where
  app = ChaoticT (lmap (first coerce) app)
  {-# INLINE app #-}

deriving instance (Identifiable a, ArrowJoin c) => ArrowJoin (ChaoticT a c)
deriving instance (Identifiable a, ArrowComplete (Component a,y) c) => ArrowComplete y (ChaoticT a c)
instance (Identifiable a, ArrowEffectCommutative c) => ArrowEffectCommutative (ChaoticT a c)
