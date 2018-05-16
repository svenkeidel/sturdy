{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
module Control.Arrow.Transformer.Effect(Effect,pattern Effect,runEffect,runEffect',modifyEntrySet) where

import Prelude hiding ((.))
import Data.Order

import Control.Category
import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Lift
import Control.Arrow.Reader
import Control.Arrow.State
import Control.Arrow.Fail
import Control.Arrow.Writer
import Control.Arrow.Store
import Control.Arrow.Transformer.State
import Control.Arrow.Transformer.Writer


-- | Arrow to implement an effect analysis, e.g. reaching definitions, live variables, etc.
-- The state arrow calculates the entry sets and the writer arrow the exit sets.
newtype Effect p c x y = Effect_ (Writer p (State p c) x y)

{-# COMPLETE Effect #-}
pattern Effect :: c (p,x) (p,(p,y)) -> Effect p c x y
pattern Effect f = Effect_ (Writer (State f))

runEffect :: Arrow c => Effect p c x y -> c (p,x) y
runEffect (Effect f) = f >>^ (snd.snd)

runEffect' :: Effect p c x y -> c (p,x) (p,(p,y))
runEffect' (Effect f) = f

modifyEntrySet :: (Arrow c,Monoid p) => ((p,x) -> p) -> Effect p c x ()
modifyEntrySet f = Effect $ arr $ \(en,x) -> (f (en,x),(mempty,()))

type instance Fix x y (Effect p c) = Fix (p,x) (p,(p,y)) c
instance (Monoid p,ArrowFix (p,x) (p,(p,y)) c) => ArrowFix x y (Effect p c) where
  fixA f = Effect $ fixA $ \g ->
    runEffect' $ f $ Effect $ proc (p,x) -> do
      (_,(entry,y)) <- g -< (p,x)
      -- The entry set of this basic block becomes the exit set of the previous basic block
      returnA -< (entry, (entry,y))

instance Monoid p => ArrowLift (Effect p) where
  lift f = Effect_ (lift (lift f))

instance (Monoid p,ArrowApply c) => ArrowApply (Effect p c) where
  app = Effect_ ((\(Effect_ f,x) -> (f,x)) ^>> app)

instance (Monoid p, ArrowState s c) => ArrowState s (Effect p c) where
  getA = lift getA
  putA = lift putA

instance (Monoid p, ArrowWriter w c) => ArrowWriter w (Effect p c) where
  tellA = lift tellA

deriving instance (Monoid p,Arrow c) => Category (Effect p c)
deriving instance (Monoid p,Arrow c) => Arrow (Effect p c)
deriving instance (Monoid p,ArrowChoice c) => ArrowChoice (Effect p c)
deriving instance (Monoid p,ArrowReader r c) => ArrowReader r (Effect p c)
deriving instance (Monoid p,ArrowFail e c) => ArrowFail e (Effect p c)
deriving instance (Monoid p,ArrowStore var val lab c) => ArrowStore var val lab (Effect p c)

deriving instance PreOrd (c (p,x) (p,(p,y))) => PreOrd (Effect p c x y)
deriving instance LowerBounded (c (p,x) (p,(p,y))) => LowerBounded (Effect p c x y)
deriving instance Complete (c (p,x) (p,(p,y))) => Complete (Effect p c x y)
deriving instance CoComplete (c (p,x) (p,(p,y))) => CoComplete (Effect p c x y)
deriving instance UpperBounded (c (p,x) (p,(p,y))) => UpperBounded (Effect p c x y)
