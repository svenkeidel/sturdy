{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Control.Arrow.Transformer.Abstract.LiveVariables where

import           Prelude hiding (id,(.),read)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Alloc
import           Control.Arrow.Conditional
import           Control.Arrow.Environment
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Lift
import           Control.Arrow.Random
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Store

import           Control.Arrow.Transformer.State

import           Data.Identifiable
import           Data.Hashable
import           Data.Order
import           Data.Semigroup

import           Data.Abstract.Widening
import           Data.Abstract.DiscretePowerset(Pow)
import qualified Data.Abstract.DiscretePowerset as P

-- | Transition function for live variables
newtype LiveVars v = LiveVars (Pow v -> Pow v)

vars :: LiveVars v -> Pow v
vars (LiveVars f) = f P.empty

instance Hashable v => Hashable (LiveVars v) where
  hashWithSalt salt lv = hashWithSalt salt (vars lv)

instance Eq v => Eq (LiveVars v) where
  lv1 == lv2 = vars lv1 == vars lv2

instance Identifiable v => PreOrd (LiveVars v) where
  lv1 ⊑ lv2 = vars lv1 ⊑ vars lv2

instance Identifiable v => Complete (LiveVars v) where
  LiveVars f ⊔ LiveVars g = LiveVars (\lv -> f lv ⊔ g lv)

instance Identifiable v => Widening (LiveVars v)

live :: Identifiable v => v -> LiveVars v
live x = LiveVars $ P.insert x

dead :: Identifiable v => v -> LiveVars v
dead x = LiveVars $ P.delete x

-- | Composing two transition functions
instance Semigroup (LiveVars v) where
  (<>) = mappend

instance Monoid (LiveVars v) where
  mempty = LiveVars id
  mappend (LiveVars f) (LiveVars g) = LiveVars (f . g)

-- | An arrow transformer that tracks the live variables.
newtype LiveVariables v c x y = LiveVariables (State (LiveVars v) c x y)

runLiveVariables :: LiveVariables v c x y -> c (LiveVars v,x) (LiveVars v,y)
runLiveVariables (LiveVariables f) = runState f

instance (Identifiable var, ArrowRead var val (LiveVars var,x) (LiveVars var,y) c)
  => ArrowRead var val x y (LiveVariables var c) where
  read (LiveVariables f) (LiveVariables g) = LiveVariables $ proc (var,x) -> do
    lvs <- get -< ()
    put -< lvs <> live var
    read f g -< (var,x)

instance (Identifiable var, ArrowWrite var val c) => ArrowWrite var val (LiveVariables var c) where
  write = LiveVariables $ proc (var,val) -> do
    lvs <- get -< ()
    put -< lvs <> dead var
    write -< (var,val)

type instance Fix x y (LiveVariables v c) = LiveVariables v (Fix (LiveVars v,x) (LiveVars v,y) c)
deriving instance (ArrowFix (LiveVars v,x) (LiveVars v,y) c) => ArrowFix x y (LiveVariables v c)

deriving instance ArrowLift (LiveVariables v)
instance (ArrowApply c) => ArrowApply (LiveVariables v c) where
  app = LiveVariables ((\(LiveVariables f,x) -> (f,x)) ^>> app)

deriving instance (Arrow c) => Category (LiveVariables v c)
deriving instance (Arrow c) => Arrow (LiveVariables v c)
deriving instance (ArrowChoice c) => ArrowChoice (LiveVariables v c)
deriving instance (ArrowReader r c) => ArrowReader r (LiveVariables v c)
deriving instance (ArrowFail e c) => ArrowFail e (LiveVariables v c)
-- deriving instance (ArrowState s c) => ArrowState s (LiveVariables v c)
deriving instance ArrowAlloc x y c => ArrowAlloc x y (LiveVariables v c)
deriving instance ArrowRand r c => ArrowRand r (LiveVariables v c)
deriving instance ArrowCond val (LiveVars v,x) (LiveVars v,y) (LiveVars v,z) c => ArrowCond val x y z (LiveVariables v c)
deriving instance ArrowEnv x y env c => ArrowEnv x y env (LiveVariables v c)

deriving instance PreOrd (c (LiveVars v,x) (LiveVars v,y)) => PreOrd (LiveVariables v c x y)
deriving instance LowerBounded (c (LiveVars v,x) (LiveVars v,y)) => LowerBounded (LiveVariables v c x y)
deriving instance Complete (c (LiveVars v,x) (LiveVars v,y)) => Complete (LiveVariables v c x y)
deriving instance CoComplete (c (LiveVars v,x) (LiveVars v,y)) => CoComplete (LiveVariables v c x y)
deriving instance UpperBounded (c (LiveVars v,x) (LiveVars v,y)) => UpperBounded (LiveVariables v c x y)
