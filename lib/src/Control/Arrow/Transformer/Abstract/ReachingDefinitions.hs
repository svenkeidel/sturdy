{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Arrow.Transformer.Abstract.ReachingDefinitions(
  ReachingDefinitions(..),
  reachingDefs,
  runReachingDefs,
) where

import           Prelude hiding ((.),read)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Alloc
import           Control.Arrow.Conditional
import           Control.Arrow.Except
import           Control.Arrow.Fix
import           Control.Arrow.Lift
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Fail
import           Control.Arrow.Random
import           Control.Arrow.Store
import           Control.Arrow.Environment

import           Data.Identifiable
import           Data.Order
import           Data.Abstract.DiscretePowerset(Pow)
import qualified Data.Abstract.DiscretePowerset as P

newtype ReachingDefinitions c x y = ReachingDefinitions (c x y)

reachingDefs ::Arrow c => c x y -> ReachingDefinitions c x y
reachingDefs = ReachingDefinitions

runReachingDefs :: Arrow c => ReachingDefinitions c x y -> c x y
runReachingDefs (ReachingDefinitions f) = f

instance (Identifiable var, Identifiable lab, ArrowRead var (val,Pow lab) x y c)
 => ArrowRead (var,lab) val x y (ReachingDefinitions c) where
 read (ReachingDefinitions f) (ReachingDefinitions g) = ReachingDefinitions $ proc ((var,_),x) -> do
   read ((\((v,_::Pow lab),x) -> (v,x)) ^>> f) g -< (var,x)

instance (Identifiable var, Identifiable lab, ArrowWrite (var,lab) (val,Pow lab) c)
  => ArrowWrite (var,lab) val (ReachingDefinitions c) where
  write = ReachingDefinitions $ proc ((var,lab),val) ->
    write -< ((var,lab),(val,P.singleton lab))

type instance Fix x y (ReachingDefinitions c) = ReachingDefinitions (Fix x y c)
deriving instance (Arrow c, ArrowFix x y c) => ArrowFix x y (ReachingDefinitions c)

instance ArrowApply c => ArrowApply (ReachingDefinitions c) where
  app = ReachingDefinitions ((\(ReachingDefinitions f,x) -> (f,x)) ^>> app)

instance ArrowLift ReachingDefinitions where
  lift f = ReachingDefinitions f

deriving instance Category c => Category (ReachingDefinitions c)
deriving instance Arrow c => Arrow (ReachingDefinitions c)
deriving instance ArrowChoice c => ArrowChoice (ReachingDefinitions c)
deriving instance ArrowReader r c => ArrowReader r (ReachingDefinitions c)
deriving instance ArrowFail e c => ArrowFail e (ReachingDefinitions c)
deriving instance ArrowExcept x y e c => ArrowExcept x y e (ReachingDefinitions c)
deriving instance ArrowState s c => ArrowState s (ReachingDefinitions c)
deriving instance ArrowEnv x y env c => ArrowEnv x y env (ReachingDefinitions c)
deriving instance ArrowAlloc x y c => ArrowAlloc x y (ReachingDefinitions c)
deriving instance ArrowCond val x y z c => ArrowCond val x y z (ReachingDefinitions c)
deriving instance ArrowRand v c => ArrowRand v (ReachingDefinitions c)

deriving instance PreOrd (c x y) => PreOrd (ReachingDefinitions c x y)
deriving instance LowerBounded (c x y) => LowerBounded (ReachingDefinitions c x y)
deriving instance Complete (c x y) => Complete (ReachingDefinitions c x y)
deriving instance CoComplete (c x y) => CoComplete (ReachingDefinitions c x y)
deriving instance UpperBounded (c x y) => UpperBounded (ReachingDefinitions c x y)
