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
module Control.Arrow.Transformer.Abstract.ReachingDefinitions(
  ReachingDefinitions,
  reachingDefs,
  runReachingDefs,
  -- fromList,
  ReachingDefs
) where

import           Prelude hiding ((.),read)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Lift
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Fail
import           Control.Arrow.Store
import           Control.Arrow.Environment
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Arrow.Transformer.State

import           Data.Identifiable
import           Data.Order
import           Data.Abstract.DiscretePowerset(Pow)
import qualified Data.Abstract.DiscretePowerset as P
import           Data.Abstract.Store (Store)
import qualified Data.Abstract.Store as S

type ReachingDefs var lab = Store var (Pow lab)

newtype ReachingDefinitions var lab c x y = ReachingDefinitions (StoreArrow var (Pow lab) c x y)

reachingDefs ::Arrow c => c (ReachingDefs var lab,x) (ReachingDefs var lab,y) -> ReachingDefinitions var lab c x y
reachingDefs = ReachingDefinitions . StoreArrow . State

runReachingDefs :: Arrow c => ReachingDefinitions var lab c x y -> c (ReachingDefs var lab,x) (ReachingDefs var lab,y)
runReachingDefs (ReachingDefinitions f) = runStore f

instance (Identifiable var, Identifiable lab, ArrowRead (var,lab) val (Store var (Pow lab),x) (Store var (Pow lab),y) c)
  => ArrowRead (var,lab) val x y (ReachingDefinitions var lab c) where
  read (ReachingDefinitions (StoreArrow f)) (ReachingDefinitions (StoreArrow g)) =
     ReachingDefinitions $ StoreArrow $ proc ((var,l),x) -> read f g -< ((var,l),x)

instance (Identifiable var, Identifiable lab, ArrowWrite (var,lab) val c)
  => ArrowWrite (var,lab) val (ReachingDefinitions var lab c) where
  write = ReachingDefinitions $ StoreArrow $ State $ proc (st,((var,l),val)) -> do
    write -< ((var,l),val)
    returnA -< (S.insert var (P.singleton l) st,())

type instance Fix x y (ReachingDefinitions var lab c) = ReachingDefinitions var lab (Fix (ReachingDefs var lab, x) (ReachingDefs var lab, y) c)
deriving instance (Arrow c, ArrowFix x y (StoreArrow var (Pow lab) c)) => ArrowFix x y (ReachingDefinitions var lab c)

instance ArrowApply c => ArrowApply (ReachingDefinitions v l c) where
  app = ReachingDefinitions ((\(ReachingDefinitions f,x) -> (f,x)) ^>> app)

deriving instance ArrowLift (ReachingDefinitions v l)
deriving instance Arrow c => Category (ReachingDefinitions v l c)
deriving instance Arrow c => Arrow (ReachingDefinitions v l c)
deriving instance ArrowChoice c => ArrowChoice (ReachingDefinitions v l c)
deriving instance ArrowReader r c => ArrowReader r (ReachingDefinitions v l c)
deriving instance ArrowFail e c => ArrowFail e (ReachingDefinitions v l c)
deriving instance ArrowState s c => ArrowState s (ReachingDefinitions v l c)
deriving instance ArrowEnv x y env c => ArrowEnv x y env (ReachingDefinitions v l c)

deriving instance PreOrd (c (ReachingDefs var lab,x) (ReachingDefs var lab,y)) => PreOrd (ReachingDefinitions var lab c x y)
deriving instance LowerBounded (c (ReachingDefs var lab,x) (ReachingDefs var lab,y)) => LowerBounded (ReachingDefinitions var lab c x y)
deriving instance Complete (c (ReachingDefs var lab,x) (ReachingDefs var lab,y)) => Complete (ReachingDefinitions var lab c x y)
deriving instance CoComplete (c (ReachingDefs var lab,x) (ReachingDefs var lab,y)) => CoComplete (ReachingDefinitions var lab c x y)
deriving instance UpperBounded (c (ReachingDefs var lab,x) (ReachingDefs var lab,y)) => UpperBounded (ReachingDefinitions var lab c x y)
