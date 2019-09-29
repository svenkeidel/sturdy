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
{-# LANGUAGE DerivingStrategies #-}
module Control.Arrow.Transformer.ReachingDefinitions(
  ReachingDefsT(..),
  runReachingDefsT,
) where

import           Prelude hiding ((.),read,id)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Except
import           Control.Arrow.Fix
import           Control.Arrow.Trans
import           Control.Arrow.Reader as Reader
import           Control.Arrow.State
import           Control.Arrow.Fail
import           Control.Arrow.Store as Store
import           Control.Arrow.Environment
import           Control.Arrow.Closure
import           Control.Arrow.Order
import           Control.Arrow.Transformer.Reader

import           Data.Label
import           Data.Profunctor
import           Data.Profunctor.Unsafe((.#))
import           Data.Coerce
import           Data.Utils

newtype ReachingDefsT (f :: * -> *) c x y = ReachingDefsT (ReaderT (Maybe Label) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowLift,
            ArrowState s, ArrowEnv var val, ArrowClosure expr cls,
            ArrowFail e,ArrowExcept e, ArrowLowerBounded, ArrowComplete z)

reachingDefsT :: c (Maybe Label,x) y -> ReachingDefsT f c x y
reachingDefsT = lift
{-# INLINE reachingDefsT #-}

runReachingDefsT :: Profunctor c => ReachingDefsT f c x y -> c x y
runReachingDefsT f = lmap (\x -> (Nothing,x)) (unlift f)
{-# INLINE runReachingDefsT #-}

runReachingDefsT' :: ReachingDefsT f c x y -> c (Maybe Label,x) y
runReachingDefsT' = unlift
{-# INLINE runReachingDefsT' #-}

instance ArrowRun c => ArrowRun (ReachingDefsT f c) where
  type Run (ReachingDefsT f c) x y = Run c x y
  run = run . runReachingDefsT

instance (ArrowStore var (val,f Label) c, IsEmpty (f Label), IsSingleton (f Label), Elem (f Label) ~ Label) =>
    ArrowStore var val (ReachingDefsT f c) where
  type Join y (ReachingDefsT f c) = Store.Join y c
  read (ReachingDefsT f) (ReachingDefsT g) = ReachingDefsT $ read (lmap (\((v,_),x) -> (v,x)) f) g
  write = reachingDefsT $ lmap (\(lab,(var,val)) -> (var,(val,fromMaybe lab))) write

type instance Fix (ReachingDefsT f c) x y = ReachingDefsT f (Fix c x y)
instance (HasLabel x, Arrow c, ArrowFix (c x y), Profunctor c) => ArrowFix (ReachingDefsT f c x y) where
  fix f = ReachingDefsT $ ReaderT $ proc (_,x) -> fix (unwrap . f . lift') -< x
    where
      unwrap :: ReachingDefsT f c x y -> c x y
      unwrap g = lmap (\x -> (Just (label x),x)) (runReachingDefsT' g)

instance (ArrowApply c,Profunctor c) => ArrowApply (ReachingDefsT f c) where
  app = ReachingDefsT (app .# first coerce)

instance ArrowReader r c => ArrowReader r (ReachingDefsT f c) where
  ask = lift' Reader.ask
  local f = lift $ lmap (\(m,(r,a)) -> (r,(m,a))) (Reader.local (unlift f))
