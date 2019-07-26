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
import           Control.Arrow.Order
import           Control.Arrow.Transformer.Reader

import           Data.Label
import           Data.Profunctor
import           Data.Profunctor.Unsafe((.#))
import           Data.Coerce
import           Data.Utils

newtype ReachingDefsT (f :: * -> *) lab c x y = ReachingDefsT (ReaderT (Maybe lab) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowLift,
            ArrowState s, ArrowEnv var val, ArrowClosure var val env,
            ArrowFail e,ArrowExcept e, ArrowLowerBounded, ArrowComplete z)

reachingDefsT :: (Arrow c,Profunctor c) => c (Maybe lab,x) y -> ReachingDefsT f lab c x y
reachingDefsT = lift
{-# INLINE reachingDefsT #-}

runReachingDefsT :: (Arrow c,Profunctor c) => ReachingDefsT f lab c x y -> c x y
runReachingDefsT f = lmap (\x -> (Nothing,x)) (unlift f)
{-# INLINE runReachingDefsT #-}

runReachingDefsT' :: (Arrow c,Profunctor c) => ReachingDefsT f lab c x y -> c (Maybe lab,x) y
runReachingDefsT' = unlift
{-# INLINE runReachingDefsT' #-}

instance ArrowRun c => ArrowRun (ReachingDefsT f lab c) where
  type Rep (ReachingDefsT f lab c) x y = Rep c x y
  run = run . runReachingDefsT

instance (ArrowStore var (val,f lab) c, IsEmpty (f lab), IsSingleton (f lab), Elem (f lab) ~ lab) =>
    ArrowStore var val (ReachingDefsT f lab c) where
  type Join y (ReachingDefsT f lab c) = Store.Join y c
  read (ReachingDefsT f) (ReachingDefsT g) = ReachingDefsT $ read (lmap (\((v,_),x) -> (v,x)) f) g
  write = reachingDefsT $ lmap (\(lab,(var,val)) -> (var,(val,fromMaybe lab))) write

type instance Fix x y (ReachingDefsT f lab c) = ReachingDefsT f lab (Fix x y c)
instance (HasLabel x lab, Arrow c, ArrowFix x y c) => ArrowFix x y (ReachingDefsT f lab c) where
  fix f = ReachingDefsT $ ReaderT $ proc (_,x) -> fix (unwrap . f . lift') -< x
    where
      unwrap :: HasLabel x lab => ReachingDefsT f lab c x y -> c x y
      unwrap g = lmap (\x -> (Just (label x),x)) (runReachingDefsT' g)

instance (ArrowApply c,Profunctor c) => ArrowApply (ReachingDefsT f lab c) where
  app = ReachingDefsT (app .# first coerce)

instance ArrowReader r c => ArrowReader r (ReachingDefsT f lab c) where
  ask = lift' Reader.ask
  local f = lift $ lmap (\(m,(r,a)) -> (r,(m,a))) (Reader.local (unlift f))
