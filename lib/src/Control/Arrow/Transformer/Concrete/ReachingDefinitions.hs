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
-- | Arrow transformer for a dynamic reaching definition analysis.
module Control.Arrow.Transformer.Concrete.ReachingDefinitions(
  ReachingDefsT(..),
  reachingDefsT,
  runReachingDefsT,
  runReachingDefsT',
) where

import           Prelude hiding ((.),read,id)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Alloc
import           Control.Arrow.Except
import           Control.Arrow.Fix
import           Control.Arrow.Trans
import           Control.Arrow.Reader as Reader
import           Control.Arrow.State
import           Control.Arrow.Fail
import           Control.Arrow.Store as Store
import           Control.Arrow.Environment
import           Control.Arrow.Transformer.Reader

import           Data.Identifiable
import           Data.Label
import           Data.Profunctor
import           Data.Profunctor.Unsafe((.#))
import           Data.Coerce

newtype ReachingDefsT lab c x y = ReachingDefsT (ReaderT (Maybe lab) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowLift,
            ArrowFail e,ArrowExcept e,ArrowState s,ArrowEnv var val,ArrowClosure var val env)

reachingDefsT :: (Arrow c,Profunctor c) => c (Maybe lab,x) y -> ReachingDefsT lab c x y
reachingDefsT = lift

runReachingDefsT :: (Arrow c,Profunctor c) => ReachingDefsT lab c x y -> c (Maybe lab,x) y
runReachingDefsT = unlift

runReachingDefsT' :: (Arrow c,Profunctor c) => ReachingDefsT lab c x y -> c x y
runReachingDefsT' f = lmap (\x -> (Nothing,x)) (runReachingDefsT f)

instance ArrowRun c => ArrowRun (ReachingDefsT lab c) where
  type Rep (ReachingDefsT lab c) x y = Rep c x y
  run = run . runReachingDefsT'
  {-# INLINE run #-}

instance (Identifiable var, Identifiable lab, ArrowStore var (val,Maybe lab) c) => ArrowStore var val (ReachingDefsT lab c) where
  type Join (ReachingDefsT lab c) ((val,x),x) y = Store.Join c (((val,Maybe lab),Dom (ReachingDefsT lab) x y), Dom (ReachingDefsT lab) x y) (Cod (ReachingDefsT lab) x y)
  read (ReachingDefsT f) (ReachingDefsT g) = ReachingDefsT $ read (lmap (\((v,_),x) -> (v,x)) f) g
  write = reachingDefsT $ lmap (\(lab,(var,val)) -> (var,(val,lab))) write

type instance Fix x y (ReachingDefsT lab c) = ReachingDefsT lab (Fix x y c)
instance (HasLabel x lab, Arrow c, ArrowFix x y c) => ArrowFix x y (ReachingDefsT lab c) where
  fix f = ReachingDefsT $ ReaderT $ proc (_,x) -> fix (unwrap . f . lift') -< x
    where
      unwrap :: HasLabel x lab => ReachingDefsT lab c x y -> c x y
      unwrap f' = (Just . label &&& id) ^>> runReachingDefsT f'

instance (ArrowApply c,Profunctor c) => ArrowApply (ReachingDefsT lab c) where
  app = ReachingDefsT (app .# first coerce)

instance ArrowReader r c => ArrowReader r (ReachingDefsT lab c) where
  ask = lift' Reader.ask
  local f = lift $ lmap (\(m,(r,a)) -> (r,(m,a))) (Reader.local (unlift f))

instance ArrowAlloc x y c => ArrowAlloc x y (ReachingDefsT lab c) where
  alloc = lift' alloc
