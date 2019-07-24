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
module Control.Arrow.Transformer.Abstract.ReachingDefinitions(
  ReachingDefsT(..),
  reachingDefsT,
  runReachingDefsT,
  runReachingDefsT',
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

import           Data.Abstract.DiscretePowerset(Pow)
import qualified Data.Abstract.DiscretePowerset as P

import           Data.Identifiable
import           Data.Label
import           Data.Profunctor
import           Data.Profunctor.Unsafe((.#))
import           Data.Coerce

newtype ReachingDefsT lab c x y = ReachingDefsT (ReaderT (Maybe lab) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowLift,
            ArrowState s,
            ArrowEnv var val, ArrowClosure var val env,
            ArrowFail e,ArrowExcept e,
            ArrowLowerBounded, ArrowComplete z)

reachingDefsT :: (Arrow c,Profunctor c) => c (Maybe lab,x) y -> ReachingDefsT lab c x y
reachingDefsT = lift
{-# INLINE reachingDefsT #-}

runReachingDefsT :: (Arrow c,Profunctor c) => ReachingDefsT lab c x y -> c (Maybe lab,x) y
runReachingDefsT = unlift
{-# INLINE runReachingDefsT #-}

runReachingDefsT' :: (Arrow c,Profunctor c) => ReachingDefsT lab c x y -> c x y
runReachingDefsT' f = lmap (\x -> (Nothing,x)) (runReachingDefsT f)
{-# INLINE runReachingDefsT' #-}

instance ArrowRun c => ArrowRun (ReachingDefsT lab c) where
  type Rep (ReachingDefsT lab c) x y = Rep c x y
  run = run . runReachingDefsT'

instance (Identifiable var, Identifiable lab, ArrowStore var (val,Pow lab) c) => ArrowStore var val (ReachingDefsT lab c) where
  type Join y (ReachingDefsT lab c) = Store.Join y c
  read (ReachingDefsT f) (ReachingDefsT g) = ReachingDefsT $ read (lmap (\((v,_::Pow lab),x) -> (v,x)) f) g
  write = reachingDefsT $ lmap (\(lab,(var,val)) -> (var,(val,P.fromMaybe lab))) write

type instance Fix x y (ReachingDefsT lab c) = ReachingDefsT lab (Fix x y c)
instance (HasLabel x lab, Arrow c, ArrowFix x y c) => ArrowFix x y (ReachingDefsT lab c) where
  fix f = ReachingDefsT $ ReaderT $ proc (_,x) -> fix (unwrap . f . lift') -< x
    where
      unwrap :: HasLabel x lab => ReachingDefsT lab c x y -> c x y
      unwrap f' = lmap (\x -> (Just (label x),x)) (runReachingDefsT f')

instance (ArrowApply c,Profunctor c) => ArrowApply (ReachingDefsT lab c) where
  app = ReachingDefsT (app .# first coerce)

instance ArrowReader r c => ArrowReader r (ReachingDefsT lab c) where
  ask = lift' Reader.ask
  local f = lift $ lmap (\(m,(r,a)) -> (r,(m,a))) (Reader.local (unlift f))
