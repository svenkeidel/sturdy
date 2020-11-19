{-# LANGUAGE Arrows #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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

import           Data.Kind
import           Data.Label
import           Data.Profunctor
import           Data.Profunctor.Unsafe((.#))
import           Data.Coerce
import           Data.Utils

newtype ReachingDefsT (f :: Type -> Type) c x y = ReachingDefsT (ReaderT (Maybe Label) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowLift,ArrowTrans,
            ArrowState s, ArrowEnv var val, ArrowClosure expr cls,
            ArrowFail e,ArrowExcept e, ArrowLowerBounded a, ArrowComplete z)

reachingDefsT :: c (Maybe Label,x) y -> ReachingDefsT f c x y
reachingDefsT = lift
{-# INLINE reachingDefsT #-}

runReachingDefsT :: Profunctor c => ReachingDefsT f c x y -> c x y
runReachingDefsT f = lmap (Nothing,) (unlift f)
{-# INLINE runReachingDefsT #-}

runReachingDefsT' :: ReachingDefsT f c x y -> c (Maybe Label,x) y
runReachingDefsT' = unlift
{-# INLINE runReachingDefsT' #-}

instance ArrowRun c => ArrowRun (ReachingDefsT f c) where
  type Run (ReachingDefsT f c) x y = Run c x y
  run = run . runReachingDefsT
  {-# INLINE run #-}

instance (ArrowStore var (val,f Label) c, IsEmpty (f Label), IsSingleton (f Label), Elem (f Label) ~ Label) =>
    ArrowStore var val (ReachingDefsT f c) where
  type Join y (ReachingDefsT f c) = Store.Join y c
  read (ReachingDefsT f) (ReachingDefsT g) = ReachingDefsT $ read (lmap (\((v,_),x) -> (v,x)) f) g
  write = reachingDefsT $ lmap (\(lab,(var,val)) -> (var,(val,fromMaybe lab))) write
  {-# INLINE read #-}
  {-# INLINE write #-}

instance (HasLabel x, Arrow c, ArrowFix (c x y), Profunctor c) => ArrowFix (ReachingDefsT f c x y) where
  type Fix (ReachingDefsT f c x y) = Fix (c x y)
  fix f = ReachingDefsT $ ReaderT $ proc (_,x) -> fix (unwrap . f . lift') -< x
    where
      unwrap :: ReachingDefsT f c x y -> c x y
      unwrap g = lmap (\x -> (Just (label x),x)) (runReachingDefsT' g)
  {-# INLINE fix #-}

instance (ArrowApply c,Profunctor c) => ArrowApply (ReachingDefsT f c) where
  app = ReachingDefsT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowReader r c => ArrowReader r (ReachingDefsT f c) where
  ask = lift' Reader.ask
  local f = lift $ lmap (\(m,(r,a)) -> (r,(m,a))) (Reader.local (unlift f))
  {-# INLINE ask #-}
  {-# INLINE local #-}
