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
  ReachingDefsT(..),
  reachingDefs,
  runReachingDefs,
) where

import           Prelude hiding ((.),read,id)

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
import           Control.Arrow.Store as Store
import           Control.Arrow.Environment
import           Control.Arrow.Transformer.Reader

import           Data.Identifiable
import           Data.Order
import           Data.Label
import           Data.Abstract.DiscretePowerset(Pow)
import qualified Data.Abstract.DiscretePowerset as P

newtype ReachingDefsT lab c x y = ReachingDefsT (ReaderT (Maybe lab) c x y)

reachingDefs :: Arrow c => c (Maybe lab,x) y -> ReachingDefsT lab c x y
reachingDefs = ReachingDefsT . ReaderT

runReachingDefs :: Arrow c => ReachingDefsT lab c x y -> c (Maybe lab,x) y
runReachingDefs (ReachingDefsT (ReaderT f)) = f

instance (Identifiable var, Identifiable lab, ArrowStore var (val,Pow lab) c) => ArrowStore var val (ReachingDefsT lab c) where
  type Join (ReachingDefsT lab c) ((val,x),x) y = Store.Join c (((val,Pow lab),(Maybe lab,x)), (Maybe lab,x)) y
  read (ReachingDefsT f) (ReachingDefsT g) = ReachingDefsT $ read ((\((v,_::Pow lab),x) -> (v,x)) ^>> f) g
  write = reachingDefs $ proc (lab,(var,val)) ->
    write -< (var,(val,P.fromMaybe lab))

type instance Fix x y (ReachingDefsT lab c) = ReachingDefsT lab (Fix x y c)
instance (HasLabel x lab, Arrow c, ArrowFix x y c) => ArrowFix x y (ReachingDefsT lab c) where
  fix f = ReachingDefsT $ ReaderT $ proc (_,x) -> fix (unwrap . f . wrap) -< x
    where
      wrap :: c x y -> ReachingDefsT lab c x y
      wrap = lift

      unwrap :: HasLabel x lab => ReachingDefsT lab c x y -> c x y
      unwrap f' = (Just . label &&& id) ^>> runReachingDefs f'

instance ArrowApply c => ArrowApply (ReachingDefsT lab c) where
  app = ReachingDefsT ((\(ReachingDefsT f,x) -> (f,x)) ^>> app)

instance ArrowLift (ReachingDefsT lab) where
  lift f = reachingDefs (snd ^>> f)

instance ArrowReader r c => ArrowReader r (ReachingDefsT lab c) where
  ask = lift ask
  local (ReachingDefsT (ReaderT f)) = ReachingDefsT $ ReaderT $ (\(m,(r,a)) -> (r,(m,a))) ^>> local f

instance ArrowAlloc x y c => ArrowAlloc x y (ReachingDefsT lab c) where
  alloc = lift alloc

instance ArrowRand v c => ArrowRand v (ReachingDefsT lab c) where
  random = lift random

deriving instance Arrow c => Category (ReachingDefsT lab c)
deriving instance Arrow c => Arrow (ReachingDefsT lab c)
deriving instance ArrowChoice c => ArrowChoice (ReachingDefsT lab c)
deriving instance ArrowFail e c => ArrowFail e (ReachingDefsT lab c)
deriving instance ArrowExcept e c => ArrowExcept e (ReachingDefsT lab c)
deriving instance ArrowState s c => ArrowState s (ReachingDefsT lab c)
deriving instance ArrowEnv x y env c => ArrowEnv x y env (ReachingDefsT lab c)
deriving instance ArrowCond val c => ArrowCond val (ReachingDefsT lab c)

deriving instance PreOrd (c (Maybe lab,x) y) => PreOrd (ReachingDefsT lab c x y)
deriving instance LowerBounded (c (Maybe lab,x) y) => LowerBounded (ReachingDefsT lab c x y)
deriving instance Complete (c (Maybe lab,x) y) => Complete (ReachingDefsT lab c x y)
deriving instance CoComplete (c (Maybe lab,x) y) => CoComplete (ReachingDefsT lab c x y)
deriving instance UpperBounded (c (Maybe lab,x) y) => UpperBounded (ReachingDefsT lab c x y)
