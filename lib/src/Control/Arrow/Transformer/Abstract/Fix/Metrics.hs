{-# LANGUAGE GADTs #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Metrics where

import           Prelude hiding (pred,lookup,map,head,iterate,(.),id,truncate,elem,product,(**))

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Order
import           Control.Arrow.State
import           Control.Arrow.Trans
import           Control.Arrow.Fix.Metrics (ArrowFiltered)
import qualified Control.Arrow.Fix.Metrics as F
import           Control.Arrow.Fix.ControlFlow as CF
import           Control.Arrow.Fix.Chaotic as Chaotic
import           Control.Arrow.Fix.Cache as Cache
import           Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Fix.Context(ArrowContext)

import           Control.Arrow.Transformer.State

import           Data.Empty
import           Data.Foldable (fold)
import           Data.Identifiable
import           Data.Profunctor.Unsafe
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Coerce

import           Text.Printf

newtype MetricsT metric a c x y = MetricsT (StateT (metric a) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowLowerBounded z,
            ArrowComponent a,ArrowInComponent a,ArrowControlFlow stmt,
            ArrowStackDepth,ArrowStackElements a,ArrowContext ctx,ArrowTopLevel)

instance (IsEmpty (metrics a), ArrowRun c) => ArrowRun (MetricsT metrics a c) where
  type Run (MetricsT metrics a c) x y = Run c x (metrics a,y)
  run f = run (lmap (empty,) (unlift f))

instance ArrowTrans (MetricsT metrics a c) where
  type Underlying (MetricsT metrics a c) x y = c (metrics a,x) (metrics a,y)

instance ArrowLift (MetricsT metrics a) where
  lift' = MetricsT . lift'
  {-# INLINE lift' #-}

instance (Profunctor c,ArrowApply c) => ArrowApply (MetricsT metrics a c) where
  app = MetricsT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowState s c => ArrowState s (MetricsT metrics a c) where
  get = lift' get
  put = lift' put
  {-# INLINE get #-}
  {-# INLINE put #-}

-- Basic Metric ----------------------------------------------------------------
data Metrics a = Metrics { iteration :: !Int, metricCache :: HashMap a Metric }

data Metric = Metric { filtered :: !Int, stackLookups :: !Int, cacheEntries :: !Int, cacheLookups :: !Int, updates :: !Int } deriving (Show)
instance Semigroup Metric where
  Metric f1 s1 e1 l1 u1 <> Metric f2 s2 e2 l2 u2 = Metric (f1 + f2) (s1 + s2) (e1 + e2) (l1 + l2) (u1 + u2)
instance Monoid Metric where
  mempty = Metric 0 0 0 0 0
  mappend = (<>)
  {-# INLINE mappend #-}

csvHeader :: String
csvHeader = "Filtered,Stack Lookups,Cache Entries,Iteration,Cache Lookups,Cache Updates"

toCSV :: Metrics a -> String
toCSV (Metrics i m) =
  let Metric f s e l u = fold m
  in printf "%d,%d,%d,%d,%d,%d" f s e i l u

instance IsEmpty (Metrics a) where
  empty = Metrics 0 empty

instance (Identifiable a, Arrow c,Profunctor c) => ArrowFiltered a (MetricsT Metrics a c) where
  filtered = MetricsT $ proc a ->
    modifyMetric setFiltered -< a

instance (Identifiable a, ArrowStack a c) => ArrowStack a (MetricsT Metrics a c) where
  elem = MetricsT $ proc a -> do
    modifyMetric incrementStackLookups -< a
    lift' elem -< a
  push f = lift $ lmap (\(m, (a, x)) -> (a, (m, x))) (push (unlift f))
  {-# INLINE elem #-}
  {-# INLINE push #-}

instance (Identifiable a, ArrowChoice c, Profunctor c, ArrowCache a b c) => ArrowCache a b (MetricsT Metrics a c) where
  type Widening (MetricsT Metrics a c) = Cache.Widening c
  initialize = MetricsT $ proc a -> do
    modifyMetric incrementInitializes -< a
    initialize -< a
  lookup = MetricsT $ proc a -> do
    modifyMetric incrementCacheLookups -< a
    Cache.lookup -< a
  update = MetricsT $ proc (a,b) -> do
    modifyMetric incrementUpdates -< a
    update -< (a,b)
  write = MetricsT $ proc (a,b,s) -> do
    modifyMetric incrementUpdates -< a
    write -< (a,b,s)
  {-# INLINE initialize #-}
  {-# INLINE lookup #-}
  {-# INLINE update #-}
  {-# INLINE write #-}

instance ArrowIterateCache a b c => ArrowIterateCache a b (MetricsT Metrics a c) where
  nextIteration = MetricsT $ proc a -> do
    modify' (\((),Metrics i c) -> ((),Metrics (i + 1) c)) -< ()
    lift' nextIteration -< a
  {-# INLINE nextIteration #-}

instance (Identifiable a, ArrowParallelCache a b c) => ArrowParallelCache a b (MetricsT Metrics a c) where
  lookupOldCache = MetricsT $ proc a -> do
    modifyMetric incrementCacheLookups -< a
    Cache.lookupOldCache -< a
  lookupNewCache = MetricsT $ proc a -> do
    modifyMetric incrementCacheLookups -< a
    Cache.lookupNewCache -< a
  updateNewCache = MetricsT $ proc (a,b) -> do
    modifyMetric incrementUpdates -< a
    Cache.updateNewCache -< (a,b)
  {-# INLINE lookupOldCache #-}
  {-# INLINE lookupNewCache #-}
  {-# INLINE updateNewCache #-}

modifyMetric :: (Identifiable a, ArrowState (Metrics a) c) => (Metric -> Metric) -> c a ()
modifyMetric f = modify' (\(a,Metrics i m) -> ((),Metrics i (upsert f a m)))
{-# INLINE modifyMetric #-}

setFiltered :: Metric -> Metric
setFiltered m = m { filtered = 1 }

incrementInitializes :: Metric -> Metric
incrementInitializes m@Metric{..} = m { cacheEntries = 1 }

incrementCacheLookups :: Metric -> Metric
incrementCacheLookups m@Metric{..} = m { cacheLookups = cacheLookups + 1 }

incrementStackLookups :: Metric -> Metric
incrementStackLookups m@Metric{..} = m { stackLookups = stackLookups + 1 }

incrementUpdates :: Metric -> Metric
incrementUpdates m@Metric{..} = m { cacheEntries = 1, updates = updates + 1 }

upsert :: Identifiable a => Monoid b => (b -> b) -> a -> HashMap a b -> HashMap a b
upsert f a = M.insertWith (\_ _old -> f _old) a mempty
{-# INLINE upsert #-}

-- Metric for monotone Inputs ----------------------------------------------------------------
data Monotone a where
  Monotone :: Metrics b -> Monotone (a,b)

instance IsEmpty (Monotone (a,b)) where
  empty = Monotone empty

instance (Identifiable b, Arrow c,Profunctor c) => ArrowFiltered (a,b) (MetricsT Monotone (a,b) c) where
  filtered = MetricsT $ proc (_,b) ->
    modifyMetric' setFiltered -< b

instance (Identifiable b, ArrowStack (a,b) c) => ArrowStack (a,b) (MetricsT Monotone (a,b) c) where
  elem = MetricsT $ proc x@(_,b) -> do
    modifyMetric' incrementStackLookups -< b
    lift' elem -< x
  push f = lift $ lmap (\(m, (a, x)) -> (a, (m, x))) (push (unlift f))
  {-# INLINE elem #-}
  {-# INLINE push #-}

instance (Identifiable a', ArrowChoice c, Profunctor c, ArrowCache (a,a') b c) => ArrowCache (a,a') b (MetricsT Monotone (a,a') c) where
  type Widening (MetricsT Monotone (a,a') c) = Cache.Widening c
  initialize = MetricsT $ proc x@(_,a') -> do
    modifyMetric' incrementInitializes -< a'
    initialize -< x
  lookup = MetricsT $ proc x@(_,a') -> do
    modifyMetric' incrementCacheLookups -< a'
    Cache.lookup -< x
  update = MetricsT $ proc (x@(_,a'),b) -> do
    modifyMetric' incrementUpdates -< a'
    update -< (x,b)
  write = MetricsT $ proc (x@(_,a'),b,s) -> do
    modifyMetric' incrementUpdates -< a'
    write -< (x,b,s)
  {-# INLINE initialize #-}
  {-# INLINE lookup #-}
  {-# INLINE update #-}
  {-# INLINE write #-}

instance ArrowIterateCache x y c => ArrowIterateCache x y (MetricsT Monotone (a,a') c) where
  nextIteration = MetricsT $ proc a -> do
    modify' (\((),Monotone (Metrics i c)) -> ((),Monotone (Metrics (i + 1) c))) -< ()
    lift' nextIteration -< a
  {-# INLINE nextIteration #-}

instance (Identifiable a', ArrowParallelCache (a,a') b c) => ArrowParallelCache (a,a') b (MetricsT Monotone (a,a') c) where
  lookupOldCache = MetricsT $ proc x@(_,a') -> do
    modifyMetric' incrementCacheLookups -< a'
    Cache.lookupOldCache -< x
  lookupNewCache = MetricsT $ proc x@(_,a') -> do
    modifyMetric' incrementCacheLookups -< a'
    Cache.lookupNewCache -< x
  updateNewCache = MetricsT $ proc (x@(_,a'),b) -> do
    modifyMetric' incrementUpdates -< a'
    Cache.updateNewCache -< (x,b)
  {-# INLINE lookupOldCache #-}
  {-# INLINE lookupNewCache #-}
  {-# INLINE updateNewCache #-}

modifyMetric' :: (Identifiable b, ArrowState (Monotone (a,b)) c) => (Metric -> Metric) -> c b ()
modifyMetric' f = modify' (\(b, Monotone (Metrics i m)) -> ((), Monotone (Metrics i (upsert f b m))))
{-# INLINE modifyMetric' #-}
