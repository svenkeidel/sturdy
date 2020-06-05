{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Abstract.Fix.Metrics where

import           Prelude hiding (pred,lookup,map,head,iterate,(.),id,truncate,elem,product,(**))

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Primitive
import           Control.Arrow.Order
import           Control.Arrow.State
import           Control.Arrow.Trans
import           Control.Arrow.Fix.Metrics (ArrowMetrics)
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
            ArrowStackDepth,ArrowStackElements a,ArrowContext ctx,ArrowTopLevel,
            ArrowGetCache cache, ArrowPrimitive)

instance (IsEmpty (metrics a), ArrowRun c) => ArrowRun (MetricsT metrics a c) where
  type Run (MetricsT metrics a c) x y = Run c x (metrics a,y)
  run f = run (lmap (empty,) (unlift f))

instance ArrowLift (MetricsT metrics a c) where
  type Underlying (MetricsT metrics a c) x y = c (metrics a,x) (metrics a,y)

instance ArrowTrans (MetricsT metrics a) where
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
newtype Metrics a = Metrics (HashMap a Metric)

data Metric = Metric
            { filtered :: !Int
            , evaluated :: !Int
            , iteration :: !Int
            , stackLookups :: !Int
            , cacheEntries :: !Int
            , cacheLookups :: !Int
            , cacheUpdates :: !Int
            }
  deriving (Show)

instance Semigroup Metric where
  m1 <> m2 = Metric
    { filtered = filtered m1 + filtered m2
    , evaluated = evaluated m1 + evaluated m2
    , iteration = iteration m1 + iteration m2
    , stackLookups = stackLookups m1 + stackLookups m2
    , cacheEntries = cacheEntries m1 + cacheEntries m2
    , cacheLookups = cacheLookups m1 + cacheLookups m2
    , cacheUpdates = cacheUpdates m1 + cacheUpdates m2
    }

instance Monoid Metric where
  mappend = (<>)
  mempty = Metric
    { filtered = 0, evaluated = 0, iteration = 0, stackLookups = 0
    , cacheEntries = 0, cacheLookups = 0, cacheUpdates = 0 }
  {-# INLINE mappend #-}

csvHeader :: String
csvHeader = "Filtered,Evaluated,Iteration,Stack Lookups,Cache Entries,Cache Lookups,Cache Updates"

toCSV :: Metrics a -> String
toCSV (Metrics metrics) =
  let Metric {..} = fold metrics
  in printf "%d,%d,%d,%d,%d,%d,%d"
            filtered evaluated iteration stackLookups cacheEntries cacheLookups cacheUpdates

instance IsEmpty (Metrics a) where
  empty = Metrics empty

instance (Identifiable a, Arrow c,Profunctor c) => ArrowMetrics a (MetricsT Metrics a c) where
  filtered = MetricsT $ proc a ->
    modifyMetric setFiltered -< a
  evaluated = MetricsT $ proc a ->
    modifyMetric incrementEvaluated -< a
  iterated = MetricsT $ proc a ->
    modifyMetric incrementIterated -< a

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
  update = MetricsT $ proc (st,a,b) -> do
    modifyMetric incrementUpdates -< a
    update -< (st,a,b)
  write = MetricsT $ proc (a,b,s) -> do
    modifyMetric incrementUpdates -< a
    write -< (a,b,s)
  {-# INLINE initialize #-}
  {-# INLINE lookup #-}
  {-# INLINE update #-}
  {-# INLINE write #-}

instance (Identifiable a, ArrowIterateCache a b c) => ArrowIterateCache a b (MetricsT Metrics a c) where
  nextIteration = MetricsT $ proc (a,b) -> do
    modifyMetric incrementIterated -< a
    lift' nextIteration -< (a,b)
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
modifyMetric f = modify' (\(a,Metrics m) -> ((),Metrics (upsert f a m)))
{-# INLINE modifyMetric #-}

setFiltered :: Metric -> Metric
setFiltered m = m { filtered = 1 }

incrementEvaluated :: Metric -> Metric
incrementEvaluated m@Metric{..} = m { evaluated = evaluated + 1 }

incrementIterated :: Metric -> Metric
incrementIterated m@Metric{..} = m { iteration = iteration + 1 }

incrementInitializes :: Metric -> Metric
incrementInitializes m@Metric{..} = m { cacheEntries = 1 }

incrementCacheLookups :: Metric -> Metric
incrementCacheLookups m@Metric{..} = m { cacheLookups = cacheLookups + 1 }

incrementStackLookups :: Metric -> Metric
incrementStackLookups m@Metric{..} = m { stackLookups = stackLookups + 1 }

incrementUpdates :: Metric -> Metric
incrementUpdates m@Metric{..} = m { cacheEntries = 1, cacheUpdates = cacheUpdates + 1 }

upsert :: Identifiable a => Monoid b => (b -> b) -> a -> HashMap a b -> HashMap a b
upsert f a = M.insertWith (\_ _old -> f _old) a mempty
{-# INLINE upsert #-}

-- Metric for monotone Inputs ----------------------------------------------------------------
data Monotone a where
  Monotone :: Metrics b -> Monotone (a,b)

instance IsEmpty (Monotone (a,b)) where
  empty = Monotone empty

instance (Identifiable a', Arrow c,Profunctor c) => ArrowMetrics (a,a') (MetricsT Monotone (a,a') c) where
  filtered = MetricsT $ proc (_,a') ->
    modifyMetric' setFiltered -< a'
  evaluated = MetricsT $ proc (_,a') ->
    modifyMetric' incrementEvaluated -< a'
  iterated = MetricsT $ proc (_,a') ->
    modifyMetric' incrementIterated -< a'
  {-# INLINE filtered #-}
  {-# INLINE evaluated #-}
  {-# INLINE iterated #-}

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
  update = MetricsT $ proc (st,x@(_,a'),b) -> do
    modifyMetric' incrementUpdates -< a'
    update -< (st,x,b)
  write = MetricsT $ proc (x@(_,a'),b,s) -> do
    modifyMetric' incrementUpdates -< a'
    write -< (x,b,s)
  {-# INLINE initialize #-}
  {-# INLINE lookup #-}
  {-# INLINE update #-}
  {-# INLINE write #-}

instance (Identifiable a', ArrowIterateCache (a,a') b c) => ArrowIterateCache (a,a') b (MetricsT Monotone (a,a') c) where
  nextIteration = MetricsT $ proc x@((_,a'),_) -> do
    modifyMetric' incrementIterated -< a'
    lift' nextIteration -< x
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
modifyMetric' f = modify' (\(b, Monotone (Metrics m)) -> ((), Monotone (Metrics (upsert f b m))))
{-# INLINE modifyMetric' #-}
