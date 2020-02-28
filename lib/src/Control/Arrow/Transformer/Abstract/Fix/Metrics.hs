{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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
import           Control.Arrow.Fix.Iterate as Iterate

import           Control.Arrow.Transformer.State

import           Data.Empty
import           Data.Foldable (fold)
import           Data.Identifiable
import           Data.Profunctor.Unsafe
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Order
import           Data.Coerce

import           Text.Printf

newtype MetricsT a c x y = MetricsT (StateT (Metrics a) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowComponent comp,ArrowControlFlow stmt,
            ArrowStackDepth,ArrowStackElements a,ArrowContext ctx,ArrowTopLevel)

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

instance (Identifiable a, Arrow c,Profunctor c) => ArrowFiltered a (MetricsT a c) where
  filtered = MetricsT $ proc a ->
    modifyMetric setFiltered -< a

instance (Identifiable a, ArrowApply c, ArrowStack a c) => ArrowStack a (MetricsT a c) where
  elem = MetricsT $ proc a -> do
    modifyMetric incrementStackLookups -< a
    lift' elem -< a
  push f = lift $ proc (m,a) ->
    push (proc a' -> unlift f -< (m,a')) -<< a
  {-# INLINE elem #-}
  {-# INLINE push #-}

instance (Identifiable a, ArrowChoice c, Profunctor c, ArrowCache a b c) => ArrowCache a b (MetricsT a c) where
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

instance (Identifiable a, Profunctor c, Arrow c, ArrowIterate c) => ArrowIterate (MetricsT a c) where
  nextIteration = MetricsT $ proc a -> do
    modify' (\((),Metrics i c) -> ((),Metrics (i + 1) c)) -< ()
    lift' nextIteration -< a
  {-# INLINE nextIteration #-}

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

instance (ArrowRun c) => ArrowRun (MetricsT a c) where
  type Run (MetricsT a c) x y = Run c x (Metrics a,y)
  run f = run (lmap (empty,) (unlift f))

instance ArrowTrans (MetricsT a c) where
  type Underlying (MetricsT a c) x y = c (Metrics a,x) (Metrics a,y)

instance ArrowLift (MetricsT a) where
  lift' = MetricsT . lift'
  {-# INLINE lift' #-}

instance (Complete y, ArrowEffectCommutative c) => ArrowComplete y (MetricsT a c) where
  MetricsT f <⊔> MetricsT g = MetricsT $ rmap (uncurry (⊔)) (f &&& g)
  {-# INLINE (<⊔>) #-}

instance ArrowEffectCommutative c => ArrowEffectCommutative (MetricsT a c)

instance (Profunctor c,ArrowApply c) => ArrowApply (MetricsT a c) where
  app = MetricsT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowState s c => ArrowState s (MetricsT a c) where
  get = lift' get
  put = lift' put
  {-# INLINE get #-}
  {-# INLINE put #-}
