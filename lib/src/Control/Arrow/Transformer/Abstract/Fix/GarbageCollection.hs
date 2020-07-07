{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}


module Control.Arrow.Transformer.Abstract.Fix.GarbageCollection where 

import           Prelude hiding ((.))

import           Control.Arrow 
import           Control.Arrow.Trans
import           Control.Arrow.Fix.Cache 
import           Control.Arrow.Fix.ControlFlow 
import           Control.Arrow.Fix.GarbageCollection
import           Control.Arrow.Fix.Context (ArrowContext)
import           Control.Arrow.Transformer.Reader
import qualified Control.Arrow.Reader as Reader 
import           Control.Arrow.Transformer.State
import qualified Control.Arrow.State as State 
import           Control.Category

import           Data.Coerce
import           Data.Profunctor.Unsafe
import           Data.Hashable
import           Data.HashSet (HashSet)
import qualified Data.HashSet as Set 
import           Data.Empty


newtype GarbageCollectionT addr c x y = GarbageCollectionT (StateT (HashSet addr) (ReaderT (HashSet addr) c) x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowContext ctx,
              ArrowControlFlow stmt, ArrowCache a b, ArrowParallelCache a b,
              ArrowIterateCache a b)

instance (Eq addr, Hashable addr, ArrowChoice c, Profunctor c) => ArrowGarbageCollection addr (GarbageCollectionT addr c) where
  addLocalGCRoots (GarbageCollectionT eval) = GarbageCollectionT $ proc (addrs_new,x) -> do 
    addrs_old <- Reader.ask -< () 
    Reader.local eval -< (Set.union addrs_new addrs_old,x) 
  addGlobalGCRoots = GarbageCollectionT $ proc addrs_new -> do 
    addrs_old <- State.get -< () 
    let updated = Set.union addrs_new addrs_old 
    State.put -< updated
  -- TODO: Find way to remove global addr, or get rid of global addresses entirely 
  updateGlobalGCRoots = undefined
  getGCRoots = GarbageCollectionT $ proc _ -> do 
    addrs_stack <- Reader.ask -< () 
    addrs_global <- State.get -< () 
    returnA -< Set.union addrs_stack addrs_global 
  {-# INLINE addLocalGCRoots #-}
  {-# INLINE addGlobalGCRoots #-}
  {-# INLINE updateGlobalGCRoots #-}
  {-# INLINE getGCRoots #-}
  
instance (Profunctor c, ArrowApply c) => ArrowApply (GarbageCollectionT addr c) where
  app = GarbageCollectionT (app .# first coerce)
  {-# INLINE app #-}

instance ArrowLift (GarbageCollectionT addr c) where 
  type Underlying (GarbageCollectionT addr c) x y = c (HashSet addr, (HashSet addr, x)) (HashSet addr, y) 

instance ArrowTrans (GarbageCollectionT addr) where 
  lift' = GarbageCollectionT . lift' . lift' 
  {-# INLINE lift' #-}

instance (ArrowRun c) => ArrowRun (GarbageCollectionT addr c) where
  type Run (GarbageCollectionT addr c) x y = Run c x (HashSet addr,y)
  run f = run (lmap (\x -> (empty, (empty, x)))  (unlift f))
  {-# INLINE run #-}
