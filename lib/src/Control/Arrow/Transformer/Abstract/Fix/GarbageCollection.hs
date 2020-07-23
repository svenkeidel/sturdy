{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}



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


newtype GarbageCollectionT addr c x y = GarbageCollectionT (ReaderT (HashSet addr) c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowContext env,
              ArrowControlFlow stmt, ArrowCache a b, ArrowParallelCache a b,
              ArrowIterateCache a b)

instance (Eq addr, Hashable addr, ArrowChoice c, Profunctor c) => ArrowGarbageCollection addr (GarbageCollectionT addr c) where
  addLocalGCRoots (GarbageCollectionT eval) = GarbageCollectionT $ proc (addrs_new,x) -> do 
    addrs_old <- Reader.ask -< () 
    Reader.local eval -< (Set.union addrs_new addrs_old,x) 
  getGCRoots = GarbageCollectionT $ proc _ -> do 
    addrs_stack <- Reader.ask -< () 
    returnA -< addrs_stack
  {-# INLINE addLocalGCRoots #-}
  {-# INLINE getGCRoots #-}
  
instance (Profunctor c, ArrowApply c) => ArrowApply (GarbageCollectionT addr c) where
  app = GarbageCollectionT (app .# first coerce)
  {-# INLINE app #-}


instance (ArrowRun c) => ArrowRun (GarbageCollectionT addr c) where
  type Run (GarbageCollectionT addr c) x y = Run c x y
  run f = run (runGarbageCollectionT f)
  {-# INLINE run #-}

runGarbageCollectionT :: (Profunctor c) => GarbageCollectionT addr c x y -> c x y
runGarbageCollectionT (GarbageCollectionT f) = lmap (empty,) (runReaderT f)
{-# INLINE runGarbageCollectionT #-}