{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}


module Control.Arrow.Fix.GarbageCollection where 

import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.Reader
import Control.Arrow.Transformer.State
import Control.Arrow.Transformer.Static
import Control.Arrow.Transformer.Writer

import Data.Monoidal
import Data.Profunctor
import Data.HashSet (HashSet) 
import Data.HashMap.Strict (HashMap)


class (Arrow c, Profunctor c) => ArrowGarbageCollection val addr c | c -> addr, c -> val where
  addLocalGCRoots :: c x y -> c (HashSet addr,x) y
  getGCRoots :: c () (HashSet addr)
  collectables :: c (HashSet addr, HashSet addr) (HashSet addr) 
  getAddrVal :: c val (HashSet addr) 
  reachables :: c (HashSet addr, HashMap addr val) (HashSet addr) 

  default getGCRoots :: (c ~ t c', ArrowTrans t, ArrowGarbageCollection val addr c') => c () (HashSet addr)
  default collectables :: (c ~ t c', ArrowTrans t, ArrowGarbageCollection val addr c') => c (HashSet addr, HashSet addr) (HashSet addr)
  default getAddrVal :: (c ~ t c', ArrowTrans t, ArrowGarbageCollection val addr c') => c val (HashSet addr)
  default reachables :: (c ~ t c', ArrowTrans t, ArrowGarbageCollection val addr c') => c (HashSet addr, HashMap addr val) (HashSet addr)


  getGCRoots = lift' getGCRoots
  collectables = lift' collectables
  getAddrVal = lift' getAddrVal
  reachables = lift' reachables
  {-# INLINE getGCRoots #-}
  {-# INLINE collectables #-}
  {-# INLINE getAddrVal #-}

  ------------- Instances --------------
instance ArrowGarbageCollection val addr c => ArrowGarbageCollection val addr (ConstT r c) where
  addLocalGCRoots f = lift $ \r -> addLocalGCRoots (unlift f r)
  {-# INLINE addLocalGCRoots #-}

instance ArrowGarbageCollection val addr c => ArrowGarbageCollection val addr (ReaderT r c) where
  addLocalGCRoots f = lift $ lmap shuffle1 (addLocalGCRoots (unlift f))
  {-# INLINE addLocalGCRoots #-}

instance ArrowGarbageCollection val addr c => ArrowGarbageCollection val addr (StateT s c) where
  addLocalGCRoots f = lift $ lmap shuffle1 (addLocalGCRoots (unlift f))
  {-# INLINE addLocalGCRoots #-}

instance (Applicative f, ArrowGarbageCollection val addr c) => ArrowGarbageCollection val addr (StaticT f c) where
  addLocalGCRoots (StaticT f) = StaticT $ addLocalGCRoots <$> f
  {-# INLINE addLocalGCRoots #-}

instance (Monoid w, ArrowGarbageCollection val addr c) => ArrowGarbageCollection val addr (WriterT w c) where
  addLocalGCRoots f = lift (addLocalGCRoots (unlift f))
  {-# INLINE addLocalGCRoots #-}

