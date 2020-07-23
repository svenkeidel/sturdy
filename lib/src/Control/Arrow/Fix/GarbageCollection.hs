{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}


module Control.Arrow.Fix.GarbageCollection where 

import Prelude hiding (pred)

import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Trans
import Control.Arrow.Transformer.Const
import Control.Arrow.Transformer.Reader
import Control.Arrow.Transformer.State
import Control.Arrow.Transformer.Static
import Control.Arrow.Transformer.Writer

import Data.Monoidal
import Data.Profunctor
import Data.HashSet(HashSet,union) 
import Debug.Trace as Debug
import Data.Hashable
import Data.Text.Prettyprint.Doc.Internal


class (Arrow c, Profunctor c) => ArrowGarbageCollection addr c | c -> addr where
  addLocalGCRoots :: c x y -> c (HashSet addr,x) y
  getGCRoots :: c () (HashSet addr)

  default getGCRoots :: (c ~ t c', ArrowTrans t, ArrowGarbageCollection addr c') => c () (HashSet addr)

  getGCRoots = lift' getGCRoots
  {-# INLINE getGCRoots #-}

collectRet :: (Eq addr, Hashable addr, ArrowChoice c, ArrowGarbageCollection addr c) 
  => (x -> Bool) -> (x -> HashSet addr) -> (y -> HashSet addr) -> (HashSet addr -> y -> HashSet addr) -> (y -> HashSet addr -> y) 
  -> FixpointCombinator c x y
collectRet pred getAddrIn getAddrOut getReachables removeUnreachables eval = proc x -> do
  let addrIn = getAddrIn x
  y <- addLocalGCRoots eval -< (addrIn, x)
  -- addGlobalGCRoots -< getAddrOut y
  if pred x
    then do 
      addrRoots <- getGCRoots -< () 
      let addrReachable = getReachables (union (union addrRoots (getAddrOut y)) addrIn) y
      let y_ = removeUnreachables y addrReachable
      returnA -< y_
    else returnA -< y 
{-# INLINE collectRet #-}

collectCall :: (Eq addr, Hashable addr, ArrowChoice c, ArrowGarbageCollection addr c) 
  => (x -> Bool) -> (x -> HashSet addr) -> (y -> HashSet addr) -> (HashSet addr -> x -> HashSet addr) -> (x -> HashSet addr -> x) 
  -> FixpointCombinator c x y
collectCall pred getAddrIn getAddrOut getReachables removeUnreachables eval = proc x -> do
  let addrIn = getAddrIn x
  addrRoots <- getGCRoots -< () -- move into if stmt 
  let x_ = (if pred x
            then do 
              let addrReachable = getReachables (union addrRoots addrIn) x
              removeUnreachables x addrReachable
            else x)  
  y <- addLocalGCRoots eval -< (addrIn, x_)
  -- addGlobalGCRoots -< getAddrOut y
  returnA -< y 
{-# INLINE collectCall #-}

trace :: (Eq addr, Hashable addr, ArrowChoice c, ArrowGarbageCollection addr c) 
  => (x -> HashSet addr) -> (y -> HashSet addr) -> (HashSet addr -> y -> HashSet addr)
  -> (HashSet addr -> Doc ann) -> (x -> Doc ann) -> (y -> Doc ann)
  -> FixpointCombinator c x y
trace getAddrIn getAddrOut getReachables showAddr printIn printOut eval = proc x -> do
  let addrIn = getAddrIn x
  y <- addLocalGCRoots eval -< (addrIn, x)
  let addrOut = getAddrOut y
  -- addGlobalGCRoots -< addrOut 
  addrRoots <- getGCRoots -< () 
  let addrReachable = getReachables (union addrRoots addrIn) y --unsound <- why unsound? 
  returnA -< Debug.trace (show (vsep ["AddrRoots", 
                                    showAddr addrRoots,
                                    "AddrReachable ",
                                    showAddr addrReachable,
                                    "In", 
                                    "AddrIn",
                                    showAddr addrIn,
                                    printIn x,
                                    "Out",
                                    "AddrOut",
                                    showAddr addrOut,
                                    printOut y,
                                    line])) y
{-# INLINE trace #-}

  ------------- Instances --------------
instance ArrowGarbageCollection addr c => ArrowGarbageCollection addr (ConstT r c) where
  addLocalGCRoots f = lift $ \r -> addLocalGCRoots (unlift f r)
  {-# INLINE addLocalGCRoots #-}

instance ArrowGarbageCollection addr c => ArrowGarbageCollection addr (ReaderT r c) where
  addLocalGCRoots f = lift $ lmap shuffle1 (addLocalGCRoots (unlift f))
  {-# INLINE addLocalGCRoots #-}

instance ArrowGarbageCollection addr c => ArrowGarbageCollection addr (StateT s c) where
  addLocalGCRoots f = lift $ lmap shuffle1 (addLocalGCRoots (unlift f))
  {-# INLINE addLocalGCRoots #-}

instance (Applicative f, ArrowGarbageCollection addr c) => ArrowGarbageCollection addr (StaticT f c) where
  addLocalGCRoots (StaticT f) = StaticT $ addLocalGCRoots <$> f
  {-# INLINE addLocalGCRoots #-}

instance (Monoid w, ArrowGarbageCollection addr c) => ArrowGarbageCollection addr (WriterT w c) where
  addLocalGCRoots f = lift (addLocalGCRoots (unlift f))
  {-# INLINE addLocalGCRoots #-}

