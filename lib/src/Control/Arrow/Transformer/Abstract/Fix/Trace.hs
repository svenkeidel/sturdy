{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Arrow.Transformer.Abstract.Fix.Trace where

import Prelude hiding (pred,lookup,map,head,iterate,(.),truncate)

import Control.Category
import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.Cache as Cache
import Control.Arrow.Order

import Data.Profunctor.Unsafe
import Data.Coerce

import Debug.Trace as Debug
import Text.Printf

newtype TraceT c x y = TraceT (c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowComplete z,ArrowJoin,ArrowEffectCommutative)

instance (Show a, Show b, ArrowReuse a b c) => ArrowReuse a b (TraceT c) where
  reuse (TraceT f) = TraceT $ reuse $ proc (a,b) -> do
    f -< Debug.trace (printf "REUSE\n\tx: %s\n\ty: %s\n\n" (show a) (show b)) (a,b)

instance (Show a, Show b, ArrowCache a b c) => ArrowCache a b (TraceT c) where
  lookup = TraceT $ proc a -> do
    b  <- lookup -< a
    returnA -< Debug.trace (printf "LOOKUP\n\tx: %s\n\ty: %s\n\n" (show a) (show b)) b
  update = TraceT $ proc (a,b) -> do
    (s,b') <- update -< (a,b)
    returnA -< Debug.trace (printf "UPDATE\n\tx: %s\n\ty: %s -> %s, %s\n\n" (show a) (show b) (show b') (show s))  (s,b')
  write = TraceT $ proc (a,b,s) -> do
    write -< Debug.trace (printf "WRITE\n\tx: %s\n\ty: %s\n\t%s\n\n" (show a) (show b) (show s)) (a,b,s)
  setStable = TraceT $ proc (s,a) -> 
    setStable -< Debug.trace (printf "STABLE: %s\n\n" (show s)) (s,a)
  {-# INLINE lookup #-}
  {-# INLINE update #-}
  {-# INLINE write #-}
  {-# INLINE setStable #-}

runTraceT :: TraceT c x y -> c x y
runTraceT (TraceT f) = f
{-# INLINE runTraceT #-}

instance ArrowRun c => ArrowRun (TraceT c) where
  type Run (TraceT c) x y = Run c x y
  run f = run (runTraceT f)


instance (Profunctor c,ArrowApply c) => ArrowApply (TraceT c) where
  app = TraceT (app .# first coerce)
  {-# INLINE app #-}

