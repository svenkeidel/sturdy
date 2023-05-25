{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Arrow.Transformer.Stack where

import           Data

import           Control.Category hiding (id)
import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Functions
import           Control.Arrow.EffectiveAddress
import           Control.Arrow.Memory
import           Control.Arrow.Order
import           Control.Arrow.Reader
import           Control.Arrow.Serialize
import           Control.Arrow.Size
import           Control.Arrow.Stack
import           Control.Arrow.State
import           Control.Arrow.Globals
import           Control.Arrow.Table
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.State
import           Control.Arrow.WasmFrame
import           Control.Arrow.Writer

import           Data.Profunctor


-- | Arrow transformer that adds a stack to a computation.
newtype StackT v c x y = StackT (StateT (JoinList v) c x y)
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowTrans,ArrowLift,ArrowRun,
            ArrowConst r, ArrowReader r, ArrowFail e, ArrowExcept e, ArrowWriter w,
            ArrowGlobals v1, ArrowFunctions, ArrowFrame fd v1, ArrowMemory addr bytes sz,
            ArrowEffectiveAddress v1 n addr, ArrowSerialize v1 bytes valTy loadTy storeTy,
            ArrowSize v1 sz, ArrowTable v1)

deriving instance (ArrowComplete (JoinList v, y) c) => ArrowComplete y (StackT v c)

instance (ArrowChoice c, Profunctor c) => ArrowStack v (StackT v c) where
  push = StackT $ modify $ arr $ \(v, JoinList st) -> ((), JoinList $ v : st)
  pop = StackT $ modify $ arr $ \((), JoinList (v:st)) -> (v, JoinList st)
  peek = StackT $ get >>^ (\(JoinList vs) -> head vs)
  ifEmpty (StackT f) (StackT g) = StackT $ proc x -> do
    st <- get -< ()
    case st of
      [] -> g -< x
      _ -> f -< x
  localFreshStack (StackT f) = StackT $ proc x -> do
    (JoinList st) <- get -< ()
    put -< []
    y <- f -< x
    (JoinList stNew) <- get -< ()
    put -< JoinList $ stNew ++ st
    returnA -< y

instance ArrowFix (Underlying (StackT v c) x y) => ArrowFix (StackT v c x y) where
    type Fix (StackT v c x y) = Fix (Underlying (StackT v c) x y)--StackT v (Fix c ([v],x) ([v],y))
