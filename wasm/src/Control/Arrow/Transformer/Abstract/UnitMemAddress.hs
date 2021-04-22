{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Transformer.Abstract.UnitMemAddress where

import           Abstract (Addr(..))

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.MemAddress
import           Control.Arrow.Order
import           Control.Arrow.Reader
import           Control.Arrow.Serialize
import           Control.Arrow.Size
import           Control.Arrow.Stack
import           Control.Arrow.StaticGlobalState
import           Control.Arrow.Store
import           Control.Arrow.Table
import           Control.Arrow.Trans
import           Control.Arrow.WasmFrame

import           Control.Category

import           Data.Profunctor

newtype MemAddressT c x y = MemAddressT (c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowReader r, ArrowStaticGlobalState val, ArrowSize v sz,
              ArrowSerialize val dat valTy datDecTy datEncTy, ArrowTable v, ArrowJoin)

instance ArrowTrans MemAddressT where
  -- lift' :: c x y -> MemoryT v c x y
  lift' = MemAddressT

instance (Arrow c, Profunctor c) => ArrowMemAddress base off Addr (MemAddressT c) where
  memaddr = arr $ const Addr


deriving instance (Arrow c, Profunctor c, ArrowComplete y c) => ArrowComplete y (MemAddressT c)

instance (ArrowLift c, ArrowFix (Underlying (MemAddressT c) x y)) => ArrowFix (MemAddressT c x y) where
    type Fix (MemAddressT c x y) = Fix (Underlying (MemAddressT c) x y)
