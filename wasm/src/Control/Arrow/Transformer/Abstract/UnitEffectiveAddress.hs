{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Transformer.Abstract.UnitEffectiveAddress where

import           Abstract (Addr(..))

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Functions
import           Control.Arrow.EffectiveAddress
import           Control.Arrow.Order
import           Control.Arrow.Reader
import           Control.Arrow.Serialize
import           Control.Arrow.Size
import           Control.Arrow.Stack
import           Control.Arrow.Globals
import           Control.Arrow.Store
import           Control.Arrow.Table
import           Control.Arrow.Trans
import           Control.Arrow.WasmFrame

import           Control.Category

import           Data.Profunctor

newtype EffectiveAddressT c x y = EffectiveAddressT (c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowReader r, ArrowGlobals val, ArrowFunctions, ArrowSize v sz,
              ArrowSerialize val dat valTy datDecTy datEncTy, ArrowTable v, ArrowJoin)

instance ArrowTrans EffectiveAddressT where
  -- lift' :: c x y -> MemoryT v c x y
  lift' = EffectiveAddressT

instance (Arrow c, Profunctor c) => ArrowEffectiveAddress base off Addr (EffectiveAddressT c) where
  effectiveAddress = arr $ const Addr


deriving instance (Arrow c, Profunctor c, ArrowComplete y c) => ArrowComplete y (EffectiveAddressT c)

instance (ArrowLift c, ArrowFix (Underlying (EffectiveAddressT c) x y)) => ArrowFix (EffectiveAddressT c x y) where
    type Fix (EffectiveAddressT c x y) = Fix (Underlying (EffectiveAddressT c) x y)
