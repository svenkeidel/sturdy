{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Transformer.Abstract.UnitSize where

import           Abstract (BaseValue(..),Size(..))
import           UnitAnalysisValue (Value(..),valueI32)
import qualified TaintAnalysisValue as Taint

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
import           Control.Arrow.Globals
import           Control.Arrow.Store
import           Control.Arrow.Table
import           Control.Arrow.Trans
import           Control.Arrow.WasmFrame

import           Control.Category

import           Data.Coerce (coerce)
import           Data.Profunctor

newtype SizeT v c x y = SizeT (c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowReader r, ArrowGlobals val, ArrowMemory addr bytes s,
              ArrowEffectiveAddress base off addr, ArrowFunctions,
              ArrowSerialize val dat valTy datDecTy datEncTy, ArrowTable v, ArrowJoin)

instance ArrowTrans (SizeT v) where
  -- lift' :: c x y -> MemoryT v c x y
  lift' = SizeT

instance (ArrowChoice c, Profunctor c) => ArrowSize Value Size (SizeT Value c) where
  valToSize = proc (Value v) -> case v of
    (VI32 _) -> returnA -< Size
    _        -> returnA -< error "valToSize: argument needs to be an i32 integer."

  sizeToVal = proc Size -> returnA -< valueI32


untainted :: Arrow c => SizeT v c x v -> SizeT (Taint.Value v) c x (Taint.Value v)
untainted f = proc x -> do
  v <- liftSizeT f -< x
  returnA -< Taint.Value Taint.Untainted v
{-# INLINE untainted #-}

liftSizeT :: SizeT v c x y -> SizeT (Taint.Value v) c x y
liftSizeT = coerce
{-# INLINE liftSizeT #-}

instance (Arrow c, ArrowSize v Size (SizeT v c)) => ArrowSize (Taint.Value v) Size (SizeT (Taint.Value v) c) where
    valToSize = proc (Taint.Value _t v) -> do
        liftSizeT valToSize -< v

    sizeToVal = proc Size ->
        untainted sizeToVal -< Size

deriving instance (Arrow c, Profunctor c, ArrowComplete y c) => ArrowComplete y (SizeT v c)

instance (ArrowLift c, ArrowFix (Underlying (SizeT v c) x y)) => ArrowFix (SizeT v c x y) where
    type Fix (SizeT v c x y) = Fix (Underlying (SizeT v c) x y)
