{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Transformer.JumpTypes where

import           Prelude hiding (read)

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Globals
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Functions
import           Control.Arrow.EffectiveAddress
import           Control.Arrow.JumpTypes
import           Control.Arrow.Memory
import           Control.Arrow.Order
import           Control.Arrow.Reader
import           Control.Arrow.Serialize
import           Control.Arrow.Size
import           Control.Arrow.Stack
import           Control.Arrow.State
import           Control.Arrow.Table
import           Control.Arrow.Trans
import           Control.Arrow.WasmFrame

import           Control.Arrow.Transformer.Reader

import           Language.Wasm.Structure (ResultType)

import           Control.Category

import           Data.Profunctor

newtype JumpTypesT c x y = JumpTypesT (ReaderT [ResultType] c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift, ArrowState s,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowRun, ArrowFrame fd val, ArrowGlobals v, ArrowFunctions,
              ArrowStack st, ArrowSerialize val dat valTy datDecTy datEncTy, ArrowMemory addr bytes sz,
              ArrowSize val sz, ArrowEffectiveAddress base off addr, ArrowTable v1, ArrowJoin)

instance (ArrowReader r c) => ArrowReader r (JumpTypesT c) where
  ask = error "TODO: implement JumpTypesT.get"
  local = error "TODO: implement JumpTypesT.put"

instance ArrowTrans JumpTypesT where
    lift' a = JumpTypesT (lift' a)

instance (ArrowChoice c, Profunctor c) => ArrowJumpTypes (JumpTypesT c) where
    jumpType = JumpTypesT $ proc ix -> do
        rts <- ask -< ()
        returnA -< rts !! (fromIntegral ix)
    withJumpType (JumpTypesT f) = JumpTypesT $ proc (rt,x) -> do
        rts <- ask -< ()
        local f -< (rt:rts,x)
    localNoJumpTypes (JumpTypesT f) = JumpTypesT $ proc x -> local f -< ([],x)


instance ArrowFix (Underlying (JumpTypesT c) x y) => ArrowFix (JumpTypesT c x y) where
    type Fix (JumpTypesT c x y) = Fix (Underlying (JumpTypesT c) x y)

deriving instance (ArrowComplete y c) => ArrowComplete y (JumpTypesT c)
