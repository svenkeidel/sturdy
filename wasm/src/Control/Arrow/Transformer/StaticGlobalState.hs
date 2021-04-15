{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Transformer.StaticGlobalState where

import           Prelude hiding (read)

import           Data

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.MemAddress
import           Control.Arrow.Memory
import           Control.Arrow.MemSizable
import           Control.Arrow.Order
import           Control.Arrow.Reader
import           Control.Arrow.Serialize
import           Control.Arrow.Stack
import           Control.Arrow.State
import           Control.Arrow.StaticGlobalState
import           Control.Arrow.Table
import           Control.Arrow.Trans
import           Control.Arrow.WasmFrame

import           Control.Arrow.Transformer.State
--import           Control.Arrow.Transformer.Abstract.Store

import           Control.Category

import           Data.Vector ((!), (//))

import           Data.Profunctor

newtype StaticGlobalStateT v c x y = StaticGlobalStateT (StateT (StaticGlobalState v) c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift, ArrowReader r,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowSerialize val dat valTy datDecTy datEncTy, ArrowMemory addr bytes,
              ArrowMemSizable val, ArrowMemAddress base off addr, ArrowTable v1, ArrowJoin)

instance (ArrowState s c) => ArrowState s (StaticGlobalStateT v c) where
  get = error "TODO: implement StaticGlobalStateT.get"
  put = error "TODO: implement StaticGlobalStateT.put"
    -- TODO

instance ArrowTrans (StaticGlobalStateT v) where
    lift' a = StaticGlobalStateT (lift' a)

instance (ArrowChoice c, Profunctor c) => ArrowStaticGlobalState v (StaticGlobalStateT v c) where
    readGlobal =
        StaticGlobalStateT $ proc i -> do
            StaticGlobalState{globalInstances=vec} <- get -< ()
            let (GlobInst _ val) = vec ! i
            returnA -< val
    writeGlobal =
        StaticGlobalStateT $ proc (i,v) -> do
            store@StaticGlobalState{globalInstances=vec} <- get -< ()
            let (GlobInst m _) = vec ! i
            if m == Const
                then returnA -< error $ "writing to constant global " ++ show i
                else put -< store{globalInstances=vec // [(i, GlobInst m v)]}
    readFunction (StaticGlobalStateT funcCont) =
        StaticGlobalStateT $ proc (i,x) -> do
            StaticGlobalState{funcInstances = fs} <- get -< ()
            case fs ! i of
                FuncInst fTy modInst bdy  -> funcCont -< ((fTy,modInst,bdy),x)
                _                         -> returnA -< error "not yet implemented" --hostCont -< ((fTy,code),x)

instance ArrowFix (Underlying (StaticGlobalStateT v c) x y) => ArrowFix (StaticGlobalStateT v c x y) where
    type Fix (StaticGlobalStateT v c x y) = Fix (Underlying (StaticGlobalStateT v c) x y)

deriving instance (ArrowComplete (StaticGlobalState v, y) c) => ArrowComplete y (StaticGlobalStateT v c)
