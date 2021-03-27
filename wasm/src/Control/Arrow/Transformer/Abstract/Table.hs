{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Transformer.Abstract.Table where

import           Abstract
import           Data
import           UnitAnalysisValue

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix hiding (filter)
import           Control.Arrow.GlobalState
import           Control.Arrow.Logger
import           Control.Arrow.MemAddress
import           Control.Arrow.Memory
import           Control.Arrow.MemSizable
import           Control.Arrow.Order
import           Control.Arrow.Reader
import           Control.Arrow.Serialize
import           Control.Arrow.Stack
import           Control.Arrow.State
import           Control.Arrow.StaticGlobalState
import           Control.Arrow.Store
import           Control.Arrow.Table
import           Control.Arrow.Trans
import           Control.Arrow.WasmFrame

import           Control.Arrow.Transformer.Reader

import           Control.Category (Category)

import           Data.Maybe (catMaybes,isNothing)
import           Data.Profunctor
import           Data.Vector ((!), toList)

import qualified Language.Wasm.Interpreter as Wasm

newtype TableT c x y = TableT (ReaderT Tables c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowState s, ArrowStaticGlobalState val,
              ArrowSerialize val dat valTy datDecTy datEncTy, ArrowJoin)

instance ArrowTrans TableT where
    -- lift' :: c x y -> MemoryT v c x y
    lift' a = TableT (lift' a)

instance (ArrowChoice c, Profunctor c) => ArrowTable Value (TableT c) where
    type JoinTable y (TableT c) = ArrowComplete y c
    readTable (TableT f) (TableT g) (TableT h) = TableT $ proc (ta,ix,x) -> do
        (JoinVector tabs) <- ask -< ()
        let (TableInst (Wasm.TableInstance _ tab)) = tabs ! ta
        let tabList = toList tab
        let funcs = catMaybes tabList
        if any isNothing tabList
            then (joinList1'' f -< (funcs,x)) <⊔> (g -< (ta,ix,x)) <⊔> (h -< (ta,ix,x))
            else (joinList1'' f -< (funcs,x)) <⊔> (g -< (ta,ix,x))

deriving instance (Arrow c, Profunctor c, ArrowComplete y c) => ArrowComplete y (TableT c)

instance (ArrowLift c, ArrowFix (Underlying (TableT c) x y)) => ArrowFix (TableT c x y) where
    type Fix (TableT c x y) = Fix (Underlying (TableT c) x y)
