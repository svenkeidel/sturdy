{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Transformer.Concrete.Table where

import           Concrete

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.Serialize
import           Control.Arrow.Stack
import           Control.Arrow.State
import           Control.Arrow.StaticGlobalState
import           Control.Arrow.Store
import           Control.Arrow.Table
import           Control.Arrow.Trans
import           Control.Arrow.WasmFrame

import           Control.Arrow.Transformer.State

import           Control.Category

import           Data.Profunctor
import           Data.Vector as Vec

import qualified Language.Wasm.Interpreter as Wasm

newtype TableT c x y = TableT (StateT Tables c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowReader r, ArrowStaticGlobalState val,
              ArrowSerialize val dat valTy datDecTy datEncTy)

instance (Profunctor c, ArrowChoice c) => ArrowTable Value (TableT c) where
    type JoinTable y (TableT c) = ()
    readTable (TableT f) (TableT g) (TableT h) = TableT $ proc (ta,i@(Value (Wasm.VI32 ix)),x) -> do
        let ixI = fromIntegral ix
        tabs <- get -< ()
        let (TableInst (Wasm.TableInstance _ tab)) = tabs ! ta
        if Vec.length tab <= ixI
          then g -< (ta,i,x)
          else do
            let addr = tab ! ixI
            case addr of
                (Just a) -> f -< (a,x)
                Nothing -> h -< (ta,i,x)

instance ArrowTrans TableT where
    -- lift' :: c x y -> MemoryT v c x y
    lift' a = TableT (lift' a)

instance (ArrowLift c, ArrowFix (Underlying (TableT c) x y)) => ArrowFix (TableT c x y) where
    type Fix (TableT c x y) = Fix (Underlying (TableT c) x y)
