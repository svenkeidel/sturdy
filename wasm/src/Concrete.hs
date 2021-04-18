{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Concrete where

import           Data()
import           Data.Hashable
import           Data.Text.Prettyprint.Doc
import           Data.Vector (Vector)
import           Data.Word

import           Language.Wasm.Interpreter (ModuleInstance)
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure hiding (exports, Const, Function, Expression, Instruction)

import           GHC.Generics

newtype Value = Value Wasm.Value deriving (Show, Eq)

type Memories = Vector MemInst
type Tables = Vector TableInst

int32 :: Word32 -> Value
int32 = Value . Wasm.VI32

int64 :: Word64 -> Value
int64 = Value . Wasm.VI64

float32 :: Float -> Value
float32 = Value . Wasm.VF32

float64 :: Double -> Value
float64 = Value . Wasm.VF64

--data DynamicGlobalState = DynamicGlobalState {
--    tableInstances :: Vector TableInst,
--    memInstances :: Vector MemInst
--} deriving (Show, Eq)

--data GlobalState v = GlobalState {
--    funcInstances :: Vector FuncInst,
--    tableInstances :: Vector TableInst,
--    memInstances :: Vector MemInst,
--    globalInstances :: Vector (GlobInst v)
--} deriving (Show, Eq)
--
--emptyGlobalState :: GlobalState v
--emptyGlobalState = GlobalState {
--    funcInstances = Vec.empty,
--    tableInstances = Vec.empty,
--    memInstances = Vec.empty,
--    globalInstances = Vec.empty
--}
--

--instance Hashable MemArg
--instance Hashable BitSize
--instance Hashable IUnOp
--instance Hashable IBinOp
--instance Hashable IRelOp
--instance Hashable FUnOp
--instance Hashable FBinOp
--instance Hashable FRelOp
--instance Hashable Wasm.ExportInstance
--deriving instance Generic Wasm.ExportInstance
--instance Hashable Wasm.ExternalValue
--deriving instance Generic Wasm.ExternalValue
--instance (Hashable v) => Hashable (Instruction v)
--instance Hashable ValueType
--instance Hashable Function
--instance Hashable FuncInst
--instance Hashable FuncType
--instance Hashable ModuleInstance
--deriving instance Generic ModuleInstance
instance Pretty ModuleInstance where pretty = viaShow
instance Hashable TableInst
instance Hashable Wasm.TableInstance
deriving instance Generic Wasm.TableInstance
instance Hashable Limit

newtype TableInst = TableInst Wasm.TableInstance deriving (Show,Eq,Generic)
data MemInst = MemInst (Maybe Word32) (Vector Word8) deriving (Show,Eq)

deriving instance Show Wasm.TableInstance
deriving instance Eq Wasm.TableInstance

