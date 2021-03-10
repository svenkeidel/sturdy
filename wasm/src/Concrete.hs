{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Concrete where

import           Data.Abstract.FreeCompletion
import           Data.Order
import           Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vec
import           Data.Word

import           Language.Wasm.Interpreter (ModuleInstance)
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure hiding (exports, Const)

newtype Value = Value Wasm.Value deriving (Show, Eq)
data Mut = Const | Mutable deriving (Show, Eq)

data GlobalState v = GlobalState {
    funcInstances :: Vector FuncInst,
    tableInstances :: Vector TableInst,
    memInstances :: Vector MemInst,
    globalInstances :: Vector (GlobInst v)
} deriving (Show, Eq)

emptyGlobalState :: GlobalState v
emptyGlobalState = GlobalState {
    funcInstances = Vec.empty,
    tableInstances = Vec.empty,
    memInstances = Vec.empty,
    globalInstances = Vec.empty
}

data FuncInst =
    FuncInst {
        funcType :: FuncType,
        moduleInstance :: ModuleInstance,
        code :: Function
    }
    | HostInst {
        funcType :: FuncType
        --hostCode :: HostFunction v c
    } deriving (Show,Eq)

newtype TableInst = TableInst Wasm.TableInstance deriving (Show,Eq)
data MemInst = MemInst (Maybe Word32) (Vector Word8) deriving (Show,Eq)
data GlobInst v = GlobInst Mut v deriving (Show, Eq)

instance (PreOrd v) => PreOrd (GlobInst v) where
    (GlobInst m1 v1) ⊑ (GlobInst m2 v2) = m1 == m2 && v1 ⊑ v2

instance (Complete v) => Complete (FreeCompletion (GlobInst v)) where
    (Lower (GlobInst m1 v1)) ⊔ (Lower (GlobInst m2 v2))
        | m1 == m2 = Lower $ GlobInst m1 (v1 ⊔ v2)
        | otherwise = Top
    _ ⊔  _ = Top

deriving instance Show Wasm.TableInstance
deriving instance Eq Wasm.TableInstance

data LoadType = L_I32 | L_I64 | L_F32 | L_F64 | L_I8S | L_I8U | L_I16S | L_I16U | L_I32S | L_I32U
  deriving Show
data StoreType = S_I32 | S_I64 | S_F32 | S_F64 | S_I8 | S_I16
  deriving Show
