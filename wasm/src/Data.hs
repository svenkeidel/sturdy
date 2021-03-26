{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data where

import           Control.Monad.State

import           Data.Hashable
import           Data.IORef
import           Data.Label
import           Data.Order
import qualified Data.Primitive.ByteArray as ByteArray
import           Data.Text.Prettyprint.Doc
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import           Data.Word (Word8, Word32, Word64)

import           Numeric.Natural (Natural)

import           Language.Wasm.Interpreter (ModuleInstance,emptyStore,emptyImports)
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure (ResultType, MemArg, BitSize, IUnOp(..), IBinOp(..), IRelOp(..),
                                          FUnOp(..), FBinOp(..), FRelOp(..), FuncType, TypeIndex, LocalsType)
import qualified Language.Wasm.Structure as Wasm
import           Language.Wasm.Validate (ValidModule)

import           GHC.Generics (Generic)
import           GHC.Exts

instance (Show v) => Pretty (Vector v) where pretty = viaShow

instance (Hashable v) => Hashable (Vector v) where
    hashWithSalt salt v = hashWithSalt salt (Vec.toList v)

-- wrappers
newtype JoinVector v = JoinVector (Vector v) deriving (Show,Eq,Generic,Pretty)

instance (Hashable v) => Hashable (JoinVector v)
----    hashWithSalt salt (Vector v) = hashWithSalt salt (Vec.toList v)
--
--instance (Hashable v) => Hashable (Vec.Vector v) where
--    hashWithSalt salt v = hashWithSalt salt (Vec.toList v)

instance (PreOrd v) => PreOrd (JoinVector v) where
    (JoinVector v1) ⊑ (JoinVector v2) = all id $ Vec.zipWith (⊑) v1 v2

instance (Complete v) => Complete (JoinVector v) where
    (JoinVector v1) ⊔ (JoinVector v2) = JoinVector $ Vec.zipWith (⊔) v1 v2

newtype JoinList v = JoinList [v] deriving (Show,Eq,Generic,Pretty)

instance (Hashable v) => Hashable (JoinList v)

instance (PreOrd v) => PreOrd (JoinList v) where
    (JoinList v1) ⊑ (JoinList v2) = all id $ zipWith (⊑) v1 v2

instance (Complete v) => Complete (JoinList v) where
    (JoinList v1) ⊔ (JoinList v2) = JoinList $ zipWith (⊔) v1 v2

instance IsList (JoinList v) where
    type Item (JoinList v) = v
    fromList = JoinList
    toList (JoinList vs) = vs


-- static part of global state

data StaticGlobalState v = StaticGlobalState {
    funcInstances :: Vector FuncInst,
    globalInstances :: Vector (GlobInst v)
} deriving (Show,Eq,Generic)

instance (PreOrd v) => PreOrd (StaticGlobalState v) where
    (StaticGlobalState f1 g1) ⊑ (StaticGlobalState f2 g2)
        | f1 == f2 && Vec.length g1 == Vec.length g2 = Vec.all id $ Vec.zipWith (⊑) g1 g2

instance (Complete v) => Complete (StaticGlobalState v) where
    (StaticGlobalState f1 g1) ⊔ (StaticGlobalState f2 g2)
        | f1 == f2 && Vec.length g1 == Vec.length g2 = StaticGlobalState f1 (Vec.zipWith (⊔) g1 g2)

instance (Hashable v) => Hashable (StaticGlobalState v)

data GlobInst v = GlobInst Mut v deriving (Show, Eq, Generic)

instance (Hashable v) => Hashable (GlobInst v)

instance (PreOrd v) => PreOrd (GlobInst v) where
    (GlobInst m1 v1) ⊑ (GlobInst m2 v2)
        | m1 == m2 = v1 ⊑ v2

instance (Complete v) => Complete (GlobInst v) where
    (GlobInst m1 v1) ⊔ (GlobInst m2 v2)
        | m1 == m2 = GlobInst m1 (v1 ⊔ v2)

data Mut = Const | Mutable deriving (Show, Eq, Generic)
instance Hashable Mut

data FuncInst =
    FuncInst {
        funcType :: FuncType,
        moduleInstance :: ModuleInstance,
        code :: Function
    }
    | HostInst {
        funcType :: FuncType
        --hostCode :: HostFunction v c
    } deriving (Show,Eq, Generic)

data Function = Function {
    funcT :: TypeIndex,
    localTypes :: LocalsType,
    funcBody :: Expression
} deriving (Show, Eq, Generic)

instance Hashable Function
instance Hashable FuncInst

-- labeled expressions

type Expression = [Instruction Natural]

data Instruction index =
    -- Control instructions
    Unreachable Label
    | Nop Label
    | Block { resultType :: ResultType, body :: Expression, lbl :: Label }
    | Loop { resultType :: ResultType, body :: Expression, lbl :: Label }
    | If { resultType :: ResultType, true :: Expression, false :: Expression, lbl :: Label }
    | Br index Label
    | BrIf index Label
    | BrTable [index] index Label
    | Return Label
    | Call index Label
    | CallIndirect index Label
    -- Parametric instructions
    | Drop Label
    | Select Label
    -- Variable instructions
    | GetLocal index Label
    | SetLocal index Label
    | TeeLocal index Label
    | GetGlobal index Label
    | SetGlobal index Label
    -- Memory instructions
    | I32Load MemArg Label
    | I64Load MemArg Label
    | F32Load MemArg Label
    | F64Load MemArg Label
    | I32Load8S MemArg Label
    | I32Load8U MemArg Label
    | I32Load16S MemArg Label
    | I32Load16U MemArg Label
    | I64Load8S MemArg Label
    | I64Load8U MemArg Label
    | I64Load16S MemArg Label
    | I64Load16U MemArg Label
    | I64Load32S MemArg Label
    | I64Load32U MemArg Label
    | I32Store MemArg Label
    | I64Store MemArg Label
    | F32Store MemArg Label
    | F64Store MemArg Label
    | I32Store8 MemArg Label
    | I32Store16 MemArg Label
    | I64Store8 MemArg Label
    | I64Store16 MemArg Label
    | I64Store32 MemArg Label
    | CurrentMemory Label
    | GrowMemory Label
    -- Numeric instructions
    | I32Const Word32 Label
    | I64Const Word64 Label
    | F32Const Float Label
    | F64Const Double Label
    | IUnOp BitSize IUnOp Label
    | IBinOp BitSize IBinOp Label
    | I32Eqz Label
    | I64Eqz Label
    | IRelOp BitSize IRelOp Label
    | FUnOp BitSize FUnOp Label
    | FBinOp BitSize FBinOp Label
    | FRelOp BitSize FRelOp Label
    | I32WrapI64 Label
    | ITruncFU {- Int Size -} BitSize {- Float Size -} BitSize Label
    | ITruncFS {- Int Size -} BitSize {- Float Size -} BitSize Label
    | I64ExtendSI32 Label
    | I64ExtendUI32 Label
    | FConvertIU {- Float Size -} BitSize {- Int Size -} BitSize Label
    | FConvertIS {- Float Size -} BitSize {- Int Size -} BitSize Label
    | F32DemoteF64 Label
    | F64PromoteF32 Label
    | IReinterpretF BitSize Label
    | FReinterpretI BitSize Label
    deriving (Show, Eq, Generic)

type LInstruction = State Label (Instruction Natural)

unreachable :: LInstruction
unreachable = Unreachable <$> fresh
nop :: LInstruction
nop = Nop <$> fresh
block :: ResultType -> [LInstruction] -> LInstruction
block rt body = Block rt <$> sequence body <*> fresh
loop :: ResultType -> [LInstruction] -> LInstruction
loop rt body = Loop rt <$> sequence body <*> fresh
if_ :: ResultType -> [LInstruction] -> [LInstruction] -> LInstruction
if_ rt bTrue bFalse = If rt <$> sequence bTrue <*> sequence bFalse <*> fresh
br :: Natural -> LInstruction
br i = Br i <$> fresh
brIf :: Natural -> LInstruction
brIf i = BrIf i <$> fresh
brTable :: [Natural] -> Natural -> LInstruction
brTable is i = BrTable is i <$> fresh
return_ :: LInstruction
return_ = Return <$> fresh
call :: Natural -> LInstruction
call i = Call i <$> fresh
callIndirect :: Natural -> LInstruction
callIndirect i = CallIndirect i <$> fresh
drop_ :: LInstruction
drop_ = Drop <$> fresh
select :: LInstruction
select = Select <$> fresh
getLocal :: Natural -> LInstruction
getLocal i = GetLocal i <$> fresh
setLocal :: Natural -> LInstruction
setLocal i = SetLocal i <$> fresh
teeLocal :: Natural -> LInstruction
teeLocal i = TeeLocal i <$> fresh
getGlobal :: Natural -> LInstruction
getGlobal i = GetGlobal i <$> fresh
setGlobal :: Natural -> LInstruction
setGlobal i = SetGlobal i <$> fresh
i32Load :: MemArg  -> LInstruction
i32Load m = I32Load m <$> fresh
i64Load :: MemArg -> LInstruction
i64Load m = I64Load m <$> fresh
f32Load :: MemArg  -> LInstruction
f32Load m = F32Load m <$> fresh
f64Load :: MemArg  -> LInstruction
f64Load m = F64Load m <$> fresh

i32Store :: MemArg -> LInstruction
i32Store m = I32Store m <$> fresh
i64Store :: MemArg -> LInstruction
i64Store m = I64Store m <$> fresh
f32Store :: MemArg -> LInstruction
f32Store m = F32Store m <$> fresh
f64Store :: MemArg -> LInstruction
f64Store m = F64Store m <$> fresh

i32Const :: Word32 -> LInstruction
i32Const w32 = I32Const w32 <$> fresh
i64Const :: Word64 -> LInstruction
i64Const w64 = I64Const w64 <$> fresh
f32Const :: Float -> LInstruction
f32Const f = F32Const f <$> fresh
f64Const :: Double -> LInstruction
f64Const d = F64Const d <$> fresh
iUnOp :: BitSize -> IUnOp -> LInstruction
iUnOp bs op = IUnOp bs op <$> fresh
iBinOp :: BitSize -> IBinOp -> LInstruction
iBinOp bs op = IBinOp bs op <$> fresh
i32Eqz :: LInstruction
i32Eqz = I32Eqz <$> fresh
i64Eqz :: LInstruction
i64Eqz = I64Eqz <$> fresh
iRelOp :: BitSize -> IRelOp -> LInstruction
iRelOp bs op = IRelOp bs op <$> fresh
fUnOp :: BitSize -> FUnOp -> LInstruction
fUnOp bs op = FUnOp bs op <$> fresh
fBinOp :: BitSize -> FBinOp -> LInstruction
fBinOp bs op = FBinOp bs op <$> fresh
fRelOp :: BitSize -> FRelOp -> LInstruction
fRelOp bs op = FRelOp bs op <$> fresh
i32WrapI64 :: LInstruction
i32WrapI64 = I32WrapI64 <$> fresh
iTruncFU :: BitSize -> BitSize -> LInstruction
iTruncFU b1 b2 = ITruncFU b1 b2 <$> fresh
iTruncFS :: BitSize -> BitSize -> LInstruction
iTruncFS b1 b2 = ITruncFS b1 b2 <$> fresh
i64ExtendSI32 :: LInstruction
i64ExtendSI32 = I64ExtendSI32 <$> fresh
i64ExtendUI32 :: LInstruction
i64ExtendUI32 = I64ExtendUI32 <$> fresh


convertInstruction :: Wasm.Instruction Natural -> LInstruction
convertInstruction i = case i of
    Wasm.Unreachable -> unreachable
    Wasm.Nop -> nop
    Wasm.Block rt body -> block rt (map convertInstruction body)
    Wasm.Loop rt body -> loop rt (map convertInstruction body)
    Wasm.If rt t f -> if_ rt (map convertInstruction t) (map convertInstruction f)
    Wasm.Br i -> br i
    Wasm.BrIf i -> brIf i
    Wasm.BrTable is i -> brTable is i
    Wasm.Return -> return_
    Wasm.Call i -> call i
    Wasm.Drop -> drop_
    Wasm.Select -> select
    Wasm.GetLocal i -> getLocal i
    Wasm.SetLocal i -> setLocal i
    Wasm.TeeLocal i -> teeLocal i
    Wasm.GetGlobal i -> getGlobal i
    Wasm.SetGlobal i -> setGlobal i

    Wasm.I32Load m -> i32Load m
    Wasm.I64Load m -> i64Load m
    Wasm.F32Load m -> f32Load m
    Wasm.F64Load m -> f64Load m

    Wasm.I32Store m -> i32Store m
    Wasm.I64Store m -> i64Store m
    Wasm.F32Store m -> f32Store m
    Wasm.F64Store m -> f64Store m

    Wasm.I32Const w32 -> i32Const w32
    Wasm.I64Const w64 -> i64Const w64
    Wasm.F32Const f -> f32Const f
    Wasm.F64Const d -> f64Const d
    Wasm.IUnOp bs op -> iUnOp bs op
    Wasm.IBinOp bs op -> iBinOp bs op
    Wasm.I32Eqz -> i32Eqz
    Wasm.I64Eqz -> i64Eqz
    Wasm.IRelOp bs op -> iRelOp bs op
    Wasm.FUnOp bs op -> fUnOp bs op
    Wasm.FBinOp bs op -> fBinOp bs op
    Wasm.FRelOp bs op -> fRelOp bs op

convertExpr :: Wasm.Expression -> [LInstruction]
convertExpr = map convertInstruction

convertFunc :: Wasm.Function -> State Label Function
convertFunc (Wasm.Function ft lt bd) = Function ft lt <$> sequence (convertExpr bd)

convertFuncInst :: Wasm.FunctionInstance -> State Label FuncInst
convertFuncInst (Wasm.FunctionInstance t m c) = FuncInst t m <$> convertFunc c

-- instatiation of modules

instantiate :: ValidModule
                -> (Wasm.Value -> v)
                -> (Maybe Word32 -> [Word8] -> m)
                -> (Wasm.TableInstance -> t)
                -> IO (Either String (ModuleInstance, StaticGlobalState v, Vector m, Vector t))
instantiate valMod alpha toMem toTable = do
    res <- Wasm.instantiate emptyStore emptyImports valMod
    case res of
        Right (modInst, store) -> do
            (staticState,tables,mems) <- storeToGlobalState store
            return $ Right $ (modInst, staticState, tables, mems)
        Left e                 -> return $ Left e

    where
        storeToGlobalState (Wasm.Store funcI tableI memI globalI) = do
            let funcs = generate $ Vec.mapM convertFuncInst funcI
            mems <- Vec.mapM convertMem memI
            globs <- Vec.mapM convertGlobal globalI
            return $ (StaticGlobalState funcs globs,
                      mems,
                      Vec.map toTable tableI)

        convertMem (Wasm.MemoryInstance (Wasm.Limit _ n) mem) = do
            memStore <- readIORef mem
            size <- ByteArray.getSizeofMutableByteArray memStore
            list <- sequence $ map (\x -> ByteArray.readByteArray memStore x) [0 .. (size-1)]
            let sizeConverted = fmap fromIntegral n
            return $ toMem sizeConverted list
            --return $ MemInst sizeConverted $ Vec.fromList list

        convertGlobal (Wasm.GIConst _ v) =  return $ GlobInst Const (alpha v)
        convertGlobal (Wasm.GIMut _ v) = do
            val <- readIORef v
            return $ GlobInst Mutable (alpha val)


data LoadType = L_I32 | L_I64 | L_F32 | L_F64 | L_I8S | L_I8U | L_I16S | L_I16U | L_I32S | L_I32U
  deriving Show
data StoreType = S_I32 | S_I64 | S_F32 | S_F64 | S_I8 | S_I16
  deriving Show

-- orphan instances

instance (Hashable v) => Hashable (Instruction v)
instance Hashable FuncType
instance Hashable Wasm.ValueType
instance Hashable ModuleInstance
instance Hashable MemArg
instance Hashable BitSize
instance Hashable Wasm.ExportInstance
instance Hashable IUnOp
instance Hashable IBinOp
instance Hashable IRelOp
instance Hashable FUnOp
instance Hashable FBinOp
instance Hashable FRelOp
instance Hashable Wasm.ExternalValue


deriving instance Generic ModuleInstance
deriving instance Generic Wasm.ExportInstance
deriving instance Generic Wasm.ExternalValue
