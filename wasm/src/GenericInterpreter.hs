{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- | A generic interpreter for WebAssembly.
-- | This implements the official specification https://webassembly.github.io/spec/.
module GenericInterpreter where

import Prelude hiding (Read, fail, log)

import           Concrete()
import           Data (Instruction(..), Function(..), LoadType(..), StoreType(..))

import           Control.Arrow
import           Control.Arrow.Except
import qualified Control.Arrow.Except as Exc
import           Control.Arrow.Fail as Fail
import           Control.Arrow.Fix
import           Control.Arrow.Functions
import           Control.Arrow.EffectiveAddress
import           Control.Arrow.JumpTypes
import           Control.Arrow.Memory as Mem
import           Control.Arrow.Serialize
import           Control.Arrow.Size
import           Control.Arrow.Stack
import           Control.Arrow.Table
import qualified Control.Arrow.Utils as Arr
import           Control.Arrow.Globals
import           Control.Arrow.WasmFrame

import           Data.Hashable
import           Data.Maybe(maybeToList)
import           Data.Profunctor
import           Data.Text.Lazy (Text)
import           Data.Vector hiding (length, (++))
import           Data.Word

import           Language.Wasm.Structure (ValueType(..), BitSize, IUnOp(..), IBinOp(..), IRelOp(..),
                                          FUnOp(..), FBinOp(..), FRelOp(..), MemArg(..), FuncType(..), ResultType, BlockType(..))
import           Language.Wasm.Interpreter (ModuleInstance(..), emptyModInstance, ExportInstance(..), ExternalValue(..))

import           Numeric.Natural (Natural)
import           Text.Printf

import           GHC.Generics
import           GHC.Exts

-- | the kind of exceptions that can be thrown
data Exc v = Jump Natural [v] | CallReturn [v] deriving (Show, Eq, Generic)

-- | unrecoverable errors
data Err = Trap String | InvocationError String deriving (Show, Eq, Generic)

instance Hashable v => Hashable (Exc v)
instance Hashable Err

class ArrowExcept exc c => IsException exc v c | c -> v where
    type family JoinExc y (c :: * -> * -> *) :: Constraint
    exception :: c (Exc v) exc
    handleException :: JoinExc y c => c (Exc v, x) y -> c (exc,x) y

-- | fail with a trap
trap :: (Fail.Join x c, ArrowFail Err c) => c String x
trap = proc s -> fail -< (Trap s)

-- | fail with an invocation error
invocationError :: (Fail.Join x c, ArrowFail Err c) => c String x
invocationError = proc s -> fail -< (InvocationError s)

-- | stores a frame's static data (return arity and module instance)
type FrameData = (Natural, ModuleInstance)

type ArrowWasmMemory addr bytes sz v c =
  ( ArrowMemory addr bytes sz c,
    ArrowEffectiveAddress v Natural addr c,
    ArrowSize v sz c,
    ArrowSerialize v bytes ValueType LoadType StoreType c,
    Show addr, Show bytes)

type ArrowStaticComponents v c =
  ( ArrowGlobals v c,
    ArrowFunctions c,
    ArrowStack v c,
    ArrowFrame FrameData v c,
    ArrowJumpTypes c)

type ArrowDynamicComponents v addr bytes sz exc e c =
  ( ArrowTable v c,
    ArrowWasmMemory addr bytes sz v c,
    IsVal v c,
    ArrowExcept exc c, IsException exc v c,
    ArrowFail Err c,
    ArrowFix (c [Instruction Natural] ()),
    ?fixpointAlgorithm :: FixpointAlgorithm (Fix (c [Instruction Natural] ())))


-- | the language interface
class Show v => IsVal v c | c -> v where
    type family JoinVal y (c :: * -> * -> *) :: Constraint

    i32const :: c Word32 v
    i64const :: c Word64 v
    f32const :: c Float v
    f64const :: c Double v
    iUnOp :: c (BitSize, IUnOp, v) v
    iBinOp :: JoinVal v c => c (BitSize, IBinOp, v, v) v
    i32eqz :: c v v
    i64eqz :: c v v
    iRelOp :: c (BitSize, IRelOp, v, v) v
    fUnOp :: c (BitSize, FUnOp, v) v
    fBinOp :: c (BitSize, FBinOp, v, v) v
    fRelOp :: c (BitSize, FRelOp, v, v) v
    i32WrapI64 :: c v v
    iTruncFU :: JoinVal v c => c (BitSize, BitSize, v) v
    iTruncFS :: JoinVal v c => c (BitSize, BitSize, v) v
    iTruncSatFU :: c (BitSize, BitSize, v) v
    iTruncSatFS :: c (BitSize, BitSize, v) v
    i64ExtendSI32 :: c v v
    i64ExtendUI32 :: c v v
    fConvertIU :: c (BitSize, BitSize, v) v
    fConvertIS :: c (BitSize, BitSize, v) v
    f32DemoteF64 :: c v v
    f64PromoteF32 :: c v v
    iReinterpretF :: c (BitSize, v) v
    fReinterpretI :: c (BitSize, v) v
    i32ifNeqz :: (JoinVal y c) => c x y -> c x y -> c (v, x) y
    -- | listLookup f g (v, xs, x):
    -- Looks up the `v`-th element in `xs` and passes it to `f`, or
    -- passes `x` to `g` if `v` is out of range of `xs`.
    listLookup :: (JoinVal y c) => c x y -> c x y -> c (v, [x], x) y
    ifHasType :: c x y -> c x y -> c (v, ValueType, x) y


-- | entry point to the generic interpreter
--
-- invokes the function with the given name and the arguments

-- the module instance comes from ArrowFrame
-- ArrowGlobalState and ArrowWasmMemory are properly initialized
-- argument Text: name of the function to execute
-- argument [v]: arguments going to be passed to the function
invokeExported ::
  ( ArrowChoice c,
    ArrowStaticComponents v c, ArrowDynamicComponents v addr bytes sz exc e c,
    JoinExc () c, Exc.Join () c,
    Mem.Join () c,
    JoinVal () c, JoinVal v c, Show v,
    Fail.Join [v] c,
    Fail.Join () c,
    Fail.Join v c,
    JoinTable () c)
  => c (Text, [v]) [v]
invokeExported = proc (funcName, args) -> do
  (_, modInst) <- frameData -< () -- get the module instance
  -- look for a function with name funcName in the function's exports
  case find (\(ExportInstance n _) -> n == funcName) (exports modInst) of
      -- if found -> invoke
      Just (ExportInstance _ (ExternFunction addr)) -> invokeExternal -< (addr, args)
      _ -> invocationError  -< printf "Function with name %s was not found in module's exports" (show funcName)

-- | invokes function with the given static index and the arguments
invokeExternal ::
  ( ArrowChoice c,
    ArrowStaticComponents v c, ArrowDynamicComponents v addr bytes sz exc e c,
    JoinExc () c,
    Mem.Join () c,
    JoinVal () c, JoinVal v c, Show v,
    Exc.Join () c,
    Fail.Join () c,
    Fail.Join v c,
    JoinTable () c)
  => c (Int, [v]) [v]
invokeExternal = proc (funcAddr, args) -> do
  funcData@(FuncType paramTys resultTys,_,_) <- readFunction -< funcAddr
  withCheckedType (withRootFrame (invoke eval)) -< (paramTys, args, (resultTys, Prelude.reverse args, funcData))
  where
    -- execute f with "dummy" frame
    withRootFrame f = proc (resultTys, args, x) -> do
      let rtLength = fromIntegral $ length resultTys
      inNewFrame
        (proc (rtLength, args, x) -> do
          pushn -< args -- push arguments to the stack
          f -< x -- execute function
          res <- popn -< rtLength -- pop result from the stack
          returnA -< Prelude.reverse res)
        -< ((rtLength, emptyModInstance), [], (rtLength, args, x))

    -- execute f if arguments match paramTys
    withCheckedType f = proc (paramTys, args, x) -> do
      if length paramTys /= length args
        then invocationError -< printf "Wrong number of arguments in external invocation. Expected %d but got %d" (length paramTys) (length args)
        else returnA -< ()
      Arr.zipWith
        (proc (arg, ty) ->
          ifHasType
            (arr $ const ())
            (proc (arg, ty) -> invocationError -< printf "Wrong argument type in external invocation. Expected %s but got %s" (show ty) (show arg))
            -< (arg, ty, (arg, ty)))
        -< (args, paramTys)
      f -< x

-- | evaluates the list of instructions in the context provided by arrow c
eval ::
  ( ArrowChoice c,
    ArrowStaticComponents v c, ArrowDynamicComponents v addr bytes sz exc e c,
    Fail.Join () c,
    JoinExc () c,
    Mem.Join () c,
    JoinVal () c, JoinVal v c, Show v,
    Exc.Join () c,
    JoinTable () c,
    Fail.Join v c)
  => c [Instruction Natural] ()
eval = fix $ \eval' -> proc is -> do
    case is of
      [] -> returnA -< ()
      i:is' | isNumericInst i -> do
        push <<< evalNumericInst -< i
        eval' -< is'
      i:is' | isVariableInst i -> do
        evalVariableInst -< i
        eval' -< is'
      i:is' | isParametricInst i -> do
        evalParametricInst -< i
        eval' -< is'
      i:is' | isControlInst i -> do
        evalControlInst eval' -< i
        eval' -< is'
      i:is' | isMemoryInst i -> do
        evalMemoryInst -< i
        eval' -< is'

evalControlInst ::
  ( ArrowChoice c, Profunctor c,
    ArrowStaticComponents v c,
    ArrowDynamicComponents v addr bytes sz exc e c,
    Fail.Join () c,
    JoinVal () c,
    JoinExc () c,
    Exc.Join () c,
    JoinTable () c)
  => c [Instruction Natural] () -> c (Instruction Natural) ()
evalControlInst eval' = proc i -> case i of
  Unreachable _ -> trap -< "Execution of unreachable instruction"
  Nop _ -> returnA -< ()
  Block bt is _ -> do
    (FuncType _paramTys resultTys) <- expandType -< bt
    --popn -< fromIntegral (length paramTys)
    label eval' eval' -< (resultTys, is, [])
  Loop bt is l -> do
    (FuncType paramTys _resultTys) <- expandType -< bt
    --pars <- popn -< fromIntegral (length paramTys)
    --label (proc (pars,instr) -> do
    --           pushn -< pars
    --           eval' -< instr)
    --      (proc (_,instr) -> eval' -< instr)
    --      -< (paramTys, (pars,is), (pars,[Loop bt is l]))
    label eval' eval' -< (paramTys, is, [Loop bt is l])
  If bt isNZero isZero _ -> do
    v <- pop -< ()
    (FuncType _paramTys resultTys) <- expandType -< bt
    --popn -< fromIntegral (length paramTys)
    i32ifNeqz
      (proc (rt, isNZero, _) -> label eval' eval' -< (rt, isNZero, []))
      (proc (rt, _, isZero) -> label eval' eval' -< (rt, isZero, []))
      -< (v, (resultTys, isNZero, isZero))
  Br ix _ -> branch -< ix
  BrIf ix _ -> do
    v <- pop -< ()
    i32ifNeqz
      branch
      (arr $ const ())
      -< (v, ix)
  BrTable ls ldefault _ -> do
    v <- pop -< ()
    listLookup branch branch -< (v, ls, ldefault)
  Return _ -> do
    (n,_) <- frameData -< ()
    vs <- popn -< n
    throw <<< exception -< CallReturn vs
  Call ix _ -> do
    (_, modInst) <- frameData -< ()
    let funcAddr = funcaddrs modInst ! fromIntegral ix
    invoke eval' <<< readFunction -< funcAddr
  CallIndirect ix _ -> do
    (_, modInst) <- frameData -< () -- get the current module instance
    let tableAddr = tableaddrs modInst ! 0 -- get address of table 0
    let ftExpect = funcTypes modInst ! fromIntegral ix -- get expected functype
    funcAddr <- pop -< ()
    readTable
      (invokeChecked eval')
      (proc (ta,ix,_) -> trap -< printf "Index %s out of bounds for table address %s" (show ix) (show ta))
      (proc (ta,ix,_) -> trap -< printf "Index %s uninitialized for table address %s" (show ix) (show ta))
      -< (tableAddr, funcAddr, ftExpect)


expandType :: (ArrowChoice c, ArrowStaticComponents v c) => c BlockType FuncType
expandType = proc bt -> case bt of
    Inline mVal -> returnA -< FuncType [] (maybeToList mVal)
    TypeIndex idx -> do
        (_, modInst) <- frameData -< ()
        let ft = funcTypes modInst ! fromIntegral idx
        returnA -< ft



invokeChecked ::
  ( ArrowChoice c,
    ArrowStaticComponents v c,
    IsVal v c, ArrowExcept exc c, IsException exc v c, JoinExc () c, Exc.Join () c, Fail.Join () c,
    ArrowFail Err c)
  => c [Instruction Natural] () -> c (Int, FuncType) ()
invokeChecked eval' = proc (addr, ftExpect) -> do
  funcData@(ftActual,_,_) <- readFunction -< addr
  withCheckedType (invoke eval') -< (ftActual, ftExpect, funcData)
  where
    withCheckedType f = proc (ftActual, ftExpect, x) ->
      if ftActual == ftExpect
      then f -< x
      else trap -< printf "Mismatched function type in indirect call. Expected %s, actual %s." (show ftExpect) (show ftActual)

-- | invoke function with code code within module instance funcModInst
--
--  the function execution can finish by different reasons:
--
--   * all instructions have been executed -> result are the top |resultTys| values on the stack
--   * the function calls return -> result are the top |resultTys| values on the stack
--   * the function produces a trap -> no result, trap is propagated
invoke ::
  ( ArrowChoice c, ArrowStack v c, ArrowJumpTypes c,
    IsVal v c, ArrowFrame FrameData v c, ArrowExcept exc c, IsException exc v c, Exc.Join y c,
    ArrowStack v c, JoinExc y c)
  => c [Instruction Natural] y -> c (FuncType, ModuleInstance, Function) y
invoke eval' = catch
    (proc (FuncType paramTys resultTys, funcModInst, Function _ localTys code) -> do
        vs <- popn -< fromIntegral $ length paramTys
        zeros <- Arr.map initLocal -< localTys
        let rtLength = fromIntegral $ length resultTys
        result <- inNewFrame (localNoJumpTypes $ localFreshStack $ label eval' eval') -< ((rtLength, funcModInst), Prelude.reverse vs ++ zeros, (resultTys, code, []))
        returnA -< result
        )
    (proc (_,e) -> handleException
                   (proc (exc,_) -> case exc of
                       CallReturn vs -> do
                           pushn -< vs
                           eval' -< []
                       --Trap _ -> throw <<< exception -< exc
                       Jump _ _ -> returnA -< error "invalid module: tried to jump through a function boundary")
                   -< (e,()))
  where
    initLocal :: (ArrowChoice c, IsVal v c) => c ValueType v
    initLocal = proc ty ->  case ty of
      I32 -> i32const -< 0
      I64 -> i64const -< 0
      F32 -> f32const -< 0
      F64 -> f64const -< 0

-- | jumps to the enclosing block with given index
branch :: (ArrowChoice c, ArrowExcept exc c, IsException exc v c, ArrowStack v c, ArrowJumpTypes c) => c Natural ()
branch = proc ix -> do
  rt <- jumpType -< ix
  vs <- popn -< fromIntegral $ length rt
  throw <<< exception -< Jump ix vs

-- | Introduces a branching point `g` that can be jumped to from within `f`.
-- When escalating jumps, all label-local operands must be popped from the stack.
-- This implementation assumes that ArrowExcept discards label-local operands in ArrowStack upon throw.
label :: (ArrowChoice c, ArrowExcept exc c, IsException exc v c, ArrowStack v c, ArrowJumpTypes c, Exc.Join z c,
          ArrowStack v c, Show v, JoinExc z c)
  => c x z -> c y z -> c (ResultType, x, y) z
label f g = catch
  -- after executing f without a break we expect |rt| results on top of the stack
  (proc (rt,x,_) -> do
    result <- withJumpType f -< (rt, x)
    returnA -< result
  )
  -- after a break the results are popped from the stack and passed back via exception e
  (proc ((_,_,y),e) -> handleException
                       (proc (exc,y) -> case exc of
                           Jump 0 vs -> do
                               pushn -< vs
                               g -< y
                           -- we expect all label-local operands are popped from the stack
                           Jump n vs -> throw <<< exception -< Jump (n-1) vs
                           _ -> throw <<< exception -< exc)
                       -< (e,y)
  )


evalParametricInst :: (ArrowChoice c, Profunctor c, ArrowStack v c, IsVal v c, JoinVal () c)
  => c (Instruction Natural) ()
evalParametricInst = proc i -> case i of
  Drop _ -> const () ^<< pop -< ()
  Select _ -> do
    v <- pop -< ()
    i32ifNeqz
      (const () ^<< pop)
      (proc () -> do
        (_,v2) <- pop2 -< ()
        push -< v2)
      -< (v, ())


evalMemoryInst ::
  ( ArrowChoice c,
    ArrowStaticComponents v c, ArrowDynamicComponents v addr bytes sz exc e c,
    Mem.Join () c, Fail.Join () c)
  => c (Instruction Natural) ()
evalMemoryInst = proc i -> case i of --withCurrentMemory $ proc (m,i) -> case i of
  I32Load (MemArg off _) _ -> load 4 L_I32 I32 -< off
  I64Load (MemArg off _) _ -> load 8 L_I64 I64 -< off
  F32Load (MemArg off _) _ -> load 4 L_F32 F32 -< off
  F64Load (MemArg off _) _ -> load 8 L_F64 F64 -< off
  I32Load8S (MemArg off _) _ -> load 1 L_I8S I32 -< off
  I32Load8U (MemArg off _) _ -> load 1 L_I8U I32 -< off
  I32Load16S (MemArg off _) _ -> load 2 L_I16S I32 -< off
  I32Load16U (MemArg off _) _ -> load 2 L_I16U I32 -< off
  I64Load8S (MemArg off _) _ -> load 1 L_I8S I64 -< off
  I64Load8U (MemArg off _) _ -> load 1 L_I8U I64 -< off
  I64Load16S (MemArg off _) _ -> load 2 L_I16S I64 -< off
  I64Load16U (MemArg off _) _ -> load 2 L_I16U I64 -< off
  I64Load32S (MemArg off _) _ -> load 4 L_I32S I64 -< off
  I64Load32U (MemArg off _) _ -> load 4 L_I32U I64 -< off
  I32Store (MemArg off _) _ -> store S_I32 I32 -< off
  I64Store (MemArg off _) _ -> store S_I64 I64 -< off
  F32Store (MemArg off _) _ -> store S_F32 F32 -< off
  F64Store (MemArg off _) _ -> store S_F64 F64 -< off
  I32Store8 (MemArg off _) _ -> store S_I8 I32 -< off
  I32Store16 (MemArg off _) _ -> store S_I16 I32 -< off
  I64Store8 (MemArg off _) _ -> store S_I8 I64 -< off
  I64Store16 (MemArg off _) _ -> store S_I16 I64 -< off
  I64Store32 (MemArg off _) _ -> store S_I32 I64 -< off
  CurrentMemory _ -> push <<< sizeToVal <<< memsize <<< memoryIndex -< ()
  GrowMemory _ -> do
    n <- valToSize <<< pop -< ()
    memIndex <- memoryIndex -< ()
    memgrow
      (push <<< sizeToVal <<^ fst)
      (proc _ -> push <<< i32const -< 0xFFFFFFFF) -- 0xFFFFFFFF ~= -1
      -< (memIndex, n, ())

memoryIndex :: (Arrow c, ArrowFrame FrameData v c) => c () Int
memoryIndex = proc () -> do
  (_,modInst) <- frameData -< ()
  returnA -< memaddrs modInst ! 0

-- | loads byteSize bytes from memory and converts it to valType, the resulting value is pushed to the stack
load ::
  ( ArrowChoice c,
    ArrowStaticComponents v c, ArrowDynamicComponents v addr bytes sz exc e c,
    Mem.Join () c, Fail.Join () c)
  => Int -> LoadType -> ValueType -> c Natural ()
load byteSize loadType valType = proc off -> do
  base <- pop -< ()
  addr <- effectiveAddress -< (base, off)
  memIndex <- memoryIndex -< ()
  memread
    (proc (bytes,_) ->
      push <<< decode -< (bytes, loadType, valType))
    (proc addr -> trap -< printf "Memory access out of bounds: Cannot read %d bytes at address %s in current memory" byteSize (show addr))
    -< (memIndex, addr, byteSize, addr)

-- | stores a value into the memory, value and memory address are read from the stack
store ::
  ( ArrowChoice c,
    ArrowStaticComponents v c, ArrowDynamicComponents v addr bytes sz exc e c,
    Mem.Join () c, Fail.Join () c)
  => StoreType -> ValueType -> c Natural ()
store storeType valType = proc off -> do
  v <- pop -< ()
  bytes <- encode -< (v, valType, storeType)
  base <- pop -< ()
  addr <- effectiveAddress -< (base, off)
  memIndex <- memoryIndex -< ()
  memstore
    (arr $ const ())
    (proc (addr,bytes) -> trap -< printf "Memory access out of bounds: Cannot write %s at address %s in current memory" (show bytes) (show addr))
    -< (memIndex, addr, bytes, (addr, bytes))

evalVariableInst ::
  ( ArrowChoice c, ArrowStaticComponents v c)
  => c (Instruction Natural) ()
evalVariableInst = proc i -> case i of
  GetLocal ix _ -> push <<< getLocal -< ix
  SetLocal ix _-> do
    v <- pop -< ()
    setLocal -< (ix, v)
  TeeLocal ix _ -> do
    v <- peek -< ()
    setLocal -< (ix, v)
  GetGlobal ix _ ->  do
    (_,modInst) <- frameData -< ()
    let globalAddr = globaladdrs modInst ! fromIntegral ix
    push <<< readGlobal -< globalAddr
  SetGlobal ix _ -> do
    v <- pop -< ()
    (_,modInst) <- frameData -< ()
    let globalAddr = globaladdrs modInst ! fromIntegral ix
    writeGlobal -< (globalAddr, v)


evalNumericInst ::
  ( ArrowChoice c, ArrowStack v c, ArrowExcept exc c, IsException exc v c, IsVal v c, Show v, JoinVal v c,
    Fail.Join v c, ArrowFail Err c)
  => c (Instruction Natural) v
evalNumericInst = proc i -> case i of
  I32Const lit _ -> i32const -< lit
  I64Const lit _ -> i64const -< lit
  F32Const lit _ -> f32const -< lit
  F64Const lit _ -> f64const -< lit
  IUnOp bs op _ -> do
    v <- pop -< ()
    iUnOp -< (bs, op, v)
  IBinOp bs op _ -> do
    (v1,v2) <- pop2 -< ()
    iBinOp -< (bs, op, v1, v2)
  I32Eqz _ -> do
    v <- pop -< ()
    i32eqz -< v
  I64Eqz _ -> do
    v <- pop -< ()
    i64eqz -< v
  IRelOp bs op _ -> do
    (v1,v2) <- pop2 -< ()
    iRelOp -< (bs, op, v1, v2)
  FBinOp bs op _ -> do
    (v1,v2) <- pop2 -< ()
    fBinOp -< (bs, op, v1, v2)
  FUnOp bs op _ -> do
    v <- pop -< ()
    fUnOp -< (bs, op, v)
  FRelOp bs op _ -> do
    (v1,v2) <- pop2 -< ()
    fRelOp -< (bs, op, v1, v2)
  I32WrapI64 _ -> do
    v <- pop -< ()
    i32WrapI64 -< v
  ITruncFU bs1 bs2 _ -> do
    v <- pop -< ()
    iTruncFU -< (bs1,bs2,v)
  ITruncFS bs1 bs2 _ -> do
    v <- pop -< ()
    iTruncFS -< (bs1,bs2,v)
  ITruncSatFU bs1 bs2 _ -> do
    v <- pop -< ()
    iTruncSatFU -< (bs1,bs2,v)
  ITruncSatFS bs1 bs2 _ -> do
    v <- pop -< ()
    iTruncSatFS -< (bs1,bs2,v)
  I64ExtendSI32 _ -> do
    v <- pop -< ()
    i64ExtendSI32 -< v
  I64ExtendUI32 _ -> do
    v <- pop -< ()
    i64ExtendUI32 -< v
  FConvertIU bs1 bs2 _ -> do
    v <- pop -< ()
    fConvertIU -< (bs1, bs2, v)
  FConvertIS bs1 bs2 _ -> do
    v <- pop -< ()
    fConvertIS -< (bs1, bs2, v)
  F32DemoteF64 _ -> do
    v <- pop -< ()
    f32DemoteF64 -< v
  F64PromoteF32 _ -> do
    v <- pop -< ()
    f64PromoteF32 -< v
  IReinterpretF bs _ -> do
    v <- pop -< ()
    iReinterpretF -< (bs, v)
  FReinterpretI bs _ -> do
    v <- pop -< ()
    fReinterpretI -< (bs, v)


isControlInst :: Instruction index -> Bool
isControlInst i = case i of
  Unreachable _ -> True
  Nop _ -> True
  Block{}-> True
  Loop{} -> True
  If{} -> True
  Br{} -> True
  BrIf{} -> True
  BrTable{} -> True
  Return _  -> True
  Call{} -> True
  CallIndirect{} -> True
  _ -> False

isParametricInst :: Instruction index -> Bool
isParametricInst i = case i of
  Drop _ -> True
  Select _ -> True
  _ -> False

isMemoryInst :: Instruction index -> Bool
isMemoryInst i = case i of
  I32Load _ _ -> True
  I64Load _ _ -> True
  F32Load _ _ -> True
  F64Load _ _ -> True
  I32Load8S _ _ -> True
  I32Load8U _ _ -> True
  I32Load16S _ _ -> True
  I32Load16U _ _ -> True
  I64Load8S _ _ -> True
  I64Load8U _ _ -> True
  I64Load16S _ _ -> True
  I64Load16U _ _ -> True
  I64Load32S _ _ -> True
  I64Load32U _ _ -> True
  I32Store _ _ -> True
  I64Store _ _ -> True
  F32Store _ _ -> True
  F64Store _ _ -> True
  I32Store8 _ _ -> True
  I32Store16 _ _ -> True
  I64Store8 _ _ -> True
  I64Store16 _ _ -> True
  I64Store32 _ _ -> True
  CurrentMemory _ -> True
  GrowMemory _ -> True
  _ -> False


isVariableInst :: Instruction index -> Bool
isVariableInst i = case i of
  GetLocal _ _ -> True
  SetLocal _ _ -> True
  TeeLocal _ _ -> True
  GetGlobal _ _ -> True
  SetGlobal _ _ -> True
  _ -> False

isNumericInst :: Instruction index -> Bool
isNumericInst i = case i of
  I32Const _ _ -> True
  I64Const _ _ -> True
  F32Const _ _ -> True
  F64Const _ _ -> True
  IUnOp _ _ _ -> True
  IBinOp _ _ _ -> True
  I32Eqz _ -> True
  I64Eqz _ -> True
  IRelOp _ _ _ -> True
  FUnOp _ _ _ -> True
  FBinOp _ _ _ -> True
  FRelOp _ _ _ -> True
  I32WrapI64 _ -> True
  ITruncFU _ _ _ -> True
  ITruncFS _ _ _ -> True
  ITruncSatFU _ _ _ -> True
  ITruncSatFS _ _ _ -> True
  I64ExtendSI32 _ -> True
  I64ExtendUI32 _ -> True
  FConvertIU  _ _ _ -> True
  FConvertIS _ _ _ -> True
  F32DemoteF64 _ -> True
  F64PromoteF32 _ -> True
  IReinterpretF _ _ -> True
  FReinterpretI _ _ -> True
  _ -> False

