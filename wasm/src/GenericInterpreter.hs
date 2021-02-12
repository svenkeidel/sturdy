{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- | A generic interpreter for WebAssembly.
-- | This implements the official specification https://webassembly.github.io/spec/.
module GenericInterpreter where

import Prelude hiding (Read, fail)

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import qualified Control.Arrow.Except as Exc
import           Control.Arrow.Fail as Fail
import           Control.Arrow.Fix
import           Control.Arrow.Frame
import           Control.Arrow.MemAddress
import           Control.Arrow.Memory
import           Control.Arrow.MemSizable
import           Control.Arrow.Reader
import           Control.Arrow.Serialize
import           Control.Arrow.Stack
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Arrow.Trans
import qualified Control.Arrow.Utils as Arr
import           Control.Arrow.WasmStore

import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Kleisli
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.Stack
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Value

import           Control.Category

import           Data.Monoidal (shuffle1)
import           Data.Profunctor
import           Data.Text.Lazy (Text)
import           Data.Vector hiding (length, (++))
import           Data.Word

import           Language.Wasm.Structure hiding (exports)
import           Language.Wasm.Interpreter (ModuleInstance(..), emptyModInstance, ExportInstance(..), ExternalValue(..))

import           Numeric.Natural (Natural)
import           Text.Printf

-- the kind of exceptions that can be thrown
data Exc v = Trap String | Jump Natural [v] | CallReturn [v] deriving (Show, Eq)

-- used for storing the return "arity" of nested labels
newtype LabelArities = LabelArities {labels :: [Natural]}

-- stores a frame's static data (return arity and module instance)
type FrameData = (Natural, ModuleInstance)

---- constraints to support (and call) host functions
--type HostFunctionSupport addr bytes v c = (ArrowApply c, ArrowWasmStore v c, ArrowWasmMemory addr bytes v c)
---- a host function is a function from a list of values (parameters) to a list of values (return values)
--newtype HostFunction v c = HostFunction (
--  forall addr bytes. HostFunctionSupport addr bytes v c => (c [v] [v]) )
--
--instance Show (HostFunction v c) where
--    show _ = "HostFunction"





type ArrowWasmMemory addr bytes v c =
  ( ArrowMemory addr bytes c,
    ArrowMemAddress v Natural addr c,
    ArrowSerialize v bytes ValueType LoadType StoreType c,
    ArrowMemSizable v c,
    Show addr, Show bytes)







data LoadType = L_I32 | L_I64 | L_F32 | L_F64 | L_I8S | L_I8U | L_I16S | L_I16U | L_I32S | L_I32U
  deriving Show
data StoreType = S_I32 | S_I64 | S_F32 | S_F64 | S_I8 | S_I16
  deriving Show

class Show v => IsVal v c | c -> v where
  i32const :: c Word32 v
  i64const :: c Word64 v
  f32const :: c Float v
  f64const :: c Double v
  iUnOp :: c (BitSize, IUnOp, v) v
  iBinOp :: c (BitSize, IBinOp, v, v) (Maybe v)
  i32eqz :: c v v
  i64eqz :: c v v
  iRelOp :: c (BitSize, IRelOp, v, v) v
  fUnOp :: c (BitSize, FUnOp, v) v
  fBinOp :: c (BitSize, FBinOp, v, v) v
  fRelOp :: c (BitSize, FRelOp, v, v) v
  i32WrapI64 :: c v v
  iTruncFU :: c (BitSize, BitSize, v) (Maybe v)
  iTruncFS :: c (BitSize, BitSize, v) (Maybe v)
  i64ExtendSI32 :: c v v
  i64ExtendUI32 :: c v v
  fConvertIU :: c (BitSize, BitSize, v) v
  fConvertIS :: c (BitSize, BitSize, v) v
  f32DemoteF64 :: c v v
  f64PromoteF32 :: c v v
  iReinterpretF :: c (BitSize, v) v
  fReinterpretI :: c (BitSize, v) v
  i32ifNeqz :: c x y -> c x y -> c (v, x) y
  -- | `listLookup f g (v, xs, x)`
  -- | Looks up the `v`-th element in `xs` and passes it to `f`, or
  -- | passes `x` to `g` if `v` is out of range of `xs`.
  listLookup :: c x y -> c x y -> c (v, [x], x) y
  ifHasType :: c x y -> c x y -> c (v, ValueType, x) y


-- entry point to the generic interpreter
-- the module instance comes from ArrowFrame
-- ArrowWasmStore and ArrowWasmMemory are properly initialized
-- argument Text: name of the function to execute
-- argument [v]: arguments going to be passed to the function
invokeExported ::
  ( ArrowChoice c, ArrowFrame FrameData v c, ArrowWasmStore v c,
    ArrowStack v c, ArrowExcept (Exc v) c, ArrowReader LabelArities c,
    ArrowWasmMemory addr bytes v c, --HostFunctionSupport addr bytes v c,
    IsVal v c, Show v,
    Exc.Join () c,
    Fail.Join [v] c,
    Fail.Join () c,
    ArrowFail String c,
    ArrowFix (c [Instruction Natural] ()),
    ?fixpointAlgorithm :: FixpointAlgorithm (Fix (c [Instruction Natural] ()))
    )
  => c (Text, [v]) [v]
invokeExported = proc (funcName, args) -> do
  (_, modInst) <- frameData -< () -- get the module instance
  -- look for a function with name funcName in the function's exports
  case find (\(ExportInstance n _) -> n == funcName) (exports modInst) of
      -- if found -> invoke
      Just (ExportInstance _ (ExternFunction addr)) -> invokeExternal -< (addr, args)
      _ -> fail -< printf "Function with name %s was not found in module's exports" (show funcName)

invokeExternal ::
  ( ArrowChoice c, ArrowFrame FrameData v c, ArrowWasmStore v c,
    ArrowStack v c, ArrowExcept (Exc v) c, ArrowReader LabelArities c,
    ArrowWasmMemory addr bytes v c, --HostFunctionSupport addr bytes v c,
    IsVal v c, Show v,
    Exc.Join () c,
    Fail.Join () c,
    ArrowFail String c,
    ArrowFix (c [Instruction Natural] ()),
    ?fixpointAlgorithm :: FixpointAlgorithm (Fix (c [Instruction Natural] ()))
    )
  => c (Int, [v]) [v]
invokeExternal = proc (funcAddr, args) ->
  readFunction
    -- func: function at address funcAddr in the store
    -- function type: paramTys -> resultTys
    (proc (func@(FuncType paramTys resultTys, _, _), args) ->
      withCheckedType (withRootFrame (invoke eval)) -< (paramTys, args, (resultTys, args, func)))
    --(proc (func@(FuncType paramTys resultTys, _), args) ->
    --  withCheckedType (withRootFrame invokeHost) -< (paramTys, args, (resultTys, args, func)))
    -< (funcAddr, args)
  where
    -- execute f with "dummy" frame
    withRootFrame f = proc (resultTys, args, x) -> do
      let rtLength = fromIntegral $ length resultTys
      inNewFrame
        (proc (rtLength, args, x) -> do
          pushn -< args -- push arguments to the stack
          f -< x -- execute function
          popn -< rtLength) -- pop result from the stack
        -< ((rtLength, emptyModInstance), [], (rtLength, args, x))

    -- execute f if arguments match paramTys
    withCheckedType f = proc (paramTys, args, x) -> do
      if length paramTys /= length args
        then fail -< printf "Wrong number of arguments in external invocation. Expected %d but got %d" (length paramTys) (length args)
        else returnA -< ()
      Arr.zipWith
        (proc (arg, ty) ->
          ifHasType
            (arr $ const ())
            (proc (arg, ty) -> fail -< printf "Wrong argument type in external invocation. Expected %s but got %s" (show ty) (show arg))
            -< (arg, ty, (arg, ty)))
        -< (args, paramTys)
      f -< x


eval ::
  ( ArrowChoice c, ArrowFrame FrameData v c, ArrowWasmStore v c,
    ArrowStack v c, ArrowExcept (Exc v) c, ArrowReader LabelArities c,
    ArrowWasmMemory addr bytes v c, --HostFunctionSupport addr bytes v c,
    IsVal v c, Show v,
    Exc.Join () c,
    ArrowFix (c [Instruction Natural] ()),
    ?fixpointAlgorithm :: FixpointAlgorithm (Fix (c [Instruction Natural] ()))
    )
  => c [Instruction Natural] ()
eval = fix $ \eval' -> proc is -> case is of
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
    ArrowStack v c, -- operand stack of computation
    IsVal v c, -- needs to support value operations
    ArrowExcept (Exc v) c,
    ArrowReader LabelArities c, -- return arity of nested labels
    ArrowFrame FrameData v c, -- frame data and local variables
    ArrowWasmStore v c,
    --HostFunctionSupport addr bytes v c,
    Exc.Join () c)
  => c [Instruction Natural] () -> c (Instruction Natural) ()
evalControlInst eval' = proc i -> case i of
  Unreachable -> throw -< Trap "Execution of unreachable instruction"
  Nop -> returnA -< ()
  Block rt is -> label eval' eval' -< (rt, is, [])
  Loop rt is -> label eval' eval' -< (rt, is, [Loop rt is])
  If rt isNZero isZero -> do
    v <- pop -< ()
    i32ifNeqz
      (proc (rt, isNZero, _) -> label eval' eval' -< (rt, isNZero, []))
      (proc (rt, _, isZero) -> label eval' eval' -< (rt, isZero, []))
      -< (v, (rt, isNZero, isZero))
  Br ix -> branch -< ix
  BrIf ix -> do
    v <- pop -< ()
    i32ifNeqz
      branch
      (arr $ const ())
      -< (v, ix)
  BrTable ls ldefault -> do
    v <- pop -< ()
    listLookup branch branch -< (v, ls, ldefault)
  Return -> do
    (n,_) <- frameData -< ()
    vs <- popn -< n
    throw -< CallReturn vs
  Call ix -> do
    (_, modInst) <- frameData -< ()
    let funcAddr = funcaddrs modInst ! fromIntegral ix
    readFunction (invoke eval' <<^ fst) -< (funcAddr, ()) --(invokeHost <<^ fst) -< (funcAddr, ())
  CallIndirect ix -> do
    (_, modInst) <- frameData -< ()
    let tableAddr = tableaddrs modInst ! 0
    let ftExpect = funcTypes modInst ! fromIntegral ix
    readTable
      (invokeChecked eval')
      (proc (ta,ix,_) -> throw -< Trap $ printf "Index %s out of bounds for table address %s" (show ix) (show ta))
      (proc (ta,ix,_) -> throw -< Trap $ printf "Index %s uninitialized for table address %s" (show ix) (show ta))
      -< (tableAddr, fromIntegral ix, ftExpect)

invokeChecked ::
  ( ArrowChoice c, ArrowWasmStore v c, ArrowStack v c, ArrowReader LabelArities c,
    IsVal v c, ArrowFrame FrameData v c, ArrowExcept (Exc v) c, Exc.Join () c)
    --HostFunctionSupport addr bytes v c)
  => c [Instruction Natural] () -> c (Int, FuncType) ()
invokeChecked eval' = proc (addr, ftExpect) ->
  readFunction
    (proc (f@(ftActual, _, _), ftExpect) ->
       withCheckedType (invoke eval') -< (ftActual, ftExpect, f))
    --(proc (f@(ftActual, _), ftExpect) ->
    --   withCheckedType invokeHost -< (ftActual, ftExpect, f))
    -< (addr, ftExpect)
  where
    withCheckedType f = proc (ftActual, ftExpect, x) ->
      if ftActual == ftExpect
      then f -< x
      else throw -< Trap $ printf "Mismatched function type in indirect call. Expected %s, actual %s." (show ftExpect) (show ftActual)

-- invoke function with code code within module instance funcModInst
-- the function execution can finish by different reasons:
--   - all instructions have been executed -> result are the top |resultTys| values on the stack
--   - the function calls return -> result are the top |resultTys| values on the stack
--   - the function produces a trap -> no result, trap is propagated
--   - TODO: what about break? Can we "break" to a function boundary? -> NO, only to block boundaries
invoke ::
  ( ArrowChoice c, ArrowStack v c, ArrowReader LabelArities c,
    IsVal v c, ArrowFrame FrameData v c, ArrowExcept (Exc v) c, Exc.Join y c)
  => c [Instruction Natural] y -> c (FuncType, ModuleInstance, Function) y
invoke eval' = catch
    (proc (FuncType paramTys resultTys, funcModInst, Function _ localTys code) -> do
        vs <- popn -< fromIntegral $ length paramTys
        zeros <- Arr.map initLocal -< localTys
        let rtLength = fromIntegral $ length resultTys
        -- TODO: removed localFreshStack, not sure if that is what we want
        inNewFrame (localNoLabels $ label eval' eval') -< ((rtLength, funcModInst), vs ++ zeros, (resultTys, code, [])))
    (proc (_,e) -> case e of
        CallReturn vs -> do
            pushn -< vs
            eval' -< []
        Trap _ -> throw -< e
        Jump _ _ -> returnA -< error "invalid module: tried to jump through a function boundary")
        
  where
    initLocal :: (ArrowChoice c, IsVal v c) => c ValueType v
    initLocal = proc ty ->  case ty of
      I32 -> i32const -< 0
      I64 -> i64const -< 0
      F32 -> f32const -< 0
      F64 -> f64const -< 0

--invokeHost ::
--  (ArrowChoice c, ArrowStack v c, HostFunctionSupport addr bytes v c)
--  => c (FuncType, HostFunction v c) ()
--invokeHost = proc (FuncType paramTys _, HostFunction hostFunc) -> do
--  vs <- popn -< fromIntegral $ length paramTys
--  pushn <<< app -< (hostFunc, vs)


branch :: (ArrowChoice c, ArrowExcept (Exc v) c, ArrowStack v c, ArrowReader LabelArities c) => c Natural ()
branch = proc ix -> do
  LabelArities{labels=ls} <- ask -< ()
  vs <- popn -< ls !! fromIntegral ix
  throw -< Jump ix vs

-- | Introduces a branching point `g` that can be jumped to from within `f`.
-- | When escalating jumps, all label-local operands must be popped from the stack.
-- | This implementation assumes that ArrowExcept discards label-local operands in ArrowStack upon throw.
label :: (ArrowChoice c, ArrowExcept (Exc v) c, ArrowStack v c, ArrowReader LabelArities c, Exc.Join z c)
  => c x z -> c y z -> c (ResultType, x, y) z
-- x: code to execute
-- y: continuation to execute after a break to the current label
label f g = catch
  -- after executing f without a break we expect |rt| results on top of the stack
  (proc (rt,x,_) -> localLabel f -< (rt, x))
  -- after a break the results are popped from the stack and passed back via exception e
  (proc ((_,_,y),e) -> case e of
    Jump 0 vs -> do
      pushn -< vs
      g -< y
    -- we expect all label-local operands are popped from the stack
    Jump n vs -> throw -< Jump (n-1) vs
    _ -> throw -< e
  )

localLabel :: (ArrowReader LabelArities c) => c x y -> c (ResultType, x) y
localLabel f = proc (rt, x) -> do
  r@LabelArities{labels=ls} <- ask -< ()
  let l = fromIntegral $ length rt
  local f -< (r{labels=l:ls}, x)

-- reset all label arities locally and execute f
localNoLabels :: (ArrowReader LabelArities c) => c x y -> c x y
localNoLabels f = proc x -> do
  --r <- ask -< ()
  local f -< (LabelArities{labels=[]}, x)

evalParametricInst :: (ArrowChoice c, Profunctor c, ArrowStack v c, IsVal v c)
  => c (Instruction Natural) ()
evalParametricInst = proc i -> case i of
  Drop -> const () ^<< pop -< ()
  Select -> do
    v <- pop -< ()
    i32ifNeqz
      (const () ^<< pop)
      (proc () -> do
        (_,v2) <- pop2 -< ()
        push -< v2)
      -< (v, ())


evalMemoryInst ::
  ( ArrowChoice c,
    ArrowWasmMemory addr bytes v c,
    ArrowWasmStore v c, ArrowFrame FrameData v c, ArrowStack v c, IsVal v c, ArrowExcept (Exc v) c)
  => c (Instruction Natural) ()
evalMemoryInst = withCurrentMemory $ proc i -> case i of
  I32Load (MemArg off _) -> load 4 L_I32 I32 -< off
  I64Load (MemArg off _) -> load 8 L_I64 I64 -< off
  F32Load (MemArg off _) -> load 4 L_F32 F32 -< off
  F64Load (MemArg off _) -> load 8 L_F64 F64 -< off
  I32Load8S (MemArg off _) -> load 1 L_I8S I32 -< off
  I32Load8U (MemArg off _) -> load 1 L_I8U I32 -< off
  I32Load16S (MemArg off _) -> load 2 L_I16S I32 -< off
  I32Load16U (MemArg off _) -> load 2 L_I16U I32 -< off
  I64Load8S (MemArg off _) -> load 1 L_I8S I64 -< off
  I64Load8U (MemArg off _) -> load 1 L_I8U I64 -< off
  I64Load16S (MemArg off _) -> load 2 L_I16S I64 -< off
  I64Load16U (MemArg off _) -> load 2 L_I16U I64 -< off
  I64Load32S (MemArg off _) -> load 4 L_I32S I64 -< off
  I64Load32U (MemArg off _) -> load 4 L_I32U I64 -< off
  I32Store (MemArg off _) -> store S_I32 I32 -< off
  I64Store (MemArg off _) -> store S_I64 I64 -< off
  F32Store (MemArg off _) -> store S_F32 F32 -< off
  F64Store (MemArg off _) -> store S_F64 F64 -< off
  I32Store8 (MemArg off _) -> store S_I8 I32 -< off
  I32Store16 (MemArg off _) -> store S_I16 I32 -< off
  I64Store8 (MemArg off _) -> store S_I8 I64 -< off
  I64Store16 (MemArg off _) -> store S_I16 I64 -< off
  I64Store32 (MemArg off _) -> store S_I32 I64 -< off
  CurrentMemory -> push <<< memsize -< ()
  GrowMemory -> do
    n <- pop -< ()
    memgrow
      (push <<^ fst)
      (proc _ -> push <<< i32const -< 0xFFFFFFFF) -- 0xFFFFFFFF ~= -1
      -< (n, ())

withCurrentMemory :: (ArrowChoice c, ArrowWasmStore v c, ArrowFrame FrameData v c) => c x y -> c x y
withCurrentMemory f = proc x -> do
  (_,modInst) <- frameData -< ()
  let memAddr = memaddrs modInst ! 0
  withMemoryInstance f -< (memAddr, x)

load ::
  ( ArrowChoice c,
    ArrowWasmMemory addr bytes v c,
    ArrowWasmStore v c, ArrowFrame FrameData v c, ArrowStack v c, IsVal v c, ArrowExcept (Exc v) c)
  => Int -> LoadType -> ValueType -> c Natural ()
load byteSize loadType valType = proc off -> do
  base <- pop -< ()
  addr <- memaddr -< (base, off)
  memread
    (proc (bytes,_) ->
      decode
        (push <<^ fst)
        (error "decode failure")
        -< (bytes, loadType, valType, ()))
    (proc addr -> throw -< Trap $ printf "Memory access out of bounds: Cannot read %d bytes at address %s in current memory" byteSize (show addr))
    -< (addr, byteSize, addr)

store ::
  ( ArrowChoice c,
    ArrowWasmMemory addr bytes v c,
    ArrowWasmStore v c, ArrowFrame FrameData v c, ArrowStack v c, IsVal v c, ArrowExcept (Exc v) c)
  => StoreType -> ValueType -> c Natural ()
store storeType valType = proc off -> do
  v <- pop -< ()
  encode
    (proc (bytes,off) -> do
      base <- pop -< ()
      addr <- memaddr -< (base, off)
      memstore
        (arr $ const ())
        (proc (addr,bytes) -> throw -< Trap $ printf "Memory access out of bounds: Cannot write %s at address %s in current memory" (show bytes) (show addr))
        -< (addr, bytes, (addr, bytes)))
    (error "encode failure")
    -< (v, valType, storeType, off)

evalVariableInst ::
  ( ArrowChoice c, ArrowFrame FrameData v c, ArrowWasmStore v c,
    ArrowStack v c)
  => c (Instruction Natural) ()
evalVariableInst = proc i -> case i of
  GetLocal ix -> push <<< frameLookup -< ix
  SetLocal ix -> do
    v <- pop -< ()
    frameUpdate -< (ix, v)
  TeeLocal ix -> do
    v <- peek -< ()
    frameUpdate -< (ix, v)
  GetGlobal ix ->  do
    (_,modInst) <- frameData -< ()
    let globalAddr = globaladdrs modInst ! fromIntegral ix
    push <<< readGlobal -< globalAddr
  SetGlobal ix -> do
    v <- pop -< ()
    (_,modInst) <- frameData -< ()
    let globalAddr = globaladdrs modInst ! fromIntegral ix
    writeGlobal -< (globalAddr, v)


evalNumericInst ::
  ( ArrowChoice c, ArrowStack v c, ArrowExcept (Exc v) c, IsVal v c, Show v)
  => c (Instruction Natural) v
evalNumericInst = proc i -> case i of
  I32Const lit -> i32const -< lit
  I64Const lit -> i64const -< lit
  F32Const lit -> f32const -< lit
  F64Const lit -> f64const -< lit
  IUnOp bs op -> do
    v <- pop -< ()
    iUnOp -< (bs, op, v)
  IBinOp bs op -> do
    (v1,v2) <- pop2 -< ()
    res <- iBinOp -< (bs, op, v1, v2)
    case res of
      Just v' -> returnA -< v'
      Nothing -> throw -< Trap $ printf "Binary operator %s failed on %s" (show op) (show (v1,v2))
  I32Eqz -> do
    v <- pop -< ()
    i32eqz -< v
  I64Eqz -> do
    v <- pop -< ()
    i64eqz -< v
  IRelOp bs op -> do
    (v1,v2) <- pop2 -< ()
    iRelOp -< (bs, op, v1, v2)
  FBinOp bs op -> do
    (v1,v2) <- pop2 -< ()
    fBinOp -< (bs, op, v1, v2)
  FUnOp bs op -> do
    v <- pop -< ()
    fUnOp -< (bs, op, v)
  FRelOp bs op -> do
    (v1,v2) <- pop2 -< ()
    fRelOp -< (bs, op, v1, v2)
  I32WrapI64 -> do
    v <- pop -< ()
    i32WrapI64 -< v
  ITruncFU bs1 bs2 -> do
    v <- pop -< ()
    res <- iTruncFU -< (bs1, bs2, v)
    case res of
      Just v' -> returnA -< v'
      Nothing -> throw -< Trap $ printf "Truncation operator from %s to %s failed on %s" (show bs1) (show bs2) (show v)
  ITruncFS bs1 bs2 -> do
    v <- pop -< ()
    res <- iTruncFS -< (bs1, bs2, v)
    case res of
      Just v' -> returnA -< v'
      Nothing -> throw -< Trap $ printf "Truncation operator from %s to %s failed on %s" (show bs1) (show bs2) (show v)
  I64ExtendSI32 -> do
    v <- pop -< ()
    i64ExtendSI32 -< v
  I64ExtendUI32 -> do
    v <- pop -< ()
    i64ExtendUI32 -< v
  FConvertIU bs1 bs2 -> do
    v <- pop -< ()
    fConvertIU -< (bs1, bs2, v)
  FConvertIS bs1 bs2 -> do
    v <- pop -< ()
    fConvertIS -< (bs1, bs2, v)
  F32DemoteF64 -> do
    v <- pop -< ()
    f32DemoteF64 -< v
  F64PromoteF32 -> do
    v <- pop -< ()
    f64PromoteF32 -< v
  IReinterpretF bs -> do
    v <- pop -< ()
    iReinterpretF -< (bs, v)
  FReinterpretI bs -> do
    v <- pop -< ()
    fReinterpretI -< (bs, v)


isControlInst :: Instruction index -> Bool
isControlInst i = case i of
  Unreachable -> True
  Nop -> True
  Block{} -> True
  Loop{} -> True
  If{} -> True
  Br _ -> True
  BrIf _ -> True
  BrTable _ _ -> True
  Return -> True
  Call _ -> True
  CallIndirect _ -> True
  _ -> False

isParametricInst :: Instruction index -> Bool
isParametricInst i = case i of
  Drop -> True
  Select -> True
  _ -> False

isMemoryInst :: Instruction index -> Bool
isMemoryInst i = case i of
  I32Load _ -> True
  I64Load _ -> True
  F32Load _ -> True
  F64Load _ -> True
  I32Load8S _ -> True
  I32Load8U _ -> True
  I32Load16S _ -> True
  I32Load16U _ -> True
  I64Load8S _ -> True
  I64Load8U _ -> True
  I64Load16S _ -> True
  I64Load16U _ -> True
  I64Load32S _ -> True
  I64Load32U _ -> True
  I32Store _ -> True
  I64Store _ -> True
  F32Store _ -> True
  F64Store _ -> True
  I32Store8 _ -> True
  I32Store16 _ -> True
  I64Store8 _ -> True
  I64Store16 _ -> True
  I64Store32 _ -> True
  CurrentMemory -> True
  GrowMemory -> True
  _ -> False


isVariableInst :: Instruction index -> Bool
isVariableInst i = case i of
  GetLocal _ -> True
  SetLocal _ -> True
  TeeLocal _ -> True
  GetGlobal _ -> True
  SetGlobal _ -> True
  _ -> False

isNumericInst :: Instruction index -> Bool
isNumericInst i = case i of
  I32Const _ -> True
  I64Const _ -> True
  F32Const _ -> True
  F64Const _ -> True
  IUnOp _ _ -> True
  IBinOp _ _ -> True
  I32Eqz -> True
  I64Eqz -> True
  IRelOp _ _ -> True
  FUnOp _ _ -> True
  FBinOp _ _ -> True
  FRelOp _ _ -> True
  I32WrapI64 -> True
  ITruncFU _ _ -> True
  ITruncFS _ _ -> True
  I64ExtendSI32 -> True
  I64ExtendUI32 -> True
  FConvertIU  _ _ -> True
  FConvertIS _ _ -> True
  F32DemoteF64 -> True
  F64PromoteF32 -> True
  IReinterpretF _ -> True
  FReinterpretI _ -> True
  _ -> False

