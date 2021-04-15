{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
--{-# LANGUAGE OverloadedStrings #-}
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
--import           Data hiding (label,iUnOp,iBinOp)

import           Control.Arrow
import           Control.Arrow.Except
import qualified Control.Arrow.Except as Exc
import           Control.Arrow.Fail as Fail
import           Control.Arrow.Fix
import           Control.Arrow.MemAddress
import           Control.Arrow.Memory as Mem
import           Control.Arrow.MemSizable
import           Control.Arrow.Reader
import           Control.Arrow.Serialize
import           Control.Arrow.Stack
import           Control.Arrow.Table
import qualified Control.Arrow.Utils as Arr
import           Control.Arrow.StaticGlobalState
import           Control.Arrow.WasmFrame

import           Data.Hashable
import           Data.Profunctor
import           Data.String
import           Data.Text.Lazy (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Vector hiding (length, (++))
import           Data.Word

--import           Language.Wasm.Structure hiding (exports, Instruction)
import           Language.Wasm.Structure (ValueType(..), BitSize, IUnOp(..), IBinOp(..), IRelOp(..),
                                          FUnOp(..), FBinOp(..), FRelOp(..), MemArg(..), FuncType(..), ResultType)
import           Language.Wasm.Interpreter (ModuleInstance(..), emptyModInstance, ExportInstance(..), ExternalValue(..))

import           Numeric.Natural (Natural)
import           Text.Printf

import           GHC.Generics
import           GHC.Exts

-- the kind of exceptions that can be thrown
data Exc v = Trap String | Jump Natural [v] | CallReturn [v] deriving (Show, Eq, Generic)

instance Hashable v => Hashable (Exc v)

class IsException exc v c | c -> v where
    type family JoinExc y (c :: * -> * -> *) :: Constraint
    exception :: c (Exc v) exc
    handleException :: JoinExc y c => c (Exc v, x) y -> c (exc,x) y

--instance (Eq v) => Complete (Exc v) where
--    x ⊔ y | x == y = x
--          | otherwise = Top
--
--instance (Eq v) => PreOrd (Exc v) where
--    _ ⊑ Top = True
--    x ⊑ y | x == y = True

-- used for storing the return "arity" of nested labels
newtype LabelArities = LabelArities {labels :: [Natural]} deriving (Eq,Show,Generic)
instance Hashable LabelArities
instance Pretty LabelArities where pretty = viaShow

-- stores a frame's static data (return arity and module instance)
type FrameData = (Natural, ModuleInstance)

---- constraints to support (and call) host functions
--type HostFunctionSupport addr bytes v c = (ArrowApply c, ArrowGlobalState v m c, ArrowWasmMemory m addr bytes v c)
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

type ArrowStaticComponents v c =
  ( ArrowStaticGlobalState v c,
    ArrowStack v c,
    ArrowFrame FrameData v c,
    ArrowReader LabelArities c)

type ArrowDynamicComponents v addr bytes exc e c =
  ( ArrowTable v c,
    ArrowWasmMemory addr bytes v c,
    IsVal v c,
    ArrowExcept exc c, IsException exc v c,
    ArrowFail e c, IsString e,
    ArrowFix (c [Instruction Natural] ()),
    ?fixpointAlgorithm :: FixpointAlgorithm (Fix (c [Instruction Natural] ())))


class Show v => IsVal v c | c -> v where
    type family JoinVal y (c :: * -> * -> *) :: Constraint

    i32const :: c Word32 v
    i64const :: c Word64 v
    f32const :: c Float v
    f64const :: c Double v
    iUnOp :: c (BitSize, IUnOp, v) v
    iBinOp :: c (IBinOp, v, v) v -> c (BitSize, IBinOp, v, v) v
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
    i32ifNeqz :: (JoinVal y c) => c x y -> c x y -> c (v, x) y
    -- | `listLookup f g (v, xs, x)`
    -- | Looks up the `v`-th element in `xs` and passes it to `f`, or
    -- | passes `x` to `g` if `v` is out of range of `xs`.
    listLookup :: c x y -> c x y -> c (v, [x], x) y
    ifHasType :: c x y -> c x y -> c (v, ValueType, x) y


-- entry point to the generic interpreter
-- the module instance comes from ArrowFrame
-- ArrowGlobalState and ArrowWasmMemory are properly initialized
-- argument Text: name of the function to execute
-- argument [v]: arguments going to be passed to the function
invokeExported ::
  ( ArrowChoice c,
    ArrowStaticComponents v c, ArrowDynamicComponents v addr bytes exc e c,
    JoinExc () c, Exc.Join () c,
    Mem.Join () c,
    JoinVal () c, Show v,
    Fail.Join [v] c,
    Fail.Join () c,
    JoinTable () c)
  => c (Text, [v]) [v]
invokeExported = proc (funcName, args) -> do
  (_, modInst) <- frameData -< () -- get the module instance
  -- look for a function with name funcName in the function's exports
  case find (\(ExportInstance n _) -> n == funcName) (exports modInst) of
      -- if found -> invoke
      Just (ExportInstance _ (ExternFunction addr)) -> invokeExternal -< (addr, args)
      _ -> fail -< fromString $ printf "Function with name %s was not found in module's exports" (show funcName)

invokeExternal ::
  ( ArrowChoice c,
    ArrowStaticComponents v c, ArrowDynamicComponents v addr bytes exc e c,
    JoinExc () c,
    Mem.Join () c,
    JoinVal () c, Show v,
    Exc.Join () c,
    Fail.Join () c,
    JoinTable () c)
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
        then fail -< fromString $ printf "Wrong number of arguments in external invocation. Expected %d but got %d" (length paramTys) (length args)
        else returnA -< ()
      Arr.zipWith
        (proc (arg, ty) ->
          ifHasType
            (arr $ const ())
            (proc (arg, ty) -> fail -< fromString $ printf "Wrong argument type in external invocation. Expected %s but got %s" (show ty) (show arg))
            -< (arg, ty, (arg, ty)))
        -< (args, paramTys)
      f -< x


eval ::
  ( ArrowChoice c,
    ArrowStaticComponents v c, ArrowDynamicComponents v addr bytes exc e c,
    JoinExc () c,
    Mem.Join () c,
    JoinVal () c, Show v,
    Exc.Join () c,
    JoinTable () c)
  => c [Instruction Natural] ()
eval = fix $ \eval' -> proc is -> do
    --stack <- getStack -< ()
    case is of
      [] -> returnA -< ()
      i:is' | isNumericInst i -> do
        --log-< "start " ++ (show i) ++ ", stack: " ++ (show stack)-- ++ ", locals: " ++ (show locals)
        push <<< evalNumericInst -< i
        --stack2 <- getStack -< ()
        --log-< "end " ++ (show i) ++ ", stack: " ++ (show stack2)
        eval' -< is'
      i:is' | isVariableInst i -> do
        --log-< "start " ++ (show i) ++ ", stack: " ++ (show stack)-- ++ ", locals: " ++ (show locals)
        evalVariableInst -< i
        --stack2 <- getStack -< ()
        --log-< "end " ++ (show i) ++ ", stack: " ++ (show stack2)
        eval' -< is'
      i:is' | isParametricInst i -> do
        --log-< "start " ++ (show i) ++ ", stack: " ++ (show stack)-- ++ ", locals: " ++ (show locals)
        evalParametricInst -< i
        --stack2 <- getStack -< ()
        --log-< "end " ++ (show i) ++ ", stack: " ++ (show stack2)
        eval' -< is'
      i:is' | isControlInst i -> do
        --log-< "start " ++ (show i) ++ ", stack: " ++ (show stack)-- ++ ", locals: " ++ (show locals)
        evalControlInst eval' -< i
        --stack2 <- getStack -< ()
        --log-< "end " ++ (show i) ++ ", stack: " ++ (show stack2)
        eval' -< is'
      i:is' | isMemoryInst i -> do
        --log-< "start " ++ (show i) ++ ", stack: " ++ (show stack)-- ++ ", locals: " ++ (show locals)
        evalMemoryInst -< i
        --stack2 <- getStack -< ()
        --log-< "end " ++ (show i) ++ ", stack: " ++ (show stack2)
        eval' -< is'

evalControlInst ::
  ( ArrowChoice c, Profunctor c,
    ArrowStaticComponents v c,
    ArrowDynamicComponents v addr bytes exc e c,
    JoinVal () c,
    JoinExc () c,
    Exc.Join () c,
    JoinTable () c)
  => c [Instruction Natural] () -> c (Instruction Natural) ()
evalControlInst eval' = proc i -> case i of
  Unreachable _ -> throw <<< exception -< Trap "Execution of unreachable instruction"
  Nop _ -> returnA -< ()
  Block rt is _ -> label eval' eval' -< (rt, is, [])
  Loop rt is l -> label eval' eval' -< (rt, is, [Loop rt is l])
  If rt isNZero isZero _ -> do
    v <- pop -< ()
    i32ifNeqz
      (proc (rt, isNZero, _) -> label eval' eval' -< (rt, isNZero, []))
      (proc (rt, _, isZero) -> label eval' eval' -< (rt, isZero, []))
      -< (v, (rt, isNZero, isZero))
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
    result <- readFunction (proc (funcAddr, ()) -> do
                    --stack <- getStack -< ()
                    --log-< "readFunction start, stack: " ++ (show stack)
                    result <- invoke eval' <<^ fst -< (funcAddr, ())
                    --stack2 <- getStack -< ()
                    --log-< "readFunction end, stack: " ++ (show stack2)
                    returnA -< result
        ) -< (funcAddr, ()) --(invokeHost <<^ fst) -< (funcAddr, ())
    --stack <- getStack -< ()
    --log-< "before returning to eval, stack: " ++ (show stack)
    returnA -< result
  CallIndirect ix _ -> do
    (_, modInst) <- frameData -< () -- get the current module instance
    let tableAddr = tableaddrs modInst ! 0 -- get address of table 0
    let ftExpect = funcTypes modInst ! fromIntegral ix -- get expected functype
    funcAddr <- pop -< ()
    readTable
      (invokeChecked eval')
      (proc (ta,ix,_) -> throw <<< exception -< Trap $ printf "Index %s out of bounds for table address %s" (show ix) (show ta))
      (proc (ta,ix,_) -> throw <<< exception -< Trap $ printf "Index %s uninitialized for table address %s" (show ix) (show ta))
      -< (tableAddr, funcAddr, ftExpect)

invokeChecked ::
  ( ArrowChoice c,
    ArrowStaticComponents v c,
    --ArrowGlobalState v m c,
    IsVal v c, ArrowExcept exc c, IsException exc v c, JoinExc () c, Exc.Join () c)
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
      else throw <<< exception -< Trap $ printf "Mismatched function type in indirect call. Expected %s, actual %s." (show ftExpect) (show ftActual)

-- invoke function with code code within module instance funcModInst
-- the function execution can finish by different reasons:
--   - all instructions have been executed -> result are the top |resultTys| values on the stack
--   - the function calls return -> result are the top |resultTys| values on the stack
--   - the function produces a trap -> no result, trap is propagated
--   - TODO: what about break? Can we "break" to a function boundary? -> NO, only to block boundaries
invoke ::
  ( ArrowChoice c, ArrowStack v c, ArrowReader LabelArities c,
    IsVal v c, ArrowFrame FrameData v c, ArrowExcept exc c, IsException exc v c, Exc.Join y c,
    ArrowStack v c, JoinExc y c)
  => c [Instruction Natural] y -> c (FuncType, ModuleInstance, Function) y
invoke eval' = catch
    (proc (FuncType paramTys resultTys, funcModInst, Function _ localTys code) -> do
        --stack1 <- getStack -< ()
        --log-< "start invoke, stack: " ++ (show stack1)
        vs <- popn -< fromIntegral $ length paramTys
        zeros <- Arr.map initLocal -< localTys
        let rtLength = fromIntegral $ length resultTys
        --stack2 <- getStack -< ()
        --log-< "invoke before transfering control, stack: " ++ (show stack2)
        -- TODO: removed localFreshStack, not sure if that is what we want
        result <- inNewFrame (localNoLabels $ localFreshStack $ label eval' eval') -< ((rtLength, funcModInst), vs ++ zeros, (resultTys, code, []))
        --stack3 <- getStack -< ()
        --log-< "end invoke, stack: " ++ (show stack3)
        returnA -< result
        )
    (proc (_,e) -> handleException $
                   (proc (exc,_) -> case exc of
                       CallReturn vs -> do
                           pushn -< vs
                           eval' -< []
                       Trap _ -> throw <<< exception -< exc
                       Jump _ _ -> returnA -< error "invalid module: tried to jump through a function boundary")
                   -< (e,()))
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


branch :: (ArrowChoice c, ArrowExcept exc c, IsException exc v c, ArrowStack v c, ArrowReader LabelArities c) => c Natural ()
branch = proc ix -> do
  LabelArities{labels=ls} <- ask -< ()
  vs <- popn -< ls !! fromIntegral ix
  throw <<< exception -< Jump ix vs

-- | Introduces a branching point `g` that can be jumped to from within `f`.
-- | When escalating jumps, all label-local operands must be popped from the stack.
-- | This implementation assumes that ArrowExcept discards label-local operands in ArrowStack upon throw.
label :: (ArrowChoice c, ArrowExcept exc c, IsException exc v c, ArrowStack v c, ArrowReader LabelArities c, Exc.Join z c,
          ArrowStack v c, Show v, JoinExc z c)
  => c x z -> c y z -> c (ResultType, x, y) z
-- x: code to execute
-- y: continuation to execute after a break to the current label
label f g = catch
  -- after executing f without a break we expect |rt| results on top of the stack
  (proc (rt,x,_) -> do
    --stack <- getStack -< ()
    --log-< "labal start, stack: " ++ (show stack)
    result <- localLabel f -< (rt, x)
    --stack2 <- getStack -< ()
    --log-< "label end, stack: " ++ (show stack2)
    returnA -< result
  )
  -- after a break the results are popped from the stack and passed back via exception e
  (proc ((_,_,y),e) -> handleException $
                       (proc (exc,y) -> case exc of
                           Jump 0 vs -> do
                               pushn -< vs
                               g -< y
                           -- we expect all label-local operands are popped from the stack
                           Jump n vs -> throw <<< exception -< Jump (n-1) vs
                           _ -> throw <<< exception -< exc)
                       -< (e,y)--case e of
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
    ArrowStaticComponents v c, ArrowDynamicComponents v addr bytes exc e c,
    Mem.Join () c)
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
--  CurrentMemory -> push <<< memsize -< ()
--  GrowMemory -> do
--    n <- pop -< ()
--    memgrow
--      (push <<^ fst)
--      (proc _ -> push <<< i32const -< 0xFFFFFFFF) -- 0xFFFFFFFF ~= -1
--      -< (n, ())

--withCurrentMemory :: (ArrowChoice c, ArrowGlobalState v m c, ArrowMemory addr bytes c, ArrowFrame FrameData v c) => c (m,x) (m,y) -> c x y
--withCurrentMemory f = proc x -> do
--  (_,modInst) <- frameData -< ()
--  let memAddr = memaddrs modInst ! 0
--  withMemoryInstance f -< (memAddr, x)

load ::
  ( ArrowChoice c,
    ArrowStaticComponents v c, ArrowDynamicComponents v addr bytes exc e c,
    Mem.Join () c)
  => Int -> LoadType -> ValueType -> c Natural ()
load byteSize loadType valType = proc off -> do
  base <- pop -< ()
  addr <- memaddr -< (base, off)
  (_,modInst) <- frameData -< ()
  let memAddr = memaddrs modInst ! 0
  memread
    (proc (bytes,_) ->
      decode
        (push <<^ fst)
        --(error "decode failure")
        -< (bytes, loadType, valType, ()))
    (proc addr -> throw <<< exception -< Trap $ printf "Memory access out of bounds: Cannot read %d bytes at address %s in current memory" byteSize (show addr))
    -< (memAddr, addr, byteSize, addr)

store ::
  ( ArrowChoice c,
    ArrowStaticComponents v c, ArrowDynamicComponents v addr bytes exc e c,
    Mem.Join () c)
  => StoreType -> ValueType -> c Natural ()
store storeType valType = proc off -> do
  v <- pop -< ()
  encode
    (proc (bytes,off) -> do
      base <- pop -< ()
      addr <- memaddr -< (base, off)
      (_,modInst) <- frameData -< ()
      let memAddr = memaddrs modInst ! 0
      memstore
        (arr $ const ())
        (proc (addr,bytes) -> throw <<< exception -< Trap $ printf "Memory access out of bounds: Cannot write %s at address %s in current memory" (show bytes) (show addr))
        -< (memAddr, addr, bytes, (addr, bytes)))
    --(error "encode failure")
    -< (v, valType, storeType, off)

evalVariableInst ::
  ( ArrowChoice c, ArrowStaticComponents v c)
  => c (Instruction Natural) ()
evalVariableInst = proc i -> case i of
  GetLocal ix _ -> push <<< frameLookup -< ix
  SetLocal ix _-> do
    v <- pop -< ()
    frameUpdate -< (ix, v)
  TeeLocal ix _ -> do
    v <- peek -< ()
    frameUpdate -< (ix, v)
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
  ( ArrowChoice c, ArrowStack v c, ArrowExcept exc c, IsException exc v c, IsVal v c, Show v)
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
    iBinOp
      (proc (op,v1,v2) -> throw <<< exception -< Trap $ printf "Binary operator %s failed on %s" (show op) (show (v1,v2)))
      -< (bs, op, v1, v2)
--    case res of
--      Just v' -> returnA -< v'
--      Nothing -> throw <<< exception -< Trap $ printf "Binary operator %s failed on %s" (show op) (show (v1,v2))
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
    res <- iTruncFU -< (bs1, bs2, v)
    case res of
      Just v' -> returnA -< v'
      Nothing -> throw <<< exception -< Trap $ printf "Truncation operator from %s to %s failed on %s" (show bs1) (show bs2) (show v)
  ITruncFS bs1 bs2 _ -> do
    v <- pop -< ()
    res <- iTruncFS -< (bs1, bs2, v)
    case res of
      Just v' -> returnA -< v'
      Nothing -> throw <<< exception -< Trap $ printf "Truncation operator from %s to %s failed on %s" (show bs1) (show bs2) (show v)
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
  I64ExtendSI32 _ -> True
  I64ExtendUI32 _ -> True
  FConvertIU  _ _ _ -> True
  FConvertIS _ _ _ -> True
  F32DemoteF64 _ -> True
  F64PromoteF32 _ -> True
  IReinterpretF _ _ -> True
  FReinterpretI _ _ -> True
  _ -> False

