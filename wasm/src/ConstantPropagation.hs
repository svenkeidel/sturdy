{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ConstantPropagation where

import           Prelude hiding ((.))

import           Abstract
import           Data
import           GenericInterpreter hiding (Exc)
import qualified GenericInterpreter as Generic
import           ConstantPropagationValue
import           UnitAnalysisValue (Exc(..))

import qualified Concrete as Concrete
import qualified Abstract as Unit
import qualified UnitAnalysisValue as Unit

import           Control.Arrow
import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Chaotic (innermost)
import           Control.Arrow.Fix.ControlFlow
import           Control.Arrow.Trans as Trans
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Functions
import           Control.Arrow.EffectiveAddress
import           Control.Arrow.Memory
import           Control.Arrow.Order
import           Control.Arrow.Reader
import           Control.Arrow.Serialize
import           Control.Arrow.Size
import           Control.Arrow.Stack
import           Control.Arrow.Globals
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Arrow.Table
import           Control.Arrow.Trans
import           Control.Arrow.WasmFrame

import           Control.Category

import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.LogError
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable
import           Control.Arrow.Transformer.Abstract.Fix.Component
import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow
import qualified Control.Arrow.Transformer.Abstract.Fix.Stack as Fix
import           Control.Arrow.Transformer.Abstract.Table
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.KnownAddressMemory

import           Control.Arrow.Transformer.JumpTypes
import           Control.Arrow.Transformer.Stack (StackT)
import           Control.Arrow.Transformer.StaticGlobalState
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.WasmFrame


import           Data.Profunctor

import           Data.Abstract.Terminating
import           Data.Abstract.Except
import           Data.Abstract.MonotoneErrors (Errors)
import qualified Data.Abstract.Widening as W
import           Data.Text.Lazy (Text)
import qualified Data.Vector as Vec
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           Language.Wasm.Interpreter (ModuleInstance)
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure (ResultType)
import           Language.Wasm.Validate (ValidModule)

import           Numeric.Natural (Natural)

-- EffectiveAddress

newtype EffectiveAddressT c x y = EffectiveAddressT (c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowReader r, ArrowGlobals val, ArrowFunctions, ArrowSize v sz,
              ArrowSerialize val dat valTy datDecTy datEncTy, ArrowTable v, ArrowJoin)

instance ArrowTrans EffectiveAddressT where
  lift' = EffectiveAddressT

instance (Arrow c, Profunctor c, ArrowChoice c) => ArrowEffectiveAddress Value Natural Addr (EffectiveAddressT c) where
  effectiveAddress = proc (v, off) -> case v of
    Constant (Concrete.Value (Wasm.VI32 w32)) -> returnA -< Addr $ fromIntegral w32 + fromIntegral off
    UnitValue (Unit.Value (Unit.VI32 ())) -> returnA -< TopAddr
    _ -> returnA -< error "effectiveAddress: arguments needs to be an i32 integer"


deriving instance (Arrow c, Profunctor c, ArrowComplete y c) => ArrowComplete y (EffectiveAddressT c)

instance (ArrowLift c, ArrowFix (Underlying (EffectiveAddressT c) x y)) => ArrowFix (EffectiveAddressT c x y) where
    type Fix (EffectiveAddressT c x y) = Fix (Underlying (EffectiveAddressT c) x y)




-- ArrowSize

newtype SizeT v c x y = SizeT (c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowReader r, ArrowGlobals val, ArrowMemory addr bytes s,
              ArrowEffectiveAddress base off addr, ArrowFunctions,
              ArrowSerialize val dat valTy datDecTy datEncTy, ArrowTable v, ArrowJoin)

instance ArrowTrans (SizeT v) where
  -- lift' :: c x y -> MemoryT v c x y
  lift' = SizeT

instance (ArrowChoice c, Profunctor c) => ArrowSize Value MemSize (SizeT Value c) where
  valToSize = proc v -> case v of
    Constant (Concrete.Value (Wasm.VI32 w32)) -> returnA -< MemSize $ fromIntegral w32
    UnitValue (Unit.Value (Unit.VI32 ())) -> returnA -< TopMemSize
    _        -> returnA -< error "valToSize: argument needs to be an i32 integer."

  sizeToVal = proc size -> case size of
    TopMemSize -> returnA -< UnitValue (Unit.Value (Unit.VI32 ()))
    MemSize i -> returnA -< Constant (Concrete.Value (Wasm.VI32 $ fromIntegral i))


deriving instance (Arrow c, Profunctor c, ArrowComplete y c) => ArrowComplete y (SizeT v c)

instance (ArrowLift c, ArrowFix (Underlying (SizeT v c) x y)) => ArrowFix (SizeT v c x y) where
    type Fix (SizeT v c x y) = Fix (Underlying (SizeT v c) x y)






-- Abstract interpreter

type In = (Errors Err,
           (JoinVector Value,
            ((Natural, ModuleInstance),
              (Tables,
                (Memories,
                  (StaticGlobalState Value,
                    (JoinList Value, ([ResultType], [Instruction Natural]))))))))

type Out = (Errors Err, (Terminating
                (JoinVector Value,
                -- (Tables,
                    (Memories,
                      (StaticGlobalState Value,
                        Except (Exc Value) (JoinList Value, ()))))))


type Result = (CFG (Instruction Natural), (Errors Err,
                                            Terminating
                                             (JoinVector Value,
                                              -- (Tables,
                                              (Memories,
                                                (StaticGlobalState Value,
                                                 Except (Exc Value) (JoinList Value, [Value]))))))


invokeExported :: StaticGlobalState Value
                                     -> Tables
                                     -> ModuleInstance
                                     -> [MemSize]
                                     -> Text
                                     -> [Value]
                                     -> Result
invokeExported initialState tab modInst memSizes funcName args =
    let ?cacheWidening = (W.finite,W.finite) in
    --let ?fixpointAlgorithm = Function.fix in -- TODO: we need something else here
    --let algo = (trace p1 p2) . (Fix.filter isRecursive $ innermost) in
    let algo = recordControlFlowGraph' getExpression . Fix.filter isRecursive innermost in
    let ?fixpointAlgorithm = fixpointAlgorithm algo in
    (\(cfg,(_,res)) -> (cfg,res)) $ Trans.run
    (Generic.invokeExported ::
      ValueT Value
        (JumpTypesT
          (StackT Value
            (ExceptT (Exc Value)
              (StaticGlobalStateT Value
                (MemoryT
                  (EffectiveAddressT
                    (SizeT Value
                      (SerializeT Value
                        (TableT Value
                          (FrameT FrameData Value
                            (TerminatingT
                              (LogErrorT Err
                                (FixT
                                  (ComponentT Component In
                                    (Fix.StackT Fix.Stack In
                                      (CacheT Monotone In Out
                                        (ControlFlowT (Instruction Natural)
                                          (->)))))))))))))))))) (Text, [Value]) [Value]) 
        (JoinVector $ Vec.empty,((0,modInst),(tab,(freshMemories memSizes,(initialState,([],([],(funcName, args))))))))
    where
        isRecursive (_,(_,(_,(_,(_,(_,(_,(_,inst)))))))) = case inst of
            Loop {} : _ -> True
            Call _ _ : _  -> True
            CallIndirect _ _ : _ -> True --error "todo"
            _           -> False
        -- p1 (locals,(_,(_,(stack,(la, instr))))) = --braces $ hsep (punctuate "," (pretty <$> toList stack))
        --                 hsep [pretty stack, pretty locals, pretty la, pretty instr]
        -- p2 (Terminating (Error.Success (stack, (_,rest)))) = pretty rest
        -- p2 x = pretty x

        getExpression (_,(_,(_,(_,(_,(_,(_,(_,exprs)))))))) = case exprs of e:_ -> Just e; _ -> Nothing

-- instantiateAbstract :: ValidModule -> IO (Either String (ModuleInstance, StaticGlobalState Value, Tables))
-- instantiateAbstract valMod = do res <- instantiate valMod alpha (\_ _ -> ()) TableInst
--                                 return $ fmap (\(m,s,_,tab) -> (m,s,JoinVector tab)) res
