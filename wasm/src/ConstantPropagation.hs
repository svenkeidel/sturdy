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
import qualified UnitAnalysisValue as Unit

import qualified Concrete as Concrete
import qualified Abstract as Unit

import           Control.Arrow
import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Chaotic (innermost)
import           Control.Arrow.Fix.ControlFlow
import           Control.Arrow.Trans as Trans
import           Control.Arrow.EffectiveAddress
import           Control.Arrow.Serialize
import           Control.Arrow.Size

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

import           Language.Wasm.Interpreter (ModuleInstance)
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure (ResultType, ValueType)
import           Language.Wasm.Validate (ValidModule)

import           Numeric.Natural (Natural)


type Value = ConstantOr Unit.Value


-- EffectiveAddress

instance (Arrow c, Profunctor c, ArrowChoice c) => ArrowEffectiveAddress Value Natural Addr (EffectiveAddressT c) where
  effectiveAddress = proc (v, off) -> case v of
    Constant (Concrete.Value (Wasm.VI32 w32)) -> returnA -< Addr $ fromIntegral w32 + fromIntegral off
    NotConstant (Unit.Value (Unit.VI32 ())) -> returnA -< TopAddr
    _ -> returnA -< error "effectiveAddress: arguments needs to be an i32 integer"


-- ArrowSize

instance (ArrowChoice c, Profunctor c) => ArrowSize Value MemSize (SizeT Value c) where
  valToSize = proc v -> case v of
    Constant (Concrete.Value (Wasm.VI32 w32)) -> returnA -< MemSize $ fromIntegral w32
    NotConstant (Unit.Value (Unit.VI32 ())) -> returnA -< TopMemSize
    _        -> returnA -< error "valToSize: argument needs to be an i32 integer."

  sizeToVal = proc size -> case size of
    TopMemSize -> returnA -< NotConstant (Unit.Value (Unit.VI32 ()))
    MemSize i -> returnA -< Constant (Concrete.Value (Wasm.VI32 $ fromIntegral i))


-- Serialize

instance (Profunctor c, Arrow c, ArrowChoice c) => ArrowSerialize Value Bytes ValueType LoadType StoreType (SerializeT Value c) where
    encode = proc (v,_,_) -> case v of
      Constant (Concrete.Value c) -> returnA -< encodeConcreteValue c
      NotConstant (Unit.Value (Unit.VI32 ())) -> returnA -< Vec.replicate 4 TopByte
      NotConstant (Unit.Value (Unit.VI64 ())) -> returnA -< Vec.replicate 8 TopByte
      NotConstant (Unit.Value (Unit.VF32 ())) -> returnA -< Vec.replicate 4 TopByte
      NotConstant (Unit.Value (Unit.VF64 ())) -> returnA -< Vec.replicate 8 TopByte

    decode = proc (bytes, _, valTy) -> 
      if any (==TopByte) bytes
      then returnA -< NotConstant $ Unit.unitValue valTy
      else returnA -< Constant $ Concrete.Value $ decodeConcreteValue valTy bytes


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
                                     -> Memories
                                     -> Text
                                     -> [Value]
                                     -> Result
invokeExported initialState tab modInst mems funcName args =
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
        (JoinVector $ Vec.empty,((0,modInst),(tab,(mems,(initialState,([],([],(funcName, args))))))))
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

instantiateAbstract :: ValidModule -> IO (Either String (ModuleInstance, StaticGlobalState Value, Memories, JoinVector TableInst))
instantiateAbstract valMod = do
  res <- instantiate valMod (Constant . Concrete.Value) (const makeMemory) TableInst
  return $ fmap (\(m,s,mems,tab) -> (m,s,makeMemories mems,JoinVector tab)) res

