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

module UnitAnalysis where

import           Abstract
import qualified Abstract as A
import           Data
import           GenericInterpreter hiding (Exc)
import qualified GenericInterpreter as Generic
import           UnitAnalysisValue

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Logger
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Chaotic (innermost)
import           Control.Arrow.Fix.ControlFlow
import           Control.Arrow.MemAddress
import           Control.Arrow.Memory
import           Control.Arrow.MemSizable
import           Control.Arrow.Reader
import           Control.Arrow.Serialize
import           Control.Arrow.Stack
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Arrow.Order
import           Control.Arrow.Trans as Trans
import           Control.Arrow.GlobalState
import           Control.Arrow.WasmFrame

import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable
import           Control.Arrow.Transformer.Abstract.Fix.Component
import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow
import qualified Control.Arrow.Transformer.Abstract.Fix.Stack as Fix
import           Control.Arrow.Transformer.Abstract.Memory
import           Control.Arrow.Transformer.Abstract.Serialize
import           Control.Arrow.Transformer.Abstract.Table
import           Control.Arrow.Transformer.Abstract.Terminating

import           Control.Arrow.Transformer.Stack (StackT)
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.StaticGlobalState
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.WasmFrame

import           Control.Category (Category)

import           Data.Abstract.FreeCompletion as FC
import           Data.Abstract.Sign
import qualified Data.Abstract.Sign as S
import           Data.Abstract.Terminating
import qualified Data.Function as Function
import           Data.Hashable
import           Data.HashSet as HashSet
import           Data.IORef
import           Data.Label
import           Data.Order
import           Data.Abstract.DiscretePowerset
import           Data.Abstract.Error as Error
import           Data.Abstract.Except
import qualified Data.Abstract.Powerset as Pow
import qualified Data.Abstract.Widening as W
import           Data.Profunctor
import           Data.Text.Lazy (Text)
import           Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Vector as Vec

import           Language.Wasm.Interpreter (ModuleInstance,emptyStore,emptyImports)
import qualified Language.Wasm.Interpreter as Wasm
--import           Language.Wasm.Structure hiding (exports, Const)
import           Language.Wasm.Structure (BitSize(..), IUnOp(..), IBinOp(..), IRelOp(..), FUnOp(..),
                                          FBinOp(..), FRelOp(..), ValueType(..))
import qualified Language.Wasm.Structure as Wasm
import           Language.Wasm.Validate (ValidModule)

import           Numeric.Natural (Natural)

newtype Exc v = Exc (HashSet (Generic.Exc v)) deriving (Eq, Show, Hashable, PreOrd, Complete)

instance (Show v) => Pretty (Exc v) where pretty = viaShow
instance (Show n) => Pretty (Instruction n) where pretty = viaShow



alpha :: Wasm.Value -> Value
alpha v = Value $ case v of
                              Wasm.VI32 _ -> VI32 ()
                              Wasm.VI64 _ -> VI64 ()
                              Wasm.VF32 _ -> VF32 ()
                              Wasm.VF64 _ -> VF64 ()

mapList :: (ArrowChoice c) => c (a,x) y -> c ([a],x) [y]
mapList f = proc (as,x) -> do
                     --as <- aArr -< ()
                     case as of
                         [] -> returnA -< []
                         (a:ass) -> do
                            bs <- mapList f -< (ass,x)
                            b <- f -< (a,x)
                            returnA -< b:bs

--joinList1'' f = proc (acc,bs) -> do
--                 case bs of
--                     [] -> returnA -< acc
--                     (b:bss) -> do
--                         newA <- f -< (acc,b)
--                         joinList f -< (newA,bss)

tailA :: (ArrowChoice c) => c () [a] -> c () [a]
tailA f = proc () -> do
              aList <- f -< ()
              case aList of
                  (a:as) -> returnA -< as


instance (ArrowChoice c) => IsException (Exc Value) Value (ValueT Value c) where
    type JoinExc y (ValueT Value c) = ArrowComplete y (ValueT Value c)
    exception = arr $ Exc . HashSet.singleton
    handleException f = proc (Exc excs,x) -> do
                            --ys <- mapList f -< (HashSet.toList excs,x)
                            --joinList _j -< (_init,ys)
                            joinList1'' f -< (HashSet.toList excs,x)
--                      y1 <- f -< first
--                      y2 <- f -< second
--                      ...
--                      yn <- f -< nth
--                      returnA -< y1 ⊔ y2 ⊔ ... ⊔ yn

--newtype GlobalStateT v c x y = GlobalStateT (StateT (FreeCompletion (GlobalState v)) c x y)
--    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift, ArrowReader r,
--              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowRun, ArrowFrame fd val,
--              ArrowStack st, ArrowLogger l, ArrowJoin)
--
--instance (ArrowState s c) => ArrowState s (GlobalStateT v c) where
--    -- TODO
--
--instance ArrowTrans (GlobalStateT v) where
--    lift' a = GlobalStateT (lift' a)
--
--instance (ArrowChoice c, Profunctor c) => ArrowGlobalState v Int (GlobalStateT v c) where
--    readFunction (GlobalStateT funcCont) =
--        GlobalStateT $ proc (i,x) -> do
--            Lower(GlobalState{funcInstances = fs}) <- get -< ()
--            case fs Vec.! i of
--                FuncInst fTy modInst code -> funcCont -< ((fTy,modInst,code),x)
--                _                         -> returnA -< error "not yet implemented" --hostCont -< ((fTy,code),x)
--    fetchMemory = arr Prelude.id
--    storeMemory = arr $ const ()
--
--instance (Profunctor c, ArrowChoice c) => ArrowMemory Int () () (GlobalStateT Value c) where
--    type Join y (GlobalStateT Value c) = ArrowComplete y (GlobalStateT Value c)
--    memread sCont eCont = proc (i, (_, _, x)) -> do
--        y <- (sCont -< ((),x)) <⊔> (eCont -< x)
--        returnA -< (i, y)
--    memstore sCont eCont = proc (i, (_, _, x)) -> do
--        y <- (sCont -< x) <⊔> (eCont -< x)
--        returnA -< (i, y)
--
--instance (Arrow c, Profunctor c) => ArrowMemAddress Value Natural () (GlobalStateT Value c) where
--    memaddr = arr $ const ()
--
--toTopVal :: ValueType -> Value
--toTopVal I32 = Value $ Lower $ VI32 top
--toTopVal I64 = Value $ Lower $ VI64 top
--toTopVal F32 = Value $ Lower $ VF32 top
--toTopVal F64 = Value $ Lower $ VF64 top
--
--instance (Profunctor c, Arrow c) => ArrowSerialize Value () ValueType LoadType StoreType (GlobalStateT Value c) where
--    decode sCont = proc ((), _, valTy, x) -> sCont -< (toTopVal valTy, x)
--    encode sCont = proc (_,_,_,x) -> sCont -< ((),x)
--
--instance ArrowMemSizable sz (GlobalStateT Value c) where
--    -- TODO
--
--instance ArrowFix (Underlying (GlobalStateT v c) x y) => ArrowFix (GlobalStateT v c x y) where
--    type Fix (GlobalStateT v c x y) = Fix (Underlying (GlobalStateT v c) x y)


--deriving instance (ArrowComplete (FreeCompletion (GlobalState v), y) c) => ArrowComplete y (GlobalStateT v c)

type In = (
                       (JoinVector Value,
                        ((Natural, ModuleInstance),
                         (Tables,
                          (StaticGlobalState Value,
                           (JoinList Value, (LabelArities, [Instruction Natural])))))))

type Out = Terminating
                        (
                         Error
                           (Pow String)
                           (JoinVector Value,
                            --(Tables,
                             (StaticGlobalState Value,
                              Except (Exc Value) (JoinList Value, ()))))


type Result = (CFG (Instruction Natural), Terminating
                                          (Error
                                             (Pow String)
                                             (JoinVector Value,
                                              --(Tables,
                                                (StaticGlobalState Value,
                                                 Except (Exc Value) (JoinList Value, [Value])))))

invokeExported :: StaticGlobalState Value
                                     -> Tables
                                     -> ModuleInstance
                                     -> Text
                                     -> [Value]
                                     -> Result
invokeExported store tab modInst funcName args =
    let ?cacheWidening = W.finite in
    --let ?fixpointAlgorithm = Function.fix in -- TODO: we need something else here
    --let algo = (trace p1 p2) . (Fix.filter isRecursive $ innermost) in
    let algo = (recordControlFlowGraph' getExpression) . (Fix.filter isRecursive $ innermost) in
    let ?fixpointAlgorithm = fixpointAlgorithm algo in
    (\(cfg,(_,res)) -> (cfg,res)) $ Trans.run
    (Generic.invokeExported ::
      ValueT Value
        (ReaderT Generic.LabelArities
          (StackT Value
            (ExceptT (Exc Value)
              (StaticGlobalStateT Value
                (MemoryT
                  (SerializeT
                    (TableT
                      (FrameT FrameData Value
                        (ErrorT (Pow String)
                    --(LoggerT String
                          (TerminatingT
                            (FixT
                              (ComponentT Component In
                                (Fix.StackT Fix.Stack  In
                                  (CacheT Cache In Out
                                    (ControlFlowT (Instruction Natural)
                                      (->)))))))))))))))) (Text, [Value]) [Value]) (JoinVector $ Vec.empty,((0,modInst),(tab,(store,([],(Generic.LabelArities [],(funcName, args)))))))
    where
        isRecursive (_,(_,(_,(_,(_,(_,inst)))))) = case inst of
            Loop {} : _ -> True
            Call _ _ : _  -> True
            CallIndirect _ _ : _ -> True --error "todo"
            _           -> False
        p1 (locals,(_,(_,(stack,(la, instr))))) = --braces $ hsep (punctuate "," (pretty <$> toList stack))
                        hsep [pretty stack, pretty locals, pretty la, pretty instr]
        p2 (Terminating (Error.Success (stack, (_,rest)))) = pretty rest
        p2 x = pretty x

        getExpression (_,(_,(_,(_,(_,(_,exprs)))))) = case exprs of e:_ -> Just e; _ -> Nothing

instantiateAbstract :: ValidModule -> IO (Either String (ModuleInstance, StaticGlobalState Value, Tables))
instantiateAbstract valMod = do res <- instantiate valMod alpha (\_ _ -> ()) TableInst
                                return $ fmap (\(m,s,_,tab) -> (m,s,JoinVector tab)) res

--instantiate :: ValidModule -> IO (Either String (ModuleInstance, GlobalState Value))
--instantiate valMod = do
--    res <- Wasm.instantiate emptyStore emptyImports valMod
--    case res of
--        Right (modInst, store) -> do
--            wasmStore <- storeToGlobalState store
--            return $ Right $ (modInst, wasmStore)
--        Left e -> return $ Left e
--
--    where
--        storeToGlobalState (Wasm.Store funcI tableI memI globalI) = do
--            let funcs = generate $ Vec.mapM convertFuncInst funcI
--            globs <- Vec.mapM convertGlobals globalI
--            return $ GlobalState funcs
--                                 (Vec.map TableInst tableI)
--                                 globs
--
----        convertFuncs (Wasm.FunctionInstance t m c) = error "todo" --FuncInst t m c
----        convertFuncs (Wasm.HostInstance t _) = HostInst t
--
--        convertGlobals (Wasm.GIConst _ v) = return $ Lower $ GlobInst Const (alpha v)
--        convertGlobals (Wasm.GIMut _ v) = do
--            val <- readIORef v
--            return $ Lower $ GlobInst Mutable (alpha val)
