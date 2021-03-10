{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module UnitAnalysis where

import           Abstract
import qualified Abstract as A
import           Concrete (LoadType, StoreType, FuncInst(..), GlobInst(..), TableInst(..), Mut(..))
import           GenericInterpreter hiding (Exc)
import qualified GenericInterpreter as Generic

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Logger
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
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

import           Control.Arrow.Transformer.Abstract.DebuggableStack
import           Control.Arrow.Transformer.Abstract.Logger
import           Control.Arrow.Transformer.Abstract.Stack (AbsList)
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.WasmFrame

import           Control.Category

import           Data.Abstract.FreeCompletion as FC
import           Data.Abstract.Sign
import qualified Data.Abstract.Sign as S
import qualified Data.Function as Function
import           Data.Hashable
import           Data.HashSet as HashSet
import           Data.IORef
import           Data.Order
import           Data.Abstract.DiscretePowerset
import           Data.Abstract.Error
import           Data.Abstract.Except
import qualified Data.Abstract.Powerset as Pow
import           Data.Profunctor
import           Data.Text.Lazy (Text)
import qualified Data.Vector as Vec

import           Language.Wasm.Interpreter (ModuleInstance,emptyStore,emptyImports)
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure hiding (exports, Const)
import           Language.Wasm.Validate (ValidModule)

import           Numeric.Natural (Natural)

newtype Exc v = Exc (HashSet (Generic.Exc v)) deriving (Eq, Show, Hashable, PreOrd, Complete)

newtype Value = Value (FreeCompletion (BaseValue () () () ())) deriving (Eq, Show, Hashable, PreOrd, Complete)

valueI32 = Value $ Lower $ VI32 ()
valueI64 = Value $ Lower $ VI64 ()
valueF32 = Value $ Lower $ VF32 ()
valueF64 = Value $ Lower $ VF64 ()

alpha :: Wasm.Value -> Value
alpha v = Value $ Lower $ case v of
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

joinList1'' :: (ArrowChoice c, ArrowComplete y c) => c (v,x) y -> c ([v],x) y
joinList1'' f = proc (vs,x) -> case vs of
                    [v]    -> f -< (v,x)
                    (v:vss) -> (f -< (v,x)) <⊔> (joinList1'' f -< (vss,x))
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

instance (ArrowChoice c) => IsVal Value (ValueT Value c) where
    i32const = proc _ -> returnA -< valueI32
    i64const = proc _ -> returnA -< valueI64
    f32const = proc _ -> returnA -< valueF32
    f64const = proc _ -> returnA -< valueF64
    iBinOp = proc (bs, op, Value v1, Value v2) ->
        case (bs,op,v1,v2) of
            (BS32, IAdd, Lower (VI32 _), Lower (VI32 _)) -> returnA -< Just valueI32
            (BS32, IMul, Lower (VI32 _), Lower (VI32 _)) -> returnA -< Just valueI32
            (BS32, ISub, Lower (VI32 _), Lower (VI32 _)) -> returnA -< Just valueI32
            (BS64, IAdd, Lower (VI64 _), Lower (VI64 _)) -> returnA -< Just valueI64
            (BS64, IMul, Lower (VI64 _), Lower (VI64 _)) -> returnA -< Just valueI64
            (BS64, ISub, Lower (VI64 _), Lower (VI64 _)) -> returnA -< Just valueI64
    iRelOp = proc (bs,op,Value v1, Value v2) ->
        case (bs,op,v1,v2) of
            (BS32, IEq, Lower (VI32 _), Lower (VI32 _)) -> returnA -< valueI32
            (BS64, IEq, Lower (VI64 _), Lower (VI64 _)) -> returnA -< valueI32
    i32ifNeqz f g = proc (Value v, x) -> do
        case v of
            (Lower (VI32 _)) -> (f -< x) <⊔> (g -< x)
    ifHasType f g = proc (Value v,t,x) -> do
        case (v,t) of
            (Lower (VI32 _), I32) -> f -< x
            (Lower (VI64 _), I64) -> f -< x
            (Lower (VF32 _), F32) -> f -< x
            (Lower (VF64 _), F64) -> f -< x
            _                     -> g -< x

deriving instance ArrowComplete () c => ArrowComplete () (ValueT v c)

instance (ArrowChoice c) => IsException (Exc Value) Value (ValueT Value c) where
    type JoinExc y (ValueT Value c) = ArrowComplete y (ValueT Value c)
    handleException f = proc (Exc excs,x) -> do
                            --ys <- mapList f -< (HashSet.toList excs,x)
                            --joinList _j -< (_init,ys)
                            joinList1'' f -< (HashSet.toList excs,x)
--                      y1 <- f -< first
--                      y2 <- f -< second
--                      ...
--                      yn <- f -< nth
--                      returnA -< y1 ⊔ y2 ⊔ ... ⊔ yn

newtype GlobalStateT v c x y = GlobalStateT (StateT (FreeCompletion (GlobalState v)) c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift, ArrowReader r,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowLogger l, ArrowJoin)

instance (ArrowState s c) => ArrowState s (GlobalStateT v c) where
    -- TODO

instance ArrowTrans (GlobalStateT v) where
    lift' a = GlobalStateT (lift' a)

instance (ArrowChoice c, Profunctor c) => ArrowGlobalState v Int (GlobalStateT v c) where
    readFunction (GlobalStateT funcCont) =
        GlobalStateT $ proc (i,x) -> do
            Lower(GlobalState{funcInstances = fs}) <- get -< ()
            case fs Vec.! i of
                FuncInst fTy modInst code -> funcCont -< ((fTy,modInst,code),x)
                _                         -> returnA -< error "not yet implemented" --hostCont -< ((fTy,code),x)
    fetchMemory = arr Prelude.id
    storeMemory = arr $ const ()

instance (Profunctor c, ArrowChoice c) => ArrowMemory Int () () (GlobalStateT Value c) where
    type Join y (GlobalStateT Value c) = ArrowComplete y (GlobalStateT Value c)
    memread sCont eCont = proc (i, (_, _, x)) -> do
        y <- (sCont -< ((),x)) <⊔> (eCont -< x)
        returnA -< (i, y)
    memstore sCont eCont = proc (i, (_, _, x)) -> do
        y <- (sCont -< x) <⊔> (eCont -< x)
        returnA -< (i, y)

instance (Arrow c, Profunctor c) => ArrowMemAddress Value Natural () (GlobalStateT Value c) where
    memaddr = arr $ const ()

toTopVal :: ValueType -> Value
toTopVal I32 = Value $ Lower $ VI32 top
toTopVal I64 = Value $ Lower $ VI64 top
toTopVal F32 = Value $ Lower $ VF32 top
toTopVal F64 = Value $ Lower $ VF64 top

instance (Profunctor c, Arrow c) => ArrowSerialize Value () ValueType LoadType StoreType (GlobalStateT Value c) where
    decode sCont = proc ((), _, valTy, x) -> sCont -< (toTopVal valTy, x)
    encode sCont = proc (_,_,_,x) -> sCont -< ((),x)

instance ArrowMemSizable sz (GlobalStateT Value c) where
    -- TODO

instance ArrowFix (Underlying (GlobalStateT v c) x y) => ArrowFix (GlobalStateT v c x y) where
    type Fix (GlobalStateT v c x y) = Fix (Underlying (GlobalStateT v c) x y)


deriving instance (ArrowComplete (FreeCompletion (GlobalState v), y) c) => ArrowComplete y (GlobalStateT v c)

invokeExported :: GlobalState Value
                      -> ModuleInstance
                      -> Text
                      -> [Value]
                      -> (Pow.Pow [String],
                           Error (Pow String)
                                 (Vector Value,
                                   (FreeCompletion (GlobalState Value),
                                     Except (Exc Value)
                                            (AbsList Value, [Value]))))
invokeExported store modInst funcName args =
    let ?fixpointAlgorithm = Function.fix in -- TODO: we need something else here
    Trans.run
    (Generic.invokeExported ::
      ValueT Value
        (ReaderT Generic.LabelArities
          (DebuggableStackT Value
            (ExceptT (Exc Value)
              (GlobalStateT Value
                (FrameT FrameData Value
                  (ErrorT (Pow String)
                    (LoggerT String
                      (->)))))))) (Text, [Value]) [Value]) (Pow.singleton [],(Vector $ Vec.empty,((0,modInst),(Lower store,([],(Generic.LabelArities [],(funcName, args)))))))

instantiate :: ValidModule -> IO (Either String (ModuleInstance, GlobalState Value))
instantiate valMod = do
    res <- Wasm.instantiate emptyStore emptyImports valMod
    case res of
        Right (modInst, store) -> do
            wasmStore <- storeToGlobalState store
            return $ Right $ (modInst, wasmStore)
        Left e -> return $ Left e

    where
        storeToGlobalState (Wasm.Store funcI tableI memI globalI) = do
            globs <- Vec.mapM convertGlobals globalI
            return $ GlobalState (Vec.map convertFuncs funcI)
                                 (Vec.map TableInst tableI)
                                 globs

        convertFuncs (Wasm.FunctionInstance t m c) = FuncInst t m c
        convertFuncs (Wasm.HostInstance t _) = HostInst t

        convertGlobals (Wasm.GIConst _ v) = return $ Lower $ GlobInst Const (alpha v)
        convertGlobals (Wasm.GIMut _ v) = do
            val <- readIORef v
            return $ Lower $ GlobInst Mutable (alpha val)
