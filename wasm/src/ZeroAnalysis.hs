{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ZeroAnalysis where

--import           Abstract
--import qualified Abstract as A
--import           GenericInterpreter as Generic
--
--import           Control.Arrow
--import           Control.Arrow.Const
--import           Control.Arrow.Logger
--import           Control.Arrow.Except
--import           Control.Arrow.Fail
--import           Control.Arrow.Fix
--import           Control.Arrow.MemAddress
--import           Control.Arrow.Memory
--import           Control.Arrow.MemSizable
--import           Control.Arrow.Reader
--import           Control.Arrow.Serialize
--import           Control.Arrow.Stack
--import           Control.Arrow.State
--import           Control.Arrow.Store
--import           Control.Arrow.Order
--import           Control.Arrow.Trans as Trans
--import           Control.Arrow.GlobalState
--import           Control.Arrow.WasmFrame
--
--import           Control.Arrow.Transformer.Abstract.Except
--import           Control.Arrow.Transformer.Abstract.Error
--
--import           Control.Arrow.Transformer.DebuggableStack
--import           Control.Arrow.Transformer.Logger
--import           Control.Arrow.Transformer.State
--import           Control.Arrow.Transformer.Reader
--import           Control.Arrow.Transformer.Value
--import           Control.Arrow.Transformer.Concrete.WasmFrame
--
--import           Control.Category
--
--import           Data.Abstract.FreeCompletion
--import           Data.Abstract.Sign
--import qualified Data.Abstract.Sign as S
--import qualified Data.Function as Function
--import           Data.Order
--import           Data.Profunctor
--import           Data.Text.Lazy (Text)
--
--import           Language.Wasm.Structure (ValueType(..))
--import           Language.Wasm.Interpreter (ModuleInstance)
--
--newtype Value = Value (FreeCompletion (BaseValue IsZero IsZero Sign Sign)) deriving (Show)
--
--instance (ArrowChoice c) => IsVal Value (ValueT Value c) where
--    i32const = proc w32 -> returnA -< Value $ Lower $ VI32 $ if w32==0 then A.Zero else NotZero
--
--newtype GlobalStateT v c x y = GlobalStateT (StateT (GlobalState v) c x y)
--    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift, ArrowReader r,
--              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowRun, ArrowFrame fd val,
--              ArrowStack st, ArrowLogger l, ArrowSerialize val dat valTy datDecTy datEncTy)
--
--instance (ArrowState s c) => ArrowState s (GlobalStateT v c) where
--    -- TODO
--
--instance ArrowTrans (GlobalStateT v) where
--    lift' a = GlobalStateT (lift' a)
--
--instance (Arrow c, Profunctor c) => ArrowGlobalState v Int (GlobalStateT v c) where
--    fetchMemory = arr Prelude.id
--    storeMemory = arr $ const ()
--
--instance (Profunctor c, ArrowChoice c) => ArrowMemory Int Value () (GlobalStateT Value c) where
--    type Join y (GlobalStateT Value c) = ArrowComplete y (GlobalStateT Value c)
--    memread sCont eCont = proc (i, (_, _, x)) -> do
--        y <- (sCont -< ((),x)) <⊔> (eCont -< x)
--        returnA -< (i, y)
--    memstore sCont eCont = proc (i, (_, _, x)) -> do
--        y <- (sCont -< x) <⊔> (eCont -< x)
--        returnA -< (i, y)
--
--toTopVal :: ValueType -> Value
--toTopVal I32 = Value $ Lower $ VI32 top
--toTopVal I64 = Value $ Lower $ VI64 top
--toTopVal F32 = Value $ Lower $ VF32 top
--toTopVal F64 = Value $ Lower $ VF64 top
--
--instance (Profunctor c, Arrow c) => ArrowSerialize Value () ValueType () () (GlobalStateT Value c) where
--    decode sCont = proc ((), _, valTy, x) -> sCont -< (toTopVal valTy, x)
--    encode sCont = proc (_,_,_,x) -> sCont -< ((),x)
--
--instance ArrowFix (Underlying (GlobalStateT v c) x y) => ArrowFix (GlobalStateT v c x y) where
--    type Fix (GlobalStateT v c x y) = Fix (Underlying (GlobalStateT v c) x y)
--
--
----invokeExported :: GlobalState Value
----                      -> ModuleInstance
----                      -> Text
----                      -> [Value]
----                      -> _
----invokeExported store modInst funcName args =
----    let ?fixpointAlgorithm = Function.fix in -- TODO: we need something else here
----    Trans.run
----    (Generic.invokeExported ::
----      ValueT Value
----        (ReaderT Generic.LabelArities
----          (DebuggableStackT Value
----            (ExceptT (Generic.Exc Value)
----              (GlobalStateT Value
----                (FrameT FrameData Value
----                  (ErrorT String
----                    (LoggerT String
----                      (->)))))))) (Text, [Value]) [Value]) _2
