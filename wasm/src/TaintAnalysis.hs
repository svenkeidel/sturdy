{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module TaintAnalysis where

import           Abstract
import           Data(joinList1'')
import           GenericInterpreter
import qualified UnitAnalysis as Abs

import           Control.Arrow
import           Control.Arrow.Except
import           Control.Arrow.Order

import           Control.Arrow.Transformer.Value

import           Language.Wasm.Structure (BitSize(..), IBinOp(..), IRelOp(..), ValueType(..), IUnOp(..),
                                          FUnOp(..), FBinOp(..), FRelOp(..))

import           Data.Hashable
import           Data.HashSet as HashSet
import           Data.Order
import           Data.Text.Prettyprint.Doc as Pretty
import           GHC.Generics
import Data.Coerce (coerce)

data Taint = Tainted | Untainted | Top deriving (Eq, Show, Generic, Hashable)
data Value v = Value Taint v deriving (Eq, Show, Generic, Hashable)

instance PreOrd Taint where
  _ ⊑ Top = True
  Tainted ⊑ Tainted = True
  Untainted ⊑ Untainted = True
  _ ⊑ _ = False

instance Complete Taint where
  Tainted ⊔ Tainted = Tainted
  Untainted ⊔ Untainted = Untainted
  _ ⊔ _ = Top

instance PreOrd v => PreOrd (Value v) where
  Value t1 v1 ⊑ Value t2 v2 = t1 ⊑ t2 && v1 ⊑ v2

instance Complete v => Complete (Value v) where
  Value t1 v1 ⊔ Value t2 v2 = Value (t1 ⊔ t2) (v1 ⊔ v2)

-- valueI32, valueI64, valueF32, valueF64 :: Value
-- valueI32 = Value $ VI32 ()
-- valueI64 = Value $ VI64 ()
-- valueF32 = Value $ VF32 ()
-- valueF64 = Value $ VF64 ()

untainted :: Arrow c => ValueT v c x v -> ValueT (Value v) c x (Value v)
untainted f = proc x -> do
  v <- liftValueT f -< x
  returnA -< Value Untainted v
{-# INLINE untainted #-}

liftValueT :: ValueT v c x y -> ValueT (Value v) c x y
liftValueT = coerce
{-# INLINE liftValueT #-}

unliftValueT :: ValueT (Value v) c x y -> ValueT v c x y
unliftValueT = coerce
{-# INLINE unliftValueT #-}

liftValueT1 :: (ValueT v c x y -> ValueT v c x' y') -> (ValueT (Value v) c x y -> ValueT (Value v) c x' y')
liftValueT1 = coerce
{-# INLINE liftValueT1 #-}

--instance (ArrowExcept (Exc Value) c, ArrowChoice c) => IsException (Exc Value) (Value v) (ValueT (Value v) c) where
--  exception = proc (Exc (Value v)) ->
--    liftValueT exception -< (Exc v)

instance (Hashable v, ArrowExcept (Abs.Exc (Value v)) c, ArrowChoice c) => IsException (Abs.Exc (Value v)) (Value v) (ValueT (Value v) c) where
  type JoinExc y (ValueT (Value v) c) = ArrowComplete y (ValueT (Value v) c)
  exception = arr $ Abs.Exc . HashSet.singleton
  handleException f = proc (Abs.Exc excs,x) ->
    joinList1'' f -< (HashSet.toList excs,x)

--instance (ArrowExcept (Exc (Value v)) c) => IsException (Exc (Value v)) (Value v) (ValueT (Value v) c) where
--  type JoinExc y (ValueT (Value v) c) = ()
--  exception = arr id
--  handleException = id

instance (JoinVal v (ValueT v c), IsVal v (ValueT v c), ArrowChoice c) => IsVal (Value v) (ValueT (Value v) c) where
  type JoinVal y (ValueT (Value v) c) = JoinVal y (ValueT v c)

  i32const = untainted i32const
  i64const = untainted i64const
  f32const = untainted f32const
  f64const = untainted f64const

  iUnOp = proc (bs,op,Value t v) -> do
    v' <- liftValueT iUnOp -< (bs,op,v)
    returnA -< Value t v'

  iBinOp eCont sCont = proc (bs,op,Value t1 v1, Value t2 v2,x) ->
    liftValueT (iBinOp
      (proc (op,v1,v2,(t1,t2,_,x)) -> (unliftValueT eCont) -< (op,Value t1 v1,Value t2 v2,x))
      (proc (v,(_,_,t,x)) -> (unliftValueT sCont) -< (Value t v,x)))
      -< (bs,op,v1,v2,(t1,t2,t1 ⊔ t2,x))

  iRelOp = proc (bs,op,Value t1 v1, Value t2 v2) -> do
    v <- liftValueT iRelOp -< (bs,op,v1,v2)
    returnA -< Value (t1 ⊔ t2) v

  i32eqz = proc (Value t v) -> do
    v' <- liftValueT i32eqz -< v
    returnA -< Value t v'

  i64eqz = proc (Value t v) -> do
    v' <- liftValueT i64eqz -< v
    returnA -< Value t v'

  i32ifNeqz f g = proc (Value _t v, x) ->
    liftValueT (i32ifNeqz
      (unliftValueT f)
      (unliftValueT g))
      -< (v, x)

  ifHasType f g = proc (Value _t v,valTy,x) -> do
    liftValueT (ifHasType
      (unliftValueT f)
      (unliftValueT g))
      -< (v,valTy,x)

--     fUnOp = proc (bs,op,Value v) -> case (bs,op,v) of
--         (BS32, FAbs,     VF32 _) -> returnA -< valueF32
--         (BS32, FNeg,     VF32 _) -> returnA -< valueF32
--         (BS32, FCeil,    VF32 _) -> returnA -< valueF32
--         (BS32, FFloor,   VF32 _) -> returnA -< valueF32
--         (BS32, FTrunc,   VF32 _) -> returnA -< valueF32
--         (BS32, FNearest, VF32 _) -> returnA -< valueF32
--         (BS32, FSqrt,    VF32 _) -> returnA -< valueF32
--         (BS64, FAbs,     VF64 _) -> returnA -< valueF64
--         (BS64, FNeg,     VF64 _) -> returnA -< valueF64
--         (BS64, FCeil,    VF64 _) -> returnA -< valueF64
--         (BS64, FFloor,   VF64 _) -> returnA -< valueF64
--         (BS64, FTrunc,   VF64 _) -> returnA -< valueF64
--         (BS64, FNearest, VF64 _) -> returnA -< valueF64
--         (BS64, FSqrt,    VF64 _) -> returnA -< valueF64
--         _ -> returnA -< error "fUnOp: cannot apply operator to arguements"
--     fBinOp = proc (bs,op,Value v1,Value v2) -> case (bs,op,v1,v2) of
--         (BS32, FAdd,      VF32 _, VF32 _) -> returnA -< valueF32
--         (BS32, FSub,      VF32 _, VF32 _) -> returnA -< valueF32
--         (BS32, FMul,      VF32 _, VF32 _) -> returnA -< valueF32
--         (BS32, FDiv,      VF32 _, VF32 _) -> returnA -< valueF32
--         (BS32, FMin,      VF32 _, VF32 _) -> returnA -< valueF32
--         (BS32, FMax,      VF32 _, VF32 _) -> returnA -< valueF32
--         (BS32, FCopySign, VF32 _, VF32 _) -> returnA -< valueF32
--         (BS64, FAdd,      VF64 _, VF64 _) -> returnA -< valueF64
--         (BS64, FSub,      VF64 _, VF64 _) -> returnA -< valueF64
--         (BS64, FMul,      VF64 _, VF64 _) -> returnA -< valueF64
--         (BS64, FDiv,      VF64 _, VF64 _) -> returnA -< valueF64
--         (BS64, FMin,      VF64 _, VF64 _) -> returnA -< valueF64
--         (BS64, FMax,      VF64 _, VF64 _) -> returnA -< valueF64
--         (BS64, FCopySign, VF64 _, VF64 _) -> returnA -< valueF64
--         _ -> returnA -< error "fBinOp: cannot apply binary operator to given arguments."
--     fRelOp = proc (bs,op,Value v1,Value v2) -> case (bs,op,v1,v2) of
--         (BS32, FEq, VF32 _, VF32 _) -> returnA -< valueI32
--         (BS32, FNe, VF32 _, VF32 _) -> returnA -< valueI32
--         (BS32, FLt, VF32 _, VF32 _) -> returnA -< valueI32
--         (BS32, FGt, VF32 _, VF32 _) -> returnA -< valueI32
--         (BS32, FLe, VF32 _, VF32 _) -> returnA -< valueI32
--         (BS32, FGe, VF32 _, VF32 _) -> returnA -< valueI32
--         (BS64, FEq, VF64 _, VF64 _) -> returnA -< valueI32
--         (BS64, FNe, VF64 _, VF64 _) -> returnA -< valueI32
--         (BS64, FLt, VF64 _, VF64 _) -> returnA -< valueI32
--         (BS64, FGt, VF64 _, VF64 _) -> returnA -< valueI32
--         (BS64, FLe, VF64 _, VF64 _) -> returnA -< valueI32
--         (BS64, FGe, VF64 _, VF64 _) -> returnA -< valueI32
--         _ -> returnA -< error "fRelOp: cannot apply binary operator to given arguments."
--     i32WrapI64 = proc (Value v) -> case v of
--         (VI64 _) -> returnA -< valueI32
--         _ -> returnA -< error "i32WrapI64: cannot apply operator to given argument."
--     iTruncFU eCont = proc (bs1,bs2,x@(Value v)) -> case (bs1,bs2,v) of
--         (BS32, BS32, VF32 _) -> (returnA -< valueI32) <⊔> (eCont -< (bs1,bs2,x))
--         (BS32, BS64, VF64 _) -> (returnA -< valueI32) <⊔> (eCont -< (bs1,bs2,x))
--         (BS64, BS32, VF32 _) -> (returnA -< valueI64) <⊔> (eCont -< (bs1,bs2,x))
--         (BS64, BS64, VF64 _) -> (returnA -< valueI64) <⊔> (eCont -< (bs1,bs2,x))
--         _ -> returnA -< error "iTruncFU: cannot apply operator to given argument."
--     iTruncFS eCont = proc (bs1,bs2,x@(Value v)) -> case (bs1,bs2,v) of
--         (BS32, BS32, VF32 _) -> (returnA -< valueI32) <⊔> (eCont -< (bs1,bs2,x))
--         (BS32, BS64, VF64 _) -> (returnA -< valueI32) <⊔> (eCont -< (bs1,bs2,x))
--         (BS64, BS32, VF32 _) -> (returnA -< valueI64) <⊔> (eCont -< (bs1,bs2,x))
--         (BS64, BS64, VF64 _) -> (returnA -< valueI64) <⊔> (eCont -< (bs1,bs2,x))
--         _ -> returnA -< error "iTruncFS: cannot apply operator to given argument."
--     i64ExtendSI32 = proc (Value v) -> case v of
--         (VI32 _) -> returnA -< valueI64
--         _ -> returnA -< error "i64ExtendSI32: cannot apply operator to given argument."
--     i64ExtendUI32 = proc (Value v) -> case v of
--         (VI32 _) -> returnA -< valueI64
--         _ -> returnA -< error "i64ExtendUI32: cannot apply operator to given argument."
--     fConvertIU = proc (bs1,bs2,Value v) -> case (bs1,bs2,v) of
--         (BS32, BS32, VI32 _) -> returnA -< valueF32
--         (BS32, BS64, VI64 _) -> returnA -< valueF32
--         (BS64, BS32, VI32 _) -> returnA -< valueF64
--         (BS64, BS64, VI64 _) -> returnA -< valueF64
--         _ -> returnA -< error "fConvertIU: cannot apply operator to given argument."
--     fConvertIS = proc (bs1,bs2,Value v) -> case (bs1,bs2,v) of
--         (BS32, BS32, VI32 _) -> returnA -< valueF32
--         (BS32, BS64, VI64 _) -> returnA -< valueF32
--         (BS64, BS32, VI32 _) -> returnA -< valueF64
--         (BS64, BS64, VI64 _) -> returnA -< valueF64
--         _ -> returnA -< error "fConvertIS: cannot apply operator to given argument."
--     f32DemoteF64 = proc (Value v) -> case v of
--         (VF64 _) -> returnA -< valueF32
--         _ -> returnA -< error "f32DemoteF64: cannot apply operator to given argument."
--     f64PromoteF32 = proc (Value v) -> case v of
--         (VF32 _) -> returnA -< valueF64
--         _ -> returnA -< error "f64PromoteF32: cannot apply operator to given argument."
--     iReinterpretF = proc (bs,Value v) -> case (bs,v) of
--         (BS32, VF32 _) -> returnA -< valueI32
--         (BS64, VF64 _) -> returnA -< valueI64
--         _ -> returnA -< error "iReinterpretF: cannot apply operator to given argument."
--     fReinterpretI = proc (bs,Value v) -> case (bs,v) of
--         (BS32, VI32 _) -> returnA -< valueF32
--         (BS64, VI64 _) -> returnA -< valueF64
--         _ -> returnA -< error "fReinterpretI: cannot apply operator to given argument."
--     listLookup sCont eCont = proc (Value v,xs,x) -> case v of
--         (VI32 _) -> do
--             (joinList1'' (proc (x,()) -> sCont -< x) -< (xs,())) <⊔> (eCont -< x)
--         _ -> returnA -< error "listLookup: cannot apply operator to given arguments."

-- deriving instance ArrowComplete () c => ArrowComplete () (ValueT v c)
-- deriving instance ArrowComplete v c => ArrowComplete v (ValueT v c)
