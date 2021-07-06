{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ConstantPropagationValue where

import           Data(joinList1'')
import           GenericInterpreter hiding (Exc)
import qualified Concrete as Concrete
import           ConcreteInterpreter ()
import qualified UnitAnalysisValue as Unit

import           Control.Arrow
import           Control.Arrow.Except
import           Control.Arrow.Fail as Fail
import           Control.Arrow.Order

import           Control.Arrow.Transformer.Value

import qualified Language.Wasm.Interpreter as Wasm

import           Data.Hashable
import           Data.HashSet as HashSet
import           Data.Order
import           Data.Text.Prettyprint.Doc as Pretty
import           Data.Coerce (coerce)
import           Data.Profunctor (Profunctor)

instance PreOrd Concrete.Value where
    a ⊑ b = a == b

instance (ArrowExcept (Unit.Exc Value) c, ArrowChoice c) => IsException (Unit.Exc Value) Value (ValueT Value c) where
    type JoinExc y (ValueT Value c) = ArrowComplete y (ValueT Value c)
    exception = arr $ Unit.Exc . HashSet.singleton
    handleException f = proc (Unit.Exc excs,x) -> do
                            joinList1'' f -< (HashSet.toList excs,x)


data Value = UnitValue Unit.Value | Constant Concrete.Value
    deriving (Eq, Show)

instance Pretty Value where
    pretty (UnitValue t) = pretty t
    pretty (Constant (Concrete.Value v)) = viaShow v

instance Hashable Value where
    hashWithSalt s (UnitValue t) = hashWithSalt s t
    hashWithSalt s (Constant (Concrete.Value c)) = case c of
        Wasm.VI32 w32 -> hashWithSalt s w32
        Wasm.VI64 w64 -> hashWithSalt s w64
        Wasm.VF32 f -> hashWithSalt s f
        Wasm.VF64 d -> hashWithSalt s d

instance PreOrd Value where
    Constant c1 ⊑ Constant c2 = c1 == c2
    Constant c1 ⊑ UnitValue t2 = unitConstant c1 == t2
    UnitValue t1 ⊑ UnitValue t2 = t1 ⊑ t2
    _ ⊑ _ = False

instance Complete Value where
    Constant c1 ⊔ Constant c2 =
        if c1 == c2
        then Constant $ c1
        else UnitValue $ unitConstant c1 ⊔ unitConstant c2
    Constant c1 ⊔ UnitValue t2 = UnitValue $ unitConstant c1 ⊔ t2
    UnitValue t1 ⊔ Constant c2 = UnitValue $ t1 ⊔ unitConstant c2
    UnitValue t1 ⊔ UnitValue t2 = UnitValue $ t1 ⊔ t2

constant :: Arrow c => ValueT Concrete.Value c x Concrete.Value -> ValueT Value c x Value
constant f = proc x -> do
  v <- liftConcreteValueT f -< x
  returnA -< Constant v
{-# INLINE constant #-}

liftConcreteValueT :: ValueT Concrete.Value c x y -> ValueT Value c x y
liftConcreteValueT = coerce
{-# INLINE liftConcreteValueT #-}

unliftConcreteValueT :: ValueT Value c x y -> ValueT Concrete.Value c x y
unliftConcreteValueT = coerce
{-# INLINE unliftConcreteValueT #-}

unit :: Arrow c => ValueT Unit.Value c x Unit.Value -> ValueT Value c x Value
unit f = proc x -> do
  v <- liftUnitValueT f -< x
  returnA -< UnitValue v
{-# INLINE unit #-}

liftUnitValueT :: ValueT Unit.Value c x y -> ValueT Value c x y
liftUnitValueT = coerce
{-# INLINE liftUnitValueT #-}

unliftUnitValueT :: ValueT Value c x y -> ValueT Unit.Value c x y
unliftUnitValueT = coerce
{-# INLINE unliftUnitValueT #-}

unitConstant :: Concrete.Value -> Unit.Value
unitConstant (Concrete.Value c) = Unit.alpha c
{-# INLINE unitConstant #-}

unitValue :: Value -> Unit.Value
unitValue (Constant c) = unitConstant c
unitValue (UnitValue t) = t
{-# INLINE unitValue #-}

instance (Arrow c, Profunctor c, Complete y) => ArrowComplete y (ValueT Value c) where
    f <⊔> g = proc x -> do
        y1 <- f -< x
        y2 <- g -< x
        returnA -< y1 ⊔ y2

instance (Arrow c, Profunctor c, ArrowComplete y (ValueT Value c)) => ArrowComplete y (ValueT Unit.Value c) where
    f <⊔> g = unliftUnitValueT $ liftUnitValueT f <⊔> liftUnitValueT g

instance (ArrowChoice c, ArrowFail Err c, 
    Fail.Join Value c, Fail.Join Concrete.Value c, Fail.Join Unit.Value c
    ) => IsVal Value (ValueT Value c) where

    type JoinVal y (ValueT Value c) = ArrowComplete y (ValueT Value c)

    i32const = constant i32const
    i64const = constant i64const
    f32const = constant f32const
    f64const = constant f64const

    iUnOp = proc (bs,op,v0) -> case v0 of
        Constant c0 -> constant iUnOp -< (bs, op, c0)
        UnitValue t0 -> unit iUnOp -< (bs, op, t0)

    iBinOp = proc (bs,op,v1,v2) -> case (v1,v2) of
        (Constant c1, Constant c2) -> constant iBinOp -< (bs, op, c1, c2)
        _ -> unit iBinOp -< (bs, op, unitValue v1, unitValue v2)

    iRelOp = proc (bs,op,v1,v2) -> case (v1,v2) of
        (Constant c1, Constant c2) -> constant iRelOp -< (bs, op, c1, c2)
        _ -> unit iRelOp -< (bs, op, unitValue v1, unitValue v2)

    i32eqz = proc v -> case v of
        Constant c -> constant i32eqz -< c
        UnitValue t -> unit i32eqz -< t
    i64eqz = proc v -> case v of
        Constant c -> constant i64eqz -< c
        UnitValue t -> unit i64eqz -< t

    i32ifNeqz f g = proc (v, x) -> case v of
        Constant c -> liftConcreteValueT (i32ifNeqz (unliftConcreteValueT f) (unliftConcreteValueT g)) -< (c, x)
        UnitValue t -> liftUnitValueT (i32ifNeqz (unliftUnitValueT f) (unliftUnitValueT g)) -< (t, x)

    ifHasType f g = proc (v,t,x) -> case v of
        Constant c -> liftConcreteValueT (ifHasType (unliftConcreteValueT f) (unliftConcreteValueT g)) -< (c, t, x)
        UnitValue t' -> liftUnitValueT (ifHasType (unliftUnitValueT f) (unliftUnitValueT g)) -< (t', t, x)

    fUnOp = proc (bs,op,v0) -> case v0 of
        Constant c0 -> constant fUnOp -< (bs, op, c0)
        UnitValue t0 -> unit fUnOp -< (bs, op, t0)
    
    fBinOp = proc (bs,op,v1,v2) -> case (v1,v2) of
        (Constant c1, Constant c2) -> constant fBinOp -< (bs, op, c1, c2)
        _ -> unit fBinOp -< (bs, op, unitValue v1, unitValue v2)

    fRelOp = proc (bs,op,v1,v2) -> case (v1,v2) of
        (Constant c1, Constant c2) -> constant fRelOp -< (bs, op, c1, c2)
        _ -> unit fRelOp -< (bs, op, unitValue v1, unitValue v2)

    i32WrapI64 = proc v -> case v of
        Constant c -> constant i32WrapI64 -< c
        UnitValue t -> unit i32WrapI64 -< t

    iTruncFU = proc (bs1, bs2, v) -> case v of
        Constant c -> constant iTruncFU -< (bs1, bs2, c)
        UnitValue t -> unit iTruncFU -< (bs1, bs2, t)

    iTruncFS = proc (bs1, bs2, v) -> case v of
        Constant c -> constant iTruncFS -< (bs1, bs2, c)
        UnitValue t -> unit iTruncFS -< (bs1, bs2, t)
    
    i64ExtendSI32 = proc v -> case v of
        Constant c -> constant i64ExtendSI32 -< c
        UnitValue t -> unit i64ExtendSI32 -< t

    i64ExtendUI32 = proc v -> case v of
        Constant c -> constant i64ExtendUI32 -< c
        UnitValue t -> unit i64ExtendUI32 -< t

    fConvertIU = proc (bs1, bs2, v) -> case v of
        Constant c -> constant fConvertIU -< (bs1, bs2, c)
        UnitValue t -> unit fConvertIU -< (bs1, bs2, t)

    fConvertIS = proc (bs1, bs2, v) -> case v of
        Constant c -> constant fConvertIS -< (bs1, bs2, c)
        UnitValue t -> unit fConvertIS -< (bs1, bs2, t)
    
    f32DemoteF64 = proc v -> case v of
        Constant c -> constant f32DemoteF64 -< c
        UnitValue t -> unit f32DemoteF64 -< t

    f64PromoteF32 = proc v -> case v of
        Constant c -> constant f64PromoteF32 -< c
        UnitValue t -> unit f64PromoteF32 -< t
    
    iReinterpretF = proc (bs, v) -> case v of
        Constant c -> constant iReinterpretF -< (bs, c)
        UnitValue t -> unit iReinterpretF -< (bs, t)

    fReinterpretI = proc (bs, v) -> case v of
        Constant c -> constant fReinterpretI -< (bs, c)
        UnitValue t -> unit fReinterpretI -< (bs, t)

    listLookup sCont eCont = proc (v, xs, x) -> case v of
        Constant c -> liftConcreteValueT (listLookup (unliftConcreteValueT sCont) (unliftConcreteValueT eCont)) -< (c, xs, x)
        UnitValue t -> liftUnitValueT (listLookup (unliftUnitValueT sCont) (unliftUnitValueT eCont)) -< (t, xs, x)

    iTruncSatFU = proc (bs1, bs2, v) -> case v of
        Constant c -> constant iTruncSatFU -< (bs1, bs2, c)
        UnitValue t -> unit iTruncSatFU -< (bs1, bs2, t)

    iTruncSatFS = proc (bs1, bs2, v) -> case v of
        Constant c -> constant iTruncSatFS -< (bs1, bs2, c)
        UnitValue t -> unit iTruncSatFS -< (bs1, bs2, t)

