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

import           Abstract (BaseValue)
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
import           Data.Coerce (coerce)
import           Data.Profunctor (Profunctor)

import           Prettyprinter

instance PreOrd Concrete.Value where
    a ⊑ b = a == b

instance (ArrowExcept (Unit.Exc (ConstantOr v)) c, ArrowChoice c, Hashable v) => IsException (Unit.Exc (ConstantOr v)) (ConstantOr v) (ValueT (ConstantOr v) c) where
    type JoinExc y (ValueT (ConstantOr v) c) = ArrowComplete y (ValueT (ConstantOr v) c)
    exception = arr $ Unit.Exc . HashSet.singleton
    handleException f = proc (Unit.Exc excs,x) -> do
                            joinList1'' f -< (HashSet.toList excs,x)


data ConstantOr v = NotConstant v | Constant Concrete.Value
    deriving (Eq, Show)

class LiftConstant v where
    liftConstant :: Concrete.Value -> v

instance LiftConstant Unit.Value where
    liftConstant (Concrete.Value v) = Unit.alpha v


constantValue :: Wasm.Value -> ConstantOr v
constantValue = Constant . Concrete.Value

notConstant :: BaseValue () () () () -> ConstantOr Unit.Value
notConstant = NotConstant . Unit.Value

instance Pretty v => Pretty (ConstantOr v) where
    pretty (NotConstant t) = pretty t
    pretty (Constant (Concrete.Value v)) = viaShow v

instance Hashable v => Hashable (ConstantOr v) where
    hashWithSalt s (NotConstant t) = hashWithSalt s t
    hashWithSalt s (Constant (Concrete.Value c)) = case c of
        Wasm.VI32 w32 -> hashWithSalt s w32
        Wasm.VI64 w64 -> hashWithSalt s w64
        Wasm.VF32 f -> hashWithSalt s f
        Wasm.VF64 d -> hashWithSalt s d

instance (PreOrd v, LiftConstant v) => PreOrd (ConstantOr v) where
    Constant c1 ⊑ Constant c2 = c1 == c2
    Constant c1 ⊑ NotConstant t2 = liftConstant c1 ⊑ t2
    NotConstant t1 ⊑ NotConstant t2 = t1 ⊑ t2
    _ ⊑ _ = False

instance (Complete v, LiftConstant v) => Complete (ConstantOr v) where
    Constant c1 ⊔ Constant c2 | c1 == c2 = Constant c1
    v1 ⊔ v2 = NotConstant $ asNotConstant v1 ⊔ asNotConstant v2

constant :: Arrow c => ValueT Concrete.Value c x Concrete.Value -> ValueT (ConstantOr v) c x (ConstantOr v)
constant f = proc x -> do
  v <- liftConcreteValueT f -< x
  returnA -< Constant v
{-# INLINE constant #-}

liftConcreteValueT :: ValueT Concrete.Value c x y -> ValueT (ConstantOr v) c x y
liftConcreteValueT = coerce
{-# INLINE liftConcreteValueT #-}

unliftConcreteValueT :: ValueT (ConstantOr v) c x y -> ValueT Concrete.Value c x y
unliftConcreteValueT = coerce
{-# INLINE unliftConcreteValueT #-}

unit :: Arrow c => ValueT v c x v -> ValueT (ConstantOr v) c x (ConstantOr v)
unit f = proc x -> do
  v <- liftNotConstantT f -< x
  returnA -< NotConstant v
{-# INLINE unit #-}

liftNotConstantT :: ValueT v c x y -> ValueT (ConstantOr v) c x y
liftNotConstantT = coerce
{-# INLINE liftNotConstantT #-}

unliftNotConstantT :: ValueT (ConstantOr v) c x y -> ValueT v c x y
unliftNotConstantT = coerce
{-# INLINE unliftNotConstantT #-}

-- unitConstant :: Concrete.Value -> Unit.Value
-- unitConstant (Concrete.Value c) = Unit.alpha c
-- {-# INLINE unitConstant #-}

asNotConstant :: LiftConstant v => ConstantOr v -> v
asNotConstant (Constant c) = liftConstant c
asNotConstant (NotConstant t) = t
{-# INLINE asNotConstant #-}

-- instance (Arrow c, Profunctor c, Complete y) => ArrowComplete y (ValueT (ConstantOr v) c) where
--     f <⊔> g = proc x -> do
--         y1 <- f -< x
--         y2 <- g -< x
--         returnA -< y1 ⊔ y2

instance (Arrow c, Profunctor c) => ArrowComplete y (ValueT v c) where
    f <⊔> g = unliftNotConstantT $ liftNotConstantT f <⊔> liftNotConstantT g

instance (ArrowChoice c, ArrowFail Err c, IsVal v (ValueT v c), LiftConstant v, JoinVal v (ValueT v c),
    Fail.Join (ConstantOr v) c, Fail.Join Concrete.Value c, Fail.Join Unit.Value c
    ) => IsVal (ConstantOr v) (ValueT (ConstantOr v) c) where

    type JoinVal y (ValueT (ConstantOr v) c) = (JoinVal y (ValueT v c))

    i32const = constant i32const
    i64const = constant i64const
    f32const = constant f32const
    f64const = constant f64const

    iUnOp = proc (bs,op,v0) -> case v0 of
        Constant c0 -> constant iUnOp -< (bs, op, c0)
        NotConstant t0 -> unit iUnOp -< (bs, op, t0)

    iBinOp = proc (bs,op,v1,v2) -> case (v1,v2) of
        (Constant c1, Constant c2) -> constant iBinOp -< (bs, op, c1, c2)
        _ -> unit iBinOp -< (bs, op, asNotConstant v1, asNotConstant v2)

    iRelOp = proc (bs,op,v1,v2) -> case (v1,v2) of
        (Constant c1, Constant c2) -> constant iRelOp -< (bs, op, c1, c2)
        _ -> unit iRelOp -< (bs, op, asNotConstant v1, asNotConstant v2)

    i32eqz = proc v -> case v of
        Constant c -> constant i32eqz -< c
        NotConstant t -> unit i32eqz -< t
    i64eqz = proc v -> case v of
        Constant c -> constant i64eqz -< c
        NotConstant t -> unit i64eqz -< t

    i32ifNeqz f g = proc (v, x) -> case v of
        Constant c -> liftConcreteValueT (i32ifNeqz (unliftConcreteValueT f) (unliftConcreteValueT g)) -< (c, x)
        NotConstant t -> liftNotConstantT (i32ifNeqz (unliftNotConstantT f) (unliftNotConstantT g)) -< (t, x)

    ifHasType f g = proc (v,t,x) -> case v of
        Constant c -> liftConcreteValueT (ifHasType (unliftConcreteValueT f) (unliftConcreteValueT g)) -< (c, t, x)
        NotConstant t' -> liftNotConstantT (ifHasType (unliftNotConstantT f) (unliftNotConstantT g)) -< (t', t, x)

    fUnOp = proc (bs,op,v0) -> case v0 of
        Constant c0 -> constant fUnOp -< (bs, op, c0)
        NotConstant t0 -> unit fUnOp -< (bs, op, t0)
    
    fBinOp = proc (bs,op,v1,v2) -> case (v1,v2) of
        (Constant c1, Constant c2) -> constant fBinOp -< (bs, op, c1, c2)
        _ -> unit fBinOp -< (bs, op, asNotConstant v1, asNotConstant v2)

    fRelOp = proc (bs,op,v1,v2) -> case (v1,v2) of
        (Constant c1, Constant c2) -> constant fRelOp -< (bs, op, c1, c2)
        _ -> unit fRelOp -< (bs, op, asNotConstant v1, asNotConstant v2)

    i32WrapI64 = proc v -> case v of
        Constant c -> constant i32WrapI64 -< c
        NotConstant t -> unit i32WrapI64 -< t

    iTruncFU = proc (bs1, bs2, v) -> case v of
        Constant c -> constant iTruncFU -< (bs1, bs2, c)
        NotConstant t -> unit iTruncFU -< (bs1, bs2, t)

    iTruncFS = proc (bs1, bs2, v) -> case v of
        Constant c -> constant iTruncFS -< (bs1, bs2, c)
        NotConstant t -> unit iTruncFS -< (bs1, bs2, t)
    
    i64ExtendSI32 = proc v -> case v of
        Constant c -> constant i64ExtendSI32 -< c
        NotConstant t -> unit i64ExtendSI32 -< t

    i64ExtendUI32 = proc v -> case v of
        Constant c -> constant i64ExtendUI32 -< c
        NotConstant t -> unit i64ExtendUI32 -< t

    fConvertIU = proc (bs1, bs2, v) -> case v of
        Constant c -> constant fConvertIU -< (bs1, bs2, c)
        NotConstant t -> unit fConvertIU -< (bs1, bs2, t)

    fConvertIS = proc (bs1, bs2, v) -> case v of
        Constant c -> constant fConvertIS -< (bs1, bs2, c)
        NotConstant t -> unit fConvertIS -< (bs1, bs2, t)
    
    f32DemoteF64 = proc v -> case v of
        Constant c -> constant f32DemoteF64 -< c
        NotConstant t -> unit f32DemoteF64 -< t

    f64PromoteF32 = proc v -> case v of
        Constant c -> constant f64PromoteF32 -< c
        NotConstant t -> unit f64PromoteF32 -< t
    
    iReinterpretF = proc (bs, v) -> case v of
        Constant c -> constant iReinterpretF -< (bs, c)
        NotConstant t -> unit iReinterpretF -< (bs, t)

    fReinterpretI = proc (bs, v) -> case v of
        Constant c -> constant fReinterpretI -< (bs, c)
        NotConstant t -> unit fReinterpretI -< (bs, t)

    listLookup sCont eCont = proc (v, xs, x) -> case v of
        Constant c -> liftConcreteValueT (listLookup (unliftConcreteValueT sCont) (unliftConcreteValueT eCont)) -< (c, xs, x)
        NotConstant t -> liftNotConstantT (listLookup (unliftNotConstantT sCont) (unliftNotConstantT eCont)) -< (t, xs, x)

    iTruncSatFU = proc (bs1, bs2, v) -> case v of
        Constant c -> constant iTruncSatFU -< (bs1, bs2, c)
        NotConstant t -> unit iTruncSatFU -< (bs1, bs2, t)

    iTruncSatFS = proc (bs1, bs2, v) -> case v of
        Constant c -> constant iTruncSatFS -< (bs1, bs2, c)
        NotConstant t -> unit iTruncSatFS -< (bs1, bs2, t)

