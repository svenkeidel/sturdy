{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module TaintAnalysisValue where

import           GenericInterpreter hiding (Exc)

import           Data(joinList1'')
import qualified UnitAnalysisValue as Abs

import           Control.Arrow
import           Control.Arrow.Except
import           Control.Arrow.Order

import           Control.Arrow.Transformer.Value

import           Data.Hashable
import           Data.HashSet as HashSet
import           Data.Order
import           Data.Coerce (coerce)

import           Prettyprinter
import           GHC.Generics

data Taint = Tainted | Untainted | Top deriving (Eq, Show, Generic, Hashable)
data Value v = Value Taint v deriving (Eq, Show, Generic, Hashable)

instance Pretty Taint where
    pretty = viaShow

instance (Show v) => Pretty (Value v) where
    pretty = viaShow

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

instance (Hashable v, ArrowExcept (Abs.Exc (Value v)) c, ArrowChoice c) => IsException (Abs.Exc (Value v)) (Value v) (ValueT (Value v) c) where
  type JoinExc y (ValueT (Value v) c) = ArrowComplete y (ValueT (Value v) c)
  exception = arr $ Abs.Exc . HashSet.singleton
  handleException f = proc (Abs.Exc excs,x) ->
    joinList1'' f -< (HashSet.toList excs,x)

instance (JoinVal v (ValueT v c), IsVal v (ValueT v c), ArrowChoice c) => IsVal (Value v) (ValueT (Value v) c) where
  type JoinVal y (ValueT (Value v) c) = JoinVal y (ValueT v c)

  i32const = untainted i32const
  i64const = untainted i64const
  f32const = untainted f32const
  f64const = untainted f64const

  iUnOp = proc (bs,op,Value t v) -> do
    v' <- liftValueT iUnOp -< (bs,op,v)
    returnA -< Value t v'

  iBinOp = proc (bs,op,Value t1 v1, Value t2 v2) -> do
    v <- liftValueT iBinOp -< (bs,op,v1,v2)
    returnA -< Value (t1 ⊔ t2) v

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

  fUnOp = proc (bs,op,Value t v) -> do
    v' <- liftValueT fUnOp -< (bs,op,v)
    returnA -< Value t v'

  fBinOp = proc (bs,op,Value t1 v1,Value t2 v2) -> do
    v <- liftValueT fBinOp -< (bs,op,v1,v2)
    returnA -< Value (t1 ⊔ t2) v

  fRelOp = proc (bs,op,Value t1 v1,Value t2 v2) -> do
    v <- liftValueT fRelOp -< (bs,op,v1,v2)
    returnA -< Value (t1 ⊔ t2) v

  i32WrapI64 = proc (Value t v) -> do
    v' <- liftValueT i32WrapI64 -< v
    returnA -< Value t v'

  iTruncFU = proc (bs1,bs2,Value t v) -> do
    v' <- liftValueT iTruncFU -< (bs1,bs2,v)
    returnA -< Value t v'

  iTruncFS = proc (bs1,bs2,Value t v) -> do
    v' <- liftValueT iTruncFS -< (bs1,bs2,v)
    returnA -< Value t v'

  i64ExtendSI32 = proc (Value t v) -> do
    v' <- liftValueT i64ExtendSI32 -< v
    returnA -< Value t v'

  i64ExtendUI32 = proc (Value t v) -> do
    v' <- liftValueT i64ExtendUI32 -< v
    returnA -< Value t v'

  fConvertIU = proc (bs1,bs2,Value t v) -> do
    v' <- liftValueT fConvertIU -< (bs1,bs2,v)
    returnA -< Value t v'

  fConvertIS = proc (bs1,bs2,Value t v) -> do
    v' <- liftValueT fConvertIS -< (bs1,bs2,v)
    returnA -< Value t v'

  f32DemoteF64 = proc (Value t v) -> do
    v' <- liftValueT f32DemoteF64 -< v
    returnA -< Value t v'

  f64PromoteF32 = proc (Value t v) -> do
    v' <- liftValueT f64PromoteF32 -< v
    returnA -< Value t v'

  iReinterpretF = proc (bs,Value t v) -> do
    v' <- liftValueT iReinterpretF -< (bs,v)
    returnA -< Value t v'

  fReinterpretI = proc (bs,Value t v) -> do
    v' <- liftValueT fReinterpretI -< (bs,v)
    returnA -< Value t v'

  listLookup f g = proc (Value _t v,xs,x) -> do
    liftValueT (listLookup
      (unliftValueT f)
      (unliftValueT g))
    -< (v,xs,x)

  iTruncSatFU = error "not implemented"
  iTruncSatFS = error "not implemented"
