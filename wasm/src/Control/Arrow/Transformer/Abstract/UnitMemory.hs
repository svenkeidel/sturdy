{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Transformer.Abstract.UnitMemory where

import           Abstract (BaseValue(..))
import           UnitAnalysisValue (Value(..),valueI32,unitValue)
import qualified TaintAnalysisValue as Taint

import           Data

import           Control.Arrow
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

import           Data.Coerce (coerce)
import           Data.Profunctor

import           Language.Wasm.Structure (ValueType(..))



data Size = Size deriving (Eq,Show)
data Addr = Addr deriving (Eq,Show)
data Bytes = Bytes deriving (Eq,Show)


-- Memory

newtype MemoryT c x y = MemoryT (c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowReader r, ArrowGlobals val, ArrowSize v sz, ArrowEffectiveAddress base off addr,
              ArrowSerialize val dat valTy datDecTy datEncTy, ArrowTable v, ArrowJoin, ArrowFunctions)

instance ArrowTrans MemoryT where
  -- lift' :: c x y -> MemoryT v c x y
  lift' = MemoryT

instance (Profunctor c, ArrowChoice c) => ArrowMemory Addr Bytes Size (MemoryT c) where
  type Join y (MemoryT c) = ArrowComplete y (MemoryT c)
  memread sCont eCont = proc (_,Addr,_,x) -> (sCont -< (Bytes,x)) <⊔> (eCont -< x)
  memstore sCont eCont = proc (_,Addr,Bytes,x) -> (sCont -< x) <⊔> (eCont -< x)
  memsize = arr $ const Size
  memgrow sCont eCont = proc (_,Size,x) -> (sCont -< (Size,x)) <⊔> (eCont -< x)

--instance (ArrowChoice c, Profunctor c) => ArrowSize Value () (MemoryT c) where
--  valToSize = proc (Value v) -> case v of
--    (VI32 _) -> returnA -< ()
--    _ -> returnA -< error "valToSize: arguments needs to be an i32 integer."
--  sizeToVal = proc () -> returnA -< valueI32

--instance (Arrow c, Profunctor c) => ArrowEffectiveAddress base off Addr (MemoryT c) where
--  memaddr = arr $ const Addr


deriving instance (Arrow c, Profunctor c, ArrowComplete y c) => ArrowComplete y (MemoryT c)

instance (ArrowLift c, ArrowFix (Underlying (MemoryT c) x y)) => ArrowFix (MemoryT c x y) where
    type Fix (MemoryT c x y) = Fix (Underlying (MemoryT c) x y)


-- Serializable

newtype SerializeT v c x y = SerializeT (c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowState s, ArrowGlobals v, ArrowReader m, ArrowTable v, ArrowFunctions,
              ArrowMemory addr bytes sz, ArrowSize v sz, ArrowJoin, ArrowEffectiveAddress base off addr)

instance ArrowTrans (SerializeT v) where
    lift' = SerializeT

instance (Profunctor c, Arrow c) => ArrowSerialize Value Bytes ValueType LoadType StoreType (SerializeT Value c) where
    decode = proc (Bytes,_,valTy) -> returnA -< unitValue valTy
    encode = arr $ const Bytes


tainted :: Arrow c => SerializeT v c x v -> SerializeT (Taint.Value v) c x (Taint.Value v)
tainted f = proc x -> do
  v <- liftSerializeT f -< x
  returnA -< Taint.Value Taint.Tainted v
{-# INLINE tainted #-}

liftSerializeT :: SerializeT v c x y -> SerializeT (Taint.Value v) c x y
liftSerializeT = coerce
{-# INLINE liftSerializeT #-}

unliftSerializeT :: SerializeT (Taint.Value v) c x y -> SerializeT v c x y
unliftSerializeT = coerce
{-# INLINE unliftSerializeT #-}

instance (Arrow c, ArrowSerialize v Bytes ValueType LoadType StoreType (SerializeT v c)) => ArrowSerialize (Taint.Value v) Bytes ValueType LoadType StoreType (SerializeT (Taint.Value v) c) where
    decode = proc (bytes,datDecTy,valTy) -> do
        v <- liftSerializeT decode -< (bytes,datDecTy,valTy)
        returnA -< Taint.Value Taint.Top v
          
    encode = proc (Taint.Value _t v,valTy,datEncTy) -> 
        liftSerializeT encode -< (v,valTy,datEncTy)

deriving instance (Arrow c, Profunctor c, ArrowComplete y c) => ArrowComplete y (SerializeT v c)

instance (ArrowLift c, ArrowFix (Underlying (SerializeT v c) x y)) => ArrowFix (SerializeT v c x y) where
    type Fix (SerializeT v c x y) = Fix (Underlying (SerializeT v c) x y)



-- EffectiveAddress

newtype EffectiveAddressT c x y = EffectiveAddressT (c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowReader r, ArrowGlobals val, ArrowFunctions, ArrowSize v sz,
              ArrowSerialize val dat valTy datDecTy datEncTy, ArrowTable v, ArrowJoin)

instance ArrowTrans EffectiveAddressT where
  -- lift' :: c x y -> MemoryT v c x y
  lift' = EffectiveAddressT

instance (Arrow c, Profunctor c) => ArrowEffectiveAddress base off Addr (EffectiveAddressT c) where
  effectiveAddress = arr $ const Addr


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

instance (ArrowChoice c, Profunctor c) => ArrowSize Value Size (SizeT Value c) where
  valToSize = proc (Value v) -> case v of
    (VI32 _) -> returnA -< Size
    _        -> returnA -< error "valToSize: argument needs to be an i32 integer."

  sizeToVal = proc Size -> returnA -< valueI32


untainted :: Arrow c => SizeT v c x v -> SizeT (Taint.Value v) c x (Taint.Value v)
untainted f = proc x -> do
  v <- liftSizeT f -< x
  returnA -< Taint.Value Taint.Untainted v
{-# INLINE untainted #-}

liftSizeT :: SizeT v c x y -> SizeT (Taint.Value v) c x y
liftSizeT = coerce
{-# INLINE liftSizeT #-}

instance (Arrow c, ArrowSize v Size (SizeT v c)) => ArrowSize (Taint.Value v) Size (SizeT (Taint.Value v) c) where
    valToSize = proc (Taint.Value _t v) -> do
        liftSizeT valToSize -< v

    sizeToVal = proc Size ->
        untainted sizeToVal -< Size

deriving instance (Arrow c, Profunctor c, ArrowComplete y c) => ArrowComplete y (SizeT v c)

instance (ArrowLift c, ArrowFix (Underlying (SizeT v c) x y)) => ArrowFix (SizeT v c x y) where
    type Fix (SizeT v c x y) = Fix (Underlying (SizeT v c) x y)
