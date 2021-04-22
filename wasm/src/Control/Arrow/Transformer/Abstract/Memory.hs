{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Transformer.Abstract.Memory where

import           Abstract (Size(..),Addr(..),Bytes(..))

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.MemAddress
import           Control.Arrow.Memory
import           Control.Arrow.Order
import           Control.Arrow.Reader
import           Control.Arrow.Serialize
import           Control.Arrow.Size
import           Control.Arrow.Stack
import           Control.Arrow.StaticGlobalState
import           Control.Arrow.Store
import           Control.Arrow.Table
import           Control.Arrow.Trans
import           Control.Arrow.WasmFrame

import           Control.Category

import           Data.Profunctor

newtype MemoryT c x y = MemoryT (c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowReader r, ArrowStaticGlobalState val, ArrowSize v sz, ArrowMemAddress base off addr,
              ArrowSerialize val dat valTy datDecTy datEncTy, ArrowTable v, ArrowJoin)

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

--instance (Arrow c, Profunctor c) => ArrowMemAddress base off Addr (MemoryT c) where
--  memaddr = arr $ const Addr


deriving instance (Arrow c, Profunctor c, ArrowComplete y c) => ArrowComplete y (MemoryT c)

instance (ArrowLift c, ArrowFix (Underlying (MemoryT c) x y)) => ArrowFix (MemoryT c x y) where
    type Fix (MemoryT c x y) = Fix (Underlying (MemoryT c) x y)
