{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Transformer.Abstract.KnownAddressMemory where

import           Prelude hiding ((.))

import           Abstract (BaseValue(..))

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

import           Control.Arrow.Transformer.State

import           Control.Category

import           Data.Coerce (coerce)
import           Data.Profunctor
import           Data.Order
import           Data.Word
import           Data.ByteString.Builder
import           Data.ByteString.Lazy (unpack, pack)
import           Data.Binary.Get
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           Language.Wasm.Structure (ValueType(..))
import qualified Language.Wasm.Interpreter as Wasm


data StoredByte = StoredByte Word8 | TopByte
    deriving (Eq, Show)

type Bytes = Vector StoredByte


instance PreOrd StoredByte where
    StoredByte b1 ⊑ StoredByte b2 = b1 == b2
    _ ⊑ TopByte = True
    _ ⊑ _ = False

instance Complete StoredByte where
    StoredByte b1 ⊔ StoredByte b2 | b1 == b2 = StoredByte b1
    _ ⊔ _ = TopByte

-- -- Serializable

newtype SerializeT v c x y = SerializeT (c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowState s, ArrowGlobals v, ArrowReader m, ArrowTable v, ArrowFunctions,
              ArrowMemory addr bytes sz, ArrowSize v sz, ArrowJoin, ArrowEffectiveAddress base off addr)

instance ArrowTrans (SerializeT v) where
    lift' = SerializeT

-- encodeBytes :: Int -> [Word8] -> [Byte]
-- encodeBytes len = map (\(ixL,ixR,b) -> Byte b ixL ixR) . zip3 [0..] [len-1,len-2..]
encodeBytes :: Int -> [Word8] -> Bytes
encodeBytes _ = V.fromList . map StoredByte

-- decodeBytes :: [Byte] -> [Word8]
-- decodeBytes = map byteValue
decodeBytes :: Bytes -> Maybe [Word8]
decodeBytes = V.foldr op (Just []) 
    where
        op _ Nothing = Nothing
        op TopByte _ = Nothing
        op (StoredByte b) (Just bs) = Just $ b:bs


encodeConcreteValue :: Wasm.Value -> Bytes
encodeConcreteValue (Wasm.VI32 w32) = 
    encodeBytes 4 $ unpack $ toLazyByteString $ word32LE w32
encodeConcreteValue (Wasm.VI64 w64) = 
    encodeBytes 8 $ unpack $ toLazyByteString $ word64LE w64
encodeConcreteValue (Wasm.VF32 f32) = 
    encodeBytes 4 $ unpack $ toLazyByteString $ floatLE f32
encodeConcreteValue (Wasm.VF64 f64) = 
    encodeBytes 8 $ unpack $ toLazyByteString $ doubleLE f64

decodeConcreteValue :: ValueType -> Bytes -> Maybe Wasm.Value
decodeConcreteValue I32 = 
   fmap (Wasm.VI32 . runGet getWord32le . pack) . decodeBytes
decodeConcreteValue I64 = 
   fmap (Wasm.VI64 . runGet getWord64le . pack) . decodeBytes
decodeConcreteValue F32 = 
   fmap (Wasm.VF32 . runGet getFloatle . pack) . decodeBytes
decodeConcreteValue F64 = 
   fmap (Wasm.VF64 . runGet getDoublele . pack) . decodeBytes


-- instance (Profunctor c, Arrow c) => ArrowSerialize Value Bytes ValueType LoadType StoreType (SerializeT Value c) where
--     decode sCont = proc (Bytes,_,valTy,x) -> sCont -< (toTopVal valTy, x)
--         where
--             toTopVal I32 = Value $ VI32 top
--             toTopVal I64 = Value $ VI64 top
--             toTopVal F32 = Value $ VF32 top
--             toTopVal F64 = Value $ VF64 top
--     encode sCont = proc (_,_,_,x) -> sCont -< (Bytes,x)


deriving instance (Arrow c, Profunctor c, ArrowComplete y c) => ArrowComplete y (SerializeT v c)

instance (ArrowLift c, ArrowFix (Underlying (SerializeT v c) x y)) => ArrowFix (SerializeT v c x y) where
    type Fix (SerializeT v c x y) = Fix (Underlying (SerializeT v c) x y)




-- -- Memory

-- This abstraction assumes the size of the memory cannot grow unconditionally.

data Memory = Memory (Vector StoredByte)
    deriving (Eq, Show)

instance PreOrd Memory where
    Memory v1 ⊑ Memory v2 =
        all (\(b1,b2) -> b1 ⊑ b2) $ V.zip (V.take l v1) (V.take l v2)
      where l = min (length v1) (length v2)

instance Complete Memory where
    Memory v1 ⊔ Memory v2 =
        if l1 == l2
            then Memory $ V.zipWith (⊔) v1 v2
            else error $ "Cannot join memories of different size " ++ show (Memory v1, Memory v2)
                -- if l1 < l2
                -- then let (v2head, v2tail) = V.splitAt l1 v2 in
                --      Memory $ (V.zipWith (⊔) v1 v2head) V.++ v2tail
                -- else let (v1head, v1tail) = V.splitAt l2 v1 in
                --      Memory $ (V.zipWith (⊔) v1head v2) V.++ v1tail
      where l1 = length v1
            l2 = length v2


newtype MemoryT c x y = MemoryT (StateT (Map Int Memory) c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowReader r, ArrowGlobals val, ArrowSize v sz, ArrowEffectiveAddress base off addr,
              ArrowSerialize val dat valTy datDecTy datEncTy, ArrowTable v, ArrowJoin, ArrowFunctions)

instance (Profunctor c, ArrowChoice c) => ArrowMemory Int Bytes Int (MemoryT c) where
    type Join y (MemoryT c) = ArrowComplete (Map Int Memory, y) c
    memread (MemoryT sCont) (MemoryT eCont) = MemoryT $ proc (mIx,addr,len,x) -> do
        mems <- get -< ()
        let Memory vec = mems M.! mIx
        if length vec < addr + len
            then eCont -< x
            else let bytes = V.slice addr len vec in
                 sCont -< (bytes, x)
    memstore (MemoryT sCont) (MemoryT eCont) = MemoryT $ proc (mIx,addr,bytes,x) -> do
        mems <- get -< ()
        let Memory vec = mems M.! mIx
        if length vec < addr + V.length bytes 
            then eCont -< x
            else do
                let writtenMem = Memory $ vec V.// (zip [addr..] $ V.toList bytes)
                put -< M.insert mIx writtenMem mems
                sCont -< x
    memsize = MemoryT $ proc mIx -> do
        mems <- get -< ()
        let Memory vec = mems M.! mIx
        returnA -< V.length vec
    memgrow (MemoryT sCont) (MemoryT eCont) = MemoryT $ proc (mIx,additionalSpace,x) -> do
        mems <- get -< ()
        let Memory vec = mems M.! mIx
        let additional = V.replicate additionalSpace TopByte
        let extendedMem = Memory $ vec V.++ additional
        put -< M.insert mIx extendedMem mems
        (sCont -< (V.length vec + additionalSpace, x)) <⊔> (eCont -< x)

-- instance (ArrowChoice c, Profunctor c) => ArrowSize Value () (MemoryT c) where
--     valToSize = proc (Value v) -> case v of
--         (VI32 _) -> returnA -< ()
--         _ -> returnA -< error "valToSize: arguments needs to be an i32 integer."
--     sizeToVal = proc () -> returnA -< valueI32

-- --instance (Arrow c, Profunctor c) => ArrowEffectiveAddress base off Addr (MemoryT c) where
-- --  memaddr = arr $ const Addr


deriving instance (Arrow c, Profunctor c, ArrowComplete (Map Int Memory, y) c) => ArrowComplete y (MemoryT c)

instance (ArrowLift c, ArrowFix (Underlying (MemoryT c) x y)) => ArrowFix (MemoryT c x y) where
    type Fix (MemoryT c x y) = Fix (Underlying (MemoryT c) x y)




-- -- EffectiveAddress

-- newtype EffectiveAddressT c x y = EffectiveAddressT (c x y)
--     deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
--               ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
--               ArrowStack st, ArrowReader r, ArrowGlobals val, ArrowFunctions, ArrowSize v sz,
--               ArrowSerialize val dat valTy datDecTy datEncTy, ArrowTable v, ArrowJoin)

-- instance ArrowTrans EffectiveAddressT where
--   -- lift' :: c x y -> MemoryT v c x y
--   lift' = EffectiveAddressT

-- instance (Arrow c, Profunctor c) => ArrowEffectiveAddress base off Addr (EffectiveAddressT c) where
--   effectiveAddress = arr $ const Addr


-- deriving instance (Arrow c, Profunctor c, ArrowComplete y c) => ArrowComplete y (EffectiveAddressT c)

-- instance (ArrowLift c, ArrowFix (Underlying (EffectiveAddressT c) x y)) => ArrowFix (EffectiveAddressT c x y) where
--     type Fix (EffectiveAddressT c x y) = Fix (Underlying (EffectiveAddressT c) x y)




-- -- ArrowSize

-- newtype SizeT v c x y = SizeT (c x y)
--     deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
--               ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
--               ArrowStack st, ArrowReader r, ArrowGlobals val, ArrowMemory addr bytes s,
--               ArrowEffectiveAddress base off addr, ArrowFunctions,
--               ArrowSerialize val dat valTy datDecTy datEncTy, ArrowTable v, ArrowJoin)

-- instance ArrowTrans (SizeT v) where
--   -- lift' :: c x y -> MemoryT v c x y
--   lift' = SizeT

-- instance (ArrowChoice c, Profunctor c) => ArrowSize Value Size (SizeT Value c) where
--   valToSize = proc (Value v) -> case v of
--     (VI32 _) -> returnA -< Size
--     _        -> returnA -< error "valToSize: argument needs to be an i32 integer."

--   sizeToVal = proc Size -> returnA -< valueI32


-- deriving instance (Arrow c, Profunctor c, ArrowComplete y c) => ArrowComplete y (SizeT v c)

-- instance (ArrowLift c, ArrowFix (Underlying (SizeT v c) x y)) => ArrowFix (SizeT v c x y) where
--     type Fix (SizeT v c x y) = Fix (Underlying (SizeT v c) x y)
