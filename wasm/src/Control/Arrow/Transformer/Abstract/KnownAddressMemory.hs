{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Control.Arrow.Transformer.Abstract.KnownAddressMemory where

import           Prelude hiding ((.))

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

import           Data.Profunctor
import           Data.Order
import           Data.Hashable
import           Data.Word
import           Data.ByteString.Builder
import           Data.ByteString.Lazy (unpack, pack)
import           Data.Binary.Get
import           Data.Vector (Vector)
import qualified Data.Vector as V

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

import           Prettyprinter

import           Language.Wasm.Structure (ValueType(..))
import qualified Language.Wasm.Interpreter as Wasm

import           GHC.Generics


data Byte = Byte Word8 | TopByte
    deriving (Eq, Show, Generic)

instance Hashable Byte where

type Bytes = Vector Byte

instance PreOrd Byte where
    Byte b1 ⊑ Byte b2 = b1 == b2
    _ ⊑ TopByte = True
    _ ⊑ _ = False

instance Complete Byte where
    Byte b1 ⊔ Byte b2 | b1 == b2 = Byte b1
    _ ⊔ _ = TopByte

-- Serialize

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
encodeBytes _ = V.fromList . map Byte

-- decodeBytes :: [Byte] -> [Word8]
-- decodeBytes = map byteValue
decodeBytes :: Bytes -> [Word8]
decodeBytes = map (\(Byte b) -> b) . V.toList

encodeConcreteValue :: Wasm.Value -> Bytes
encodeConcreteValue (Wasm.VI32 w32) = 
    encodeBytes 4 $ unpack $ toLazyByteString $ word32LE w32
encodeConcreteValue (Wasm.VI64 w64) = 
    encodeBytes 8 $ unpack $ toLazyByteString $ word64LE w64
encodeConcreteValue (Wasm.VF32 f32) = 
    encodeBytes 4 $ unpack $ toLazyByteString $ floatLE f32
encodeConcreteValue (Wasm.VF64 f64) = 
    encodeBytes 8 $ unpack $ toLazyByteString $ doubleLE f64

decodeConcreteValue :: ValueType -> Bytes -> Wasm.Value
decodeConcreteValue I32 = 
   Wasm.VI32 . runGet getWord32le . pack . decodeBytes
decodeConcreteValue I64 = 
   Wasm.VI64 . runGet getWord64le . pack . decodeBytes
decodeConcreteValue F32 = 
   Wasm.VF32 . runGet getFloatle . pack . decodeBytes
decodeConcreteValue F64 = 
   Wasm.VF64 . runGet getDoublele . pack . decodeBytes


deriving instance (Arrow c, Profunctor c, ArrowComplete y c) => ArrowComplete y (SerializeT v c)

instance (ArrowLift c, ArrowFix (Underlying (SerializeT v c) x y)) => ArrowFix (SerializeT v c x y) where
    type Fix (SerializeT v c x y) = Fix (Underlying (SerializeT v c) x y)


-- Size

data MemSize = TopMemSize | MemSize Int
    deriving (Show, Eq)

newtype SizeT v c x y = SizeT (c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowReader r, ArrowGlobals val, ArrowMemory addr bytes s,
              ArrowEffectiveAddress base off addr, ArrowFunctions,
              ArrowSerialize val dat valTy datDecTy datEncTy, ArrowTable v, ArrowJoin)

instance ArrowTrans (SizeT v) where
  -- lift' :: c x y -> MemoryT v c x y
  lift' = SizeT


deriving instance (Arrow c, Profunctor c, ArrowComplete y c) => ArrowComplete y (SizeT v c)

instance (ArrowLift c, ArrowFix (Underlying (SizeT v c) x y)) => ArrowFix (SizeT v c x y) where
    type Fix (SizeT v c x y) = Fix (Underlying (SizeT v c) x y)



-- Memory

-- This abstraction assumes the MemSize of the memory cannot grow unconditionally.

data Addr = TopAddr | Addr Int
    deriving (Show, Eq)

data Memory = TopMemory | Memory (Vector Byte)
    deriving (Eq, Show, Generic)

instance PreOrd Memory where
    _ ⊑ TopMemory = True
    Memory v1 ⊑ Memory v2 | length v1 == length v2 = all (\(b1,b2) -> b1 ⊑ b2) $ V.zip v1 v2
    _ ⊑ _ = False

instance Complete Memory where
    Memory v1 ⊔ Memory v2 | length v1 == length v2 = Memory $ V.zipWith (⊔) v1 v2
    _ ⊔ _ = TopMemory
                -- if l1 < l2
                -- then let (v2head, v2tail) = V.splitAt l1 v2 in
                --      Memory $ (V.zipWith (⊔) v1 v2head) V.++ v2tail
                -- else let (v1head, v1tail) = V.splitAt l2 v1 in
                --      Memory $ (V.zipWith (⊔) v1head v2) V.++ v1tail

instance Hashable Memory where



newtype Memories = Memories (IntMap Memory)
    deriving (Eq, Show, Generic)

instance Pretty Memories where
    pretty = viaShow

instance Hashable Memories where
    hashWithSalt s (Memories mems) = hashWithSalt s $ M.toList mems

instance PreOrd Memories where
    Memories m1 ⊑ Memories m2 = M.isSubmapOfBy (⊑) m1 m2

instance Complete Memories where
    Memories mems1 ⊔ Memories mems2 = Memories $ M.unionWith (⊔) mems1 mems2

makeMemory :: [Word8] -> Memory
makeMemory bytes = Memory $ V.fromList $ map Byte bytes

makeMemories :: Vector Memory -> Memories
makeMemories mems = Memories $ M.fromList $ zip [0..] $ V.toList mems


newtype MemoryT c x y = MemoryT (StateT Memories c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowReader r, ArrowGlobals val, ArrowSize v sz, ArrowEffectiveAddress base off addr,
              ArrowSerialize val dat valTy datDecTy datEncTy, ArrowTable v, ArrowJoin, ArrowFunctions)

instance (Profunctor c, ArrowChoice c) => ArrowMemory Addr Bytes MemSize (MemoryT c) where
    type Join y (MemoryT c) = ArrowComplete (Memories, y) c
    memread (MemoryT sCont) (MemoryT eCont) = MemoryT $ proc (mIx,maddr,len,x) -> do
        Memories mems <- get -< ()
        case mems M.! mIx of
            TopMemory -> (eCont -< x) <⊔> (sCont -< (V.replicate len TopByte, x))
            Memory vec -> case maddr of
                TopAddr -> (eCont -< x) <⊔> (sCont -< (V.replicate len TopByte, x))
                Addr addr ->
                    if addr + len <= length vec
                    then sCont -< (V.slice addr len vec, x)
                    else eCont -< x
    memstore (MemoryT sCont) (MemoryT eCont) = MemoryT $ proc (mIx,maddr,bytes,x) -> do
        Memories mems <- get -< ()
        case mems M.! mIx of 
            TopMemory -> (eCont -< x) <⊔> (sCont -< x)
            Memory vec -> case maddr of
                TopAddr -> do 
                    put -< Memories $ M.insert mIx TopMemory mems
                    (eCont -< x) <⊔> (sCont -< x)
                Addr addr ->
                    if addr + V.length bytes <= length vec
                    then let updatedVec = vec V.// (zip [addr..] $ V.toList bytes) in do
                        put -< Memories $ M.insert mIx (Memory updatedVec) mems
                        sCont -< x
                    else eCont -< x
    memsize = MemoryT $ proc mIx -> do
        Memories mems <- get -< ()
        case mems M.! mIx of
            TopMemory -> returnA -< TopMemSize
            Memory vec -> returnA -< MemSize $ V.length vec `quot` pageSize
    memgrow (MemoryT sCont) (MemoryT eCont) = MemoryT $ proc (mIx,madditionalSpace,x) -> do
        -- TODO: check the maximal size of the memory
        Memories mems <- get -< ()
        case mems M.! mIx of
            TopMemory -> (sCont -< (TopMemSize, x)) <⊔> (eCont -< x)
            Memory vec -> do
                let oldsize = V.length vec `quot` pageSize
                case madditionalSpace of
                    TopMemSize -> do
                        put -< Memories $ M.insert mIx TopMemory mems
                        (sCont -< (TopMemSize, x)) <⊔> (eCont -< x)
                    MemSize delta -> do
                        let deltaVec = V.replicate (delta * pageSize) (Byte 0)
                        let extendedMem = Memory $ vec V.++ deltaVec
                        put -< Memories $ M.insert mIx extendedMem mems
                        (sCont -< (MemSize $ oldsize, x))



deriving instance (Arrow c, Profunctor c, ArrowComplete (Memories, y) c) => ArrowComplete y (MemoryT c)

instance (ArrowLift c, ArrowFix (Underlying (MemoryT c) x y)) => ArrowFix (MemoryT c x y) where
    type Fix (MemoryT c x y) = Fix (Underlying (MemoryT c) x y)


-- EffectiveAddress

newtype EffectiveAddressT c x y = EffectiveAddressT (c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowReader r, ArrowGlobals val, ArrowFunctions, ArrowSize v sz,
              ArrowSerialize val dat valTy datDecTy datEncTy, ArrowTable v, ArrowJoin)

instance ArrowTrans EffectiveAddressT where
  lift' = EffectiveAddressT


deriving instance (Arrow c, Profunctor c, ArrowComplete y c) => ArrowComplete y (EffectiveAddressT c)

instance (ArrowLift c, ArrowFix (Underlying (EffectiveAddressT c) x y)) => ArrowFix (EffectiveAddressT c x y) where
    type Fix (EffectiveAddressT c x y) = Fix (Underlying (EffectiveAddressT c) x y)





