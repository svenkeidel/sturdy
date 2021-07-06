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
import           Data.Hashable
import           Data.Word
import           Data.ByteString.Builder
import           Data.ByteString.Lazy (unpack, pack)
import           Data.Binary.Get
import           Data.Vector (Vector)
import qualified Data.Vector as V

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import qualified Data.IntMap.Merge.Strict as M

import           Language.Wasm.Structure (ValueType(..))
import qualified Language.Wasm.Interpreter as Wasm

import           GHC.Generics


data StoredByte = StoredByte Word8 | TopByte
    deriving (Eq, Show, Generic)

instance Hashable StoredByte where

type Bytes = Vector StoredByte

instance PreOrd StoredByte where
    StoredByte b1 ⊑ StoredByte b2 = b1 == b2
    _ ⊑ TopByte = True
    _ ⊑ _ = False

instance Complete StoredByte where
    StoredByte b1 ⊔ StoredByte b2 | b1 == b2 = StoredByte b1
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

data Memory = TopMemory | Memory (Vector StoredByte)
    deriving (Eq, Show, Generic)

instance PreOrd Memory where
    _ ⊑ TopMemory = True
    Memory v1 ⊑ Memory v2 =
        all (\(b1,b2) -> b1 ⊑ b2) $ V.zip (V.take l v1) (V.take l v2)
      where l = min (length v1) (length v2)
    _ ⊑ _ = False

instance Complete Memory where
    TopMemory ⊔ _ = TopMemory
    _ ⊔ TopMemory = TopMemory
    Memory v1 ⊔ Memory v2 =
        if l1 == l2
            then Memory $ V.zipWith (⊔) v1 v2
            else error $ "Cannot join memories of different MemSize " ++ show (Memory v1, Memory v2)
                -- if l1 < l2
                -- then let (v2head, v2tail) = V.splitAt l1 v2 in
                --      Memory $ (V.zipWith (⊔) v1 v2head) V.++ v2tail
                -- else let (v1head, v1tail) = V.splitAt l2 v1 in
                --      Memory $ (V.zipWith (⊔) v1head v2) V.++ v1tail
      where l1 = length v1
            l2 = length v2

instance Hashable Memory where



newtype Memories = Memories (IntMap Memory)
    deriving (Eq, Show, Generic)

instance Hashable Memories where
    hashWithSalt s (Memories mems) = hashWithSalt s $ M.toList mems

instance PreOrd Memories where
    Memories m1 ⊑ Memories m2 = M.isSubmapOfBy (⊑) m1 m2

instance Complete Memories where
    Memories mems1 ⊔ Memories mems2 = Memories $ M.unionWith (⊔) mems1 mems2

freshMemories :: [MemSize] -> Memories
freshMemories sizes = Memories $ foldl (\mems (mIx, size) -> M.insert mIx (freshMem size) mems) M.empty $ zip [0..] sizes
    where  
        freshMem TopMemSize = TopMemory
        freshMem (MemSize sz) = Memory $ V.replicate sz TopByte


newtype MemoryT c x y = MemoryT (StateT Memories c x y)
    deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowLift,
              ArrowFail e, ArrowExcept e, ArrowConst r, ArrowStore var' val', ArrowRun, ArrowFrame fd val,
              ArrowStack st, ArrowReader r, ArrowGlobals val, ArrowSize v sz, ArrowEffectiveAddress base off addr,
              ArrowSerialize val dat valTy datDecTy datEncTy, ArrowTable v, ArrowJoin, ArrowFunctions)

instance (Profunctor c, ArrowChoice c) => ArrowMemory Addr Bytes MemSize (MemoryT c) where
    type Join y (MemoryT c) = ArrowComplete (Memories, y) c
    memread (MemoryT sCont) (MemoryT eCont) = MemoryT $ proc (mIx,maddr,len,x) ->
        case maddr of
            TopAddr -> (eCont -< x) <⊔> (sCont -< (V.fromList [TopByte], x))
            Addr addr -> do
                Memories mems <- get -< ()
                case mems M.! mIx of
                    TopMemory -> (eCont -< x) <⊔> (sCont -< (V.fromList [TopByte], x))
                    Memory vec ->
                        if length vec < addr + len
                        then eCont -< x
                        else sCont -< (V.slice addr len vec, x)
    memstore (MemoryT sCont) (MemoryT eCont) = MemoryT $ proc (mIx,maddr,bytes,x) ->
        case maddr of
            TopAddr -> do 
                Memories mems <- get -< ()
                put -< Memories $ M.insert mIx TopMemory mems
                (eCont -< x) <⊔> (sCont -< x)
            Addr addr -> do
                Memories mems <- get -< ()
                case mems M.! mIx of 
                    TopMemory -> (eCont -< x) <⊔> (sCont -< x)
                    Memory vec ->
                        if length vec < addr + V.length bytes 
                        then eCont -< x
                        else do
                            let writtenMem = Memory $ vec V.// (zip [addr..] $ V.toList bytes)
                            put -< Memories $ M.insert mIx writtenMem mems
                            sCont -< x
    memsize = MemoryT $ proc mIx -> do
        Memories mems <- get -< ()
        case mems M.! mIx of
            TopMemory -> returnA -< TopMemSize
            Memory vec -> returnA -< MemSize $ V.length vec
    memgrow (MemoryT sCont) (MemoryT eCont) = MemoryT $ proc (mIx,madditionalSpace,x) -> do
        Memories mems <- get -< ()
        case mems M.! mIx of
            TopMemory -> (sCont -< (TopMemSize, x)) <⊔> (eCont -< x)
            Memory vec ->
                case madditionalSpace of
                    TopMemSize -> do
                        put -< Memories $ M.insert mIx TopMemory mems
                        (sCont -< (TopMemSize, x)) <⊔> (eCont -< x)
                    MemSize additionalSpace -> do
                        let additional = V.replicate additionalSpace TopByte
                        let extendedMem = Memory $ vec V.++ additional
                        put -< Memories $ M.insert mIx extendedMem mems
                        (sCont -< (MemSize $ V.length vec + additionalSpace, x)) <⊔> (eCont -< x)



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





