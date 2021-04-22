{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Abstract where

import           Data
--import           Concrete (TableInst, GlobInst)
import           GenericInterpreter()

import           Data.Hashable
import           Data.Order
import           Data.Text.Prettyprint.Doc

import           GHC.Generics

import qualified Language.Wasm.Interpreter as Wasm

--data IsZero = Zero | NotZero | Top deriving (Eq, Show, Generic)
--
--instance Hashable IsZero
--
--instance PreOrd IsZero where
--    _ ⊑ Top = True
--    _ ⊑ _ = False
--
--instance Complete IsZero where
--    x ⊔ y | x == y = x
--          | otherwise = Top
--
--instance UpperBounded IsZero where
--    top = Top
--
data BaseValue ai32 ai64 af32 af64 = VI32 ai32 | VI64 ai64 | VF32 af32 | VF64 af64 deriving (Show, Eq, Generic)
instance (Hashable ai32, Hashable ai64, Hashable af32, Hashable af64) => Hashable (BaseValue ai32 ai64 af32 af64)
instance (Show ai32, Show ai64, Show af32, Show af64) => Pretty (BaseValue ai32 ai64 af32 af64) where
    pretty = viaShow

instance (PreOrd ai32, PreOrd ai64, PreOrd af32, PreOrd af64) => PreOrd (BaseValue ai32 ai64 af32 af64) where
    (VI32 v1) ⊑ (VI32 v2) = v1 ⊑ v2
    (VI64 v1) ⊑ (VI64 v2) = v1 ⊑ v2
    (VF32 v1) ⊑ (VF32 v2) = v1 ⊑ v2
    (VF64 v1) ⊑ (VF64 v2) = v1 ⊑ v2
    _ ⊑ _ = False

instance (Complete ai32, Complete ai64, Complete af32, Complete af64) => Complete (BaseValue ai32 ai64 af32 af64) where
    (VI32 v1) ⊔ (VI32 v2) = VI32 $ v1 ⊔ v2
    (VI64 v1) ⊔ (VI64 v2) = VI64 $ v1 ⊔ v2
    (VF32 v1) ⊔ (VF32 v2) = VF32 $ v1 ⊔ v2
    (VF64 v1) ⊔ (VF64 v2) = VF64 $ v1 ⊔ v2
    _         ⊔ _         = error "least upper bound of base values with different types is unsupported"

type Tables = JoinVector TableInst

newtype TableInst = TableInst Wasm.TableInstance deriving (Show,Eq,Generic)

instance PreOrd TableInst where
  (⊑) = error "TODO: implement TableInst.⊑"

instance Complete TableInst where
  (⊔) = error "TODO: implement TableInst.⊔"

instance Hashable TableInst

data Size = Size deriving (Eq,Show)
data Addr = Addr deriving (Eq,Show)
data Bytes = Bytes deriving (Eq,Show)

--data GlobalState v = GlobalState {
--    funcInstances :: Vector FuncInst,
--    tableInstances :: Vector TableInst,
--    globalInstances:: Vector (FreeCompletion (GlobInst v))
--} deriving (Show,Eq,Generic)
--
--instance (Show v) => Pretty (GlobalState v) where pretty = viaShow
--
--instance (Hashable v) => Hashable (GlobalState v)

--instance (PreOrd v) => PreOrd (GlobalState v) where
--    s1 ⊑ s2 = let gs1 = globalInstances s1 in
--              let gs2 = globalInstances s2 in
--              Vec.all id $ Vec.zipWith (⊑) gs1 gs2
--
--instance (Complete v) => Complete (FreeCompletion (GlobalState v)) where
--    -- assert f1 = f2 and t1 = t2, TODO: do we need to check this?
--    (Lower (GlobalState f1 t1 g1)) ⊔ (Lower (GlobalState f2 t2 g2))
--        | f1 == f2 && t1 == t2 = Lower $ GlobalState f1 t1 (Vec.zipWith (⊔) g1 g2)
--        | otherwise = FC.Top
--    _ ⊔ _ = FC.Top
