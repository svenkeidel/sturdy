{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Abstract where

import           Concrete (TableInst, GlobInst, FuncInst)
import           GenericInterpreter hiding (Top)

import           Control.Arrow

import           Control.Arrow.Transformer.Value

import           Data.Abstract.FreeCompletion hiding (Top)
import qualified Data.Abstract.FreeCompletion as FC
import           Data.Hashable
import           Data.Order
import           Data.Vector (Vector)
import qualified Data.Vector as Vec

import           GHC.Generics

import           Language.Wasm.Structure hiding (exports, Const)

data IsZero = Zero | NotZero | Top deriving (Eq, Show, Generic)

instance Hashable IsZero

instance PreOrd IsZero where
    _ ⊑ Top = True
    _ ⊑ _ = False

instance Complete IsZero where
    x ⊔ y | x == y = x
          | otherwise = Top

instance UpperBounded IsZero where
    top = Top

data BaseValue ai32 ai64 af32 af64 = VI32 ai32 | VI64 ai64 | VF32 af32 | VF64 af64 deriving (Show, Eq, Generic)
instance (Hashable ai32, Hashable ai64, Hashable af32, Hashable af64) => Hashable (BaseValue ai32 ai64 af32 af64)

instance (PreOrd ai32, PreOrd ai64, PreOrd af32, PreOrd af64) => PreOrd (BaseValue ai32 ai64 af32 af64) where
    (VI32 v1) ⊑ (VI32 v2) = v1 ⊑ v2
    (VI64 v1) ⊑ (VI64 v2) = v1 ⊑ v2
    (VF32 v1) ⊑ (VF32 v2) = v1 ⊑ v2
    (VF64 v1) ⊑ (VF64 v2) = v1 ⊑ v2
    _ ⊑ _ = False

instance (Complete ai32, Complete ai64, Complete af32, Complete af64) => Complete (FreeCompletion (BaseValue ai32 ai64 af32 af64)) where
    (Lower (VI32 v1)) ⊔ (Lower (VI32 v2)) = Lower $ VI32 $ v1 ⊔ v2
    (Lower (VI64 v1)) ⊔ (Lower (VI64 v2)) = Lower $ VI64 $ v1 ⊔ v2
    (Lower (VF32 v1)) ⊔ (Lower (VF32 v2)) = Lower $ VF32 $ v1 ⊔ v2
    (Lower (VF64 v1)) ⊔ (Lower (VF64 v2)) = Lower $ VF64 $ v1 ⊔ v2
    _ ⊔ _ = FC.Top

data GlobalState v = GlobalState {
    funcInstances :: Vector FuncInst,
    tableInstances :: Vector TableInst,
    globalInstances:: Vector (FreeCompletion (GlobInst v))
} deriving (Show,Eq,Generic)

instance (Hashable v) => Hashable (GlobalState v)

instance (PreOrd v) => PreOrd (GlobalState v) where
    s1 ⊑ s2 = let gs1 = globalInstances s1 in
              let gs2 = globalInstances s2 in
              Vec.all id $ Vec.zipWith (⊑) gs1 gs2

instance (Complete v) => Complete (FreeCompletion (GlobalState v)) where
    -- assert f1 = f2 and t1 = t2, TODO: do we need to check this?
    (Lower (GlobalState f1 t1 g1)) ⊔ (Lower (GlobalState f2 t2 g2))
        | f1 == f2 && t1 == t2 = Lower $ GlobalState f1 t1 (Vec.zipWith (⊔) g1 g2)
        | otherwise = FC.Top
    _ ⊔ _ = FC.Top
