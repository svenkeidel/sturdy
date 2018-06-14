{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Nullness where

import           Prelude hiding (id,lookup,read,fail,Bounded(..))

import           Data.Order
import qualified Data.Map as Map
import qualified Data.Boolean as B
import           Data.Abstract.Environment (Env)
import qualified Data.Abstract.Environment as E
import qualified Data.Abstract.Store as S
import qualified Data.Abstract.Boolean as Abs
import qualified Data.Abstract.Equality as Abs
import           Data.Abstract.HandleError

import           Control.Category hiding ((.))

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Environment
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Store
import qualified Control.Arrow.Utils as U
import           Control.Arrow.Abstract.Join
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Abstract.HandleExcept
import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Store

import           Syntax
import           Shared

import           Text.Printf

---- Values ----
type Constants = (CompilationUnits,Fields)

type Addr = Int
-- Remove these instances when abstract multi-env is available.
instance LowerBounded Addr where
  bottom = (-2)^(29 :: Int)
instance Complete Addr where
  a ⊔ _ = a

data Val
  = Bottom
  | Null
  | NonNull
  | Top deriving (Eq)

instance Show Val where
  show Bottom = "⊥"
  show Null = "Null"
  show NonNull = "NonNull"
  show Top = "⊤"

instance Abs.Equality Val where
  Top == _ = Abs.Top
  _ == Top = Abs.Top
  Null == Null = Abs.True
  NonNull == NonNull = Abs.Top
  Bottom == Bottom = Abs.True
  _ == _ = Abs.False

instance PreOrd Val where
  Bottom ⊑ _ = True
  _ ⊑ Top = True
  Null ⊑ Null = True
  NonNull ⊑ NonNull = True
  _ ⊑ _ = False

instance Complete Val where
  Bottom ⊔ v = v
  v ⊔ Bottom = v
  Null ⊔ Null = Null
  NonNull ⊔ NonNull = NonNull
  _ ⊔ _ = top

instance UpperBounded Val where
  top = Top

instance LowerBounded Val where
  bottom = Bottom

---- End of Values ----

---- Interp Type ----

newtype Interp x y = Interp
  (Except (Exception Val)
    (Reader MethodReader
      (Environment String Addr
        (StoreArrow Addr Val
          (State Addr
            (Const Constants (->)))))) x y)
  deriving (Category,Arrow,ArrowChoice)

deriving instance ArrowJoin Interp
deriving instance ArrowConst Constants Interp
deriving instance ArrowFail (Exception Val) Interp
deriving instance ArrowReader MethodReader Interp
deriving instance ArrowState Addr Interp
deriving instance ArrowEnv String Addr (Env String Addr) Interp
deriving instance ArrowRead Addr Val Addr Val Interp
deriving instance ArrowRead Addr Val (Exception Val) Val Interp
deriving instance ArrowWrite Addr Val Interp
deriving instance ArrowExcept x Val (Exception Val) Interp
deriving instance ArrowExcept x (Maybe Val) (Exception Val) Interp

instance (LowerBounded e, LowerBounded a) => LowerBounded (Error e a) where
  bottom = SuccessOrFail bottom bottom

deriving instance PreOrd y => PreOrd (Interp x y)
deriving instance (Complete y) => Complete (Interp x y)
deriving instance LowerBounded y => LowerBounded (Interp x y)

---- End of Interp type ----

---- Program Boilerplate ----

runInterp :: Interp x y -> [CompilationUnit] -> MethodReader -> [(String,Val)] -> x -> Error (Exception Val) y
runInterp (Interp f) files mainMethod mem x =
  runConst (Map.fromList compilationUnits,Map.fromList fields)
    (evalState
      (evalStore
        (runEnvironment'
          (runReader
            (runExcept f)))))
  (latestAddr + length fields,(S.fromList store,(env,(mainMethod,x))))
  where
    compilationUnits = map (\file -> (fileName file,file)) files
    (env,store) = unzip $ map (\((l,v),a) -> ((l,a),(a,v))) $ reverse $ zip mem [0..]
    latestAddr = case store of
      [] -> 0
      (a,_):_ -> a
    fields = zip (concatMap (\u -> Shared.getFieldSignatures u (\m -> Static `elem` m)) files) [latestAddr..]

---- End of Program Boilerplate ----

lookup_ :: Interp String Addr
lookup_ = proc x -> lookup U.pi1 fail -< (x, StaticException $ printf "Variable %s not bound" (show x))

read_ :: Interp Addr Val
read_ = proc addr -> read U.pi1 fail -< (addr, StaticException $ printf "Address %s not bound" (show addr))

instance UseVal Val Interp where
  newSimple = arr $ const NonNull
  newArray = arr $ const NonNull
  and = arr $ uncurry (⊔)
  or = arr $ uncurry (⊔)
  xor = arr $ uncurry (⊔)
  rem = arr $ uncurry (⊔)
  cmp = arr $ uncurry (⊔)
  cmpg = arr $ uncurry (⊔)
  cmpl = arr $ uncurry (⊔)
  shl = arr $ uncurry (⊔)
  shr = arr $ uncurry (⊔)
  ushr = arr $ uncurry (⊔)
  plus = arr $ uncurry (⊔)
  minus = arr $ uncurry (⊔)
  mult = arr $ uncurry (⊔)
  div = proc (v1,v2) -> joined returnA fail -< (v1 ⊔ v2,DynamicException NonNull)
  lengthOf = arr id
  neg = arr id
  doubleConstant = arr $ const NonNull
  floatConstant = arr $ const NonNull
  intConstant = arr $ const NonNull
  longConstant = arr $ const NonNull
  nullConstant = arr $ const Null
  stringConstant = arr $ const NonNull
  classConstant = arr $ const NonNull
  defaultValue = arr (\t -> case t of
    NullType      -> Null
    (ArrayType _) -> Null
    (RefType _)   -> Null
    UnknownType   -> top
    VoidType      -> bottom
    _             -> NonNull)
  instanceOf = arr $ const NonNull
  cast = proc ((v,_),_) -> joined returnA fail -< (v,DynamicException NonNull)
  readVar = lookup_ >>> read_
  updateVar f = first (first lookup_ >>> write) >>> U.pi2 >>> f
  readIndex = arr fst
  updateIndex f = first (U.void $ arr id) >>> U.pi2 >>> f
  readField = arr fst
  updateField f = first (U.void $ arr id) >>> U.pi2 >>> f
  readStaticField = proc _ -> fail -< StaticException "Not implemented yet"
  updateStaticField f = proc _ -> fail -< StaticException "Not implemented yet"
  case_ f = U.pi2 >>> map snd ^>> lubA f
  catch f = proc (v,clauses) ->
    joined (lubA f) fail -< (zip (repeat v) clauses,DynamicException v)

instance UseBool Abs.Bool Val Interp where
  eq = arr $ uncurry (Abs.==)
  neq = arr $ B.not . uncurry (Abs.==)
  gt = arr $ const Abs.Top
  ge = arr $ const Abs.Top
  lt = arr $ const Abs.Top
  le = arr $ const Abs.Top
  if_ f g = proc ((v,BoolExpr i1 op i2),(x,y)) -> case v of
    Abs.True -> f -< x
    Abs.False -> g -< y
    Abs.Top -> case (i1,op,i2) of
      (Local l,Cmpeq,NullConstant) -> narrow-< (((l,Null),x),((l,NonNull),y))
      (NullConstant,Cmpeq,Local l) -> narrow-< (((l,Null),x),((l,NonNull),y))
      (Local l,Cmpne,NullConstant) -> narrow-< (((l,NonNull),x),((l,Null),y))
      (NullConstant,Cmpne,Local l) -> narrow-< (((l,NonNull),x),((l,Null),y))
      _ -> joined f g -< (x,y)
    where
      narrow = joined
        (first ((first lookup_) >>> write) >>> U.pi2 >>> f)
        (first ((first lookup_) >>> write) >>> U.pi2 >>> g)

instance UseMem Env Addr Interp where
  emptyEnv = arr $ const E.empty
  addrFromInt = arr id

instance UseConst Interp where
  askCompilationUnits = askConst >>^ fst
  askFields = askConst >>^ snd

---- End of Actual Evaluation methods ----
