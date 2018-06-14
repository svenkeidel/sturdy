{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Nullness where

import           Prelude hiding (id,fail,Bounded(..),Bool(..),(<),(==),(/))
import qualified Prelude as P

import qualified Data.Map as Map
import           Data.Abstract.Environment (Env)
import qualified Data.Abstract.Environment as E
import qualified Data.Abstract.Store as S
import qualified Data.Abstract.Boolean as Abs
import           Data.Abstract.Equality
import           Data.Abstract.HandleError

import           Data.Order
import qualified Data.Boolean as B

import           Control.Category hiding ((.))

import           Control.Arrow hiding ((<+>))
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

instance Equality Val where
  Top == Top = B.true
  Null == Null = Abs.Top
  NonNull == NonNull = Abs.Top
  Bottom == Bottom = B.true
  _ == _ = B.false

instance PreOrd Val where
  Bottom ⊑ _ = P.True
  _ ⊑ Top = P.True
  Null ⊑ Null = P.True
  NonNull ⊑ NonNull = P.True
  _ ⊑ _ = P.False

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

runInterp :: Interp x y -> [CompilationUnit] -> [(String,Addr)] -> [(Addr,Val)] -> MethodReader -> x -> Error (Exception Val) y
runInterp (Interp f) files env store mainMethod x =
  let compilationUnits = map (\file -> (fileName file,file)) files
      latestAddr = case map snd env ++ map fst store of
        [] -> 0
        addrs -> maximum addrs
      fields = zip (concatMap (\u -> Shared.getFieldSignatures u (\m -> Static `elem` m)) files) [latestAddr..]
  in runConst (Map.fromList compilationUnits,Map.fromList fields)
      (evalState
        (evalStore
          (runEnvironment'
            (runReader
              (runExcept f)))))
  (latestAddr + length fields,(S.fromList store,(env,(mainMethod,x))))

---- End of Program Boilerplate ----
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
  deref = arr id
  deepDeref = arr id
  defaultValue = arr (\t -> case t of
    NullType      -> Null
    (ArrayType _) -> Null
    (RefType _)   -> Null
    UnknownType   -> top
    VoidType      -> bottom
    _             -> NonNull)
  instanceOf = arr $ const NonNull
  cast = proc ((v,_),_) -> joined returnA fail -< (v,DynamicException NonNull)
  readIndex = arr fst
  updateIndex = U.void $ arr id
  readField = arr fst
  updateField = U.void $ arr id
  case_ f = U.pi2 >>> map snd ^>> lubA f
  catch f = proc (v,clauses) ->
    joined (lubA f) fail -< (zip (repeat v) clauses,DynamicException v)

cmp_ :: UseBool Abs.Bool Val c => c (Immediate,Immediate) Abs.Bool
cmp_ = proc (v1,v2) -> returnA -< Abs.True

instance UseBool Abs.Bool Val Interp where
  eq = cmp_
  neq = cmp_
  gt = cmp_
  ge = cmp_
  lt = cmp_
  le = cmp_
  if_ f1 f2 = proc (v,(x,y)) -> case v of
    Abs.True -> f1 -< x
    Abs.False -> f2 -< y
    Abs.Top -> joined f1 f2 -< (x,y)

instance UseMem Env Addr Interp where
  emptyEnv = arr $ const E.empty
  addrFromInt = arr id

instance UseConst Interp where
  askCompilationUnits = askConst >>^ fst
  askFields = askConst >>^ snd

---- End of Actual Evaluation methods ----
