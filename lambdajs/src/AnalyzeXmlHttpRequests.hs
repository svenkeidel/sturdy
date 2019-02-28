{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module AnalyzeXmlHttpRequests where

-- This implements the analysis from Section 4 of "The Essence of JavaScript", ECOOP 2010.

import           GHC.Generics (Generic)
import           Prelude hiding (break, error, fail, id, (.), lookup, map, read)

import           SharedInterpreter hiding (eval)
import qualified SharedInterpreter as Shared
import           Syntax

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Alloc
import           Control.Arrow.Conditional
import           Control.Arrow.Fail
import           Control.Arrow.Environment
import           Control.Arrow.Except
import           Control.Arrow.Fix
import           Control.Arrow.Store as Store
import           Control.Arrow.Abstract.Join

import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Store

import           Data.Foldable
import qualified Data.Label as Lab
import           Data.Order
import           Data.Profunctor
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import           Data.String

import           Data.Abstract.Powerset (Pow(..))
import qualified Data.Abstract.Powerset as Pow
import           Data.Abstract.Map (Map)
import qualified Data.Abstract.Map as M
import           Data.Abstract.Except
import           Data.Abstract.Error

import           Text.Printf

type Env = Map Ident Type

data Type
    = Top
    | SafeAny
    | SafeString
    | UnsafeString
    | TLambda [Ident] Expr Env
    deriving (Show, Eq, Generic)

instance PreOrd Type where
  Top ⊑ _ = True
  SafeAny ⊑ y = case y of
    Top -> True
    SafeAny -> True
    _ -> False
  SafeString ⊑ y = case y of
    Top -> True
    SafeAny -> True
    SafeString -> True
    _ -> False
  UnsafeString ⊑ y = case y of
    Top -> True
    UnsafeString -> True
    _ -> False
  x@(TLambda _ _ _) ⊑ y = case y of
    Top -> True
    SafeAny -> True
    TLambda _ _ _ -> x == y
    _ -> False
  
instance Complete Type where
  Top ⊔ _ = Top
  SafeAny ⊔ y = case y of
    Top -> Top
    SafeAny -> SafeAny
    SafeString -> SafeAny
    UnsafeString -> Top
    TLambda _ _ _ -> SafeAny
  SafeString ⊔ y = case y of
    Top -> Top
    SafeAny -> SafeAny
    SafeString -> SafeString
    UnsafeString -> Top
    TLambda _ _ _ -> SafeAny
  UnsafeString ⊔ y = case y of
    Top -> Top
    SafeAny -> Top
    SafeString -> Top
    UnsafeString -> UnsafeString
    TLambda _ _ _ -> Top
  x@(TLambda _ _ _) ⊔ y = case y of
    Top -> Top
    SafeAny -> SafeAny
    SafeString -> SafeString
    UnsafeString -> Top
    TLambda _ _ _ -> if x == y then x else SafeAny

type Addr = ()

data Exceptional
    = Break Label Type
    | Throw Type
    deriving (Show, Eq, Generic)

instance PreOrd Exceptional where
  x ⊑ y = x == y


eval ::
  AnalysisT (
    EnvT Ident Type (
      StoreT Addr Type (
        ErrorT (Pow String) (
          ExceptT (Pow Exceptional) (
            ->)))))
  Expr Type
eval = Shared.eval

run :: [(Ident, Type)] -> [(Addr, Type)] -> Expr -> Except (Pow Exceptional) (Error (Pow String) (Map Addr Type, Type))
run env st e =
  runExceptT (
    runErrorT (
      runStoreT (
        runEnvT (
          runAnalysisT (
            eval)))))
  (M.fromList st, (M.fromList env, e))

-- | Arrow transformer that implements the abstract value semantics
newtype AnalysisT c x y = AnalysisT { runAnalysisT :: c x y }
  deriving (Profunctor, Category, Arrow, ArrowChoice, ArrowFail f, ArrowExcept ex, ArrowStore addr val, ArrowJoin)
deriving instance ArrowFix x y c => ArrowFix x y (AnalysisT c)
deriving instance ArrowEnv var Type env c => ArrowEnv var Type env (AnalysisT c)

instance (ArrowChoice c, Profunctor c) => ArrowAlloc (Lab.Label,Type) Addr (AnalysisT c) where
  alloc = arr (const ())

unsafeStrings :: Set String
unsafeStrings = Set.fromList ["XMLHttpRequest"]

instance (ArrowChoice c,
          ArrowFail f c,
          IsString f,
          ArrowStore Addr Type c,
          ArrowJoin c,
          Store.Join c ((Type, Addr), Addr) Type
         ) => IsVal Type Env (AnalysisT c) where
    -- simple values
    numVal = arr (const SafeAny)
    boolVal = arr (const SafeAny)
    stringVal = arr (\s -> if Set.member s unsafeStrings then UnsafeString else SafeString)
    undefVal = arr (const SafeAny)
    nullVal = arr (const SafeAny)
    evalOp = evalOp_

    -- closures
    closureVal = arr $ \(env, ids, body) -> TLambda ids body env
    applyClosure f = proc (fun, args) ->
        case fun of
          TLambda names body closureEnv -> f -< ((closureEnv, names, body), args)
          _ -> fail -< fromString $ printf "Error: apply on non-lambda value: %s %s" (show fun) (show args)

    -- objects
    objectVal = arr (const SafeAny)
    getField = proc (_, f) -> case f of
      SafeString -> returnA -< Top
      SafeAny -> returnA -< Top
      _ -> failWrongType -< ("safe value", f)
    updateField = proc (_, _, _) ->
      returnA -< SafeAny
    deleteField = proc (_, _) ->
      returnA -< SafeAny


instance (ArrowChoice c, ArrowJoin c) => IsRef Type Addr (AnalysisT c) where
  type RefJoin (AnalysisT c) x y = (Complete y)

-- store ops
  ref = arr (const SafeAny)
  withRef f g = proc (e, v) ->
    (f -< (e, ((), v))) <⊔> (g -< (e, v))

failWrongType :: (ArrowChoice c, ArrowFail f c, IsString f, Show a) => c (String, a) b
failWrongType = proc (typ, a) -> fail -< fromString $ printf "Wrong type. Expected %s but found %s" typ (show a)

instance (ArrowChoice c, ArrowJoin c, ArrowExcept (Pow Exceptional) c) => IsException Type (Pow Exceptional) (AnalysisT c) where
  type ExcJoin (AnalysisT c) x a = (Complete a)

  throwExc = arr (Pow.singleton . Throw)
  breakExc = arr (Pow.singleton . uncurry Break)
  handleThrow f = proc (x, Pow es) -> case Seq.partition isThrow es of
    (throws, Seq.Empty) -> (| joinList' (\ex -> f -< (x, throwValue ex)) |) (toList throws)
    (Seq.Empty, breaks) -> throw -< Pow breaks
    (throws, breaks) -> (| joinList' (\ex -> f -< (x, throwValue ex)) |) (toList throws)  <⊔>  (throw -< Pow breaks)
    where
      isThrow e = case e of Throw _ -> True; _ -> False
      throwValue e = case e of Throw v -> v
  handleBreak f = proc (x, Pow es) -> case Seq.partition isBreak es of
    (breaks, Seq.Empty) -> (| joinList' (\ex -> f -< (x, breakLabel ex, breakValue ex)) |) (toList breaks)
    (Seq.Empty, throws) -> throw -< Pow throws
    (breaks, throws) -> (| joinList' (\ex -> f -< (x, breakLabel ex, breakValue ex)) |) (toList breaks)  <⊔>  (throw -< Pow throws)
    where
      isBreak e = case e of Break _ _ -> True; _ -> False
      breakValue e = case e of Break _ v -> v
      breakLabel e = case e of Break l _ -> l

instance (ArrowChoice c, ArrowFail f c, IsString f, ArrowJoin c) => ArrowCond Type (AnalysisT c) where
  type Join (AnalysisT c) (x,y) z = Complete (c (x,y) z)

  if_ (AnalysisT f1) (AnalysisT f2) = AnalysisT $ proc (_, (x, y)) ->
    joined f1 f2 -< (x, y)

evalOp_ :: (ArrowChoice c, ArrowFail f c, IsString f) => c (Op, [Type]) Type
evalOp_ = proc (op, args) -> case (op, args) of
    -- number operators
    (ONumPlus, _) -> returnA -< SafeAny
    (OMul, _) -> returnA -< SafeAny
    (ODiv, _) -> returnA -< SafeAny
    (OMod, _) -> returnA -< SafeAny
    (OSub, _) -> returnA -< SafeAny
    (OLt, _) -> returnA -< SafeAny
    (OToInteger, _) -> returnA -< SafeAny
    (OToInt32, _) -> returnA -< SafeAny
    (OToUInt32, _) -> returnA -< SafeAny
    -- shift operators
    (OLShift, _) -> returnA -< SafeAny
    (OSpRShift, _) -> returnA -< SafeAny
    (OZfRShift, _) -> returnA -< SafeAny
    -- string operators
    (OStrPlus, [_, _]) -> returnA -< UnsafeString
    (OStrLt, _) -> returnA -< SafeAny
    (OStrLen, _) -> returnA -< SafeAny
    (OStrStartsWith, _) -> returnA -< SafeAny
    (OStrSplitStrExp, _) -> returnA -< SafeAny
    (OStrSplitRegExp, _) -> fail -< fromString $ "Regex operations not implemented"
    (ORegExpMatch, _) -> fail -< fromString $ "Regex operations not implemented"
    (ORegExpQuote, _) -> fail -< fromString $ "Regex operations not implemented"
    -- boolean operators
    (OBAnd, _) -> returnA -< SafeAny
    (OBOr, _) -> returnA -< SafeAny
    (OBXOr, _) -> returnA -< SafeAny
    (OBNot, _) -> returnA -< SafeAny
    -- isPrimitive operator
    (OIsPrim, _) -> returnA -< SafeAny
    -- primToNum operator
    -- #todo object conversions -> valueOf call
    (OPrimToNum, _) -> returnA -< SafeAny
    -- primToStr operator
    (OPrimToStr, [a]) -> case a of
        SafeString -> returnA -< SafeString
        _ -> returnA -< UnsafeString
    -- primToBool operator
    (OPrimToBool, _) -> returnA -< SafeAny
    -- typeOf operator
    (OTypeof, _) -> returnA -< SafeString
    -- equality operators
    (OStrictEq, _) -> returnA -< SafeAny
    (OAbstractEq, _) -> returnA -< SafeAny
    -- math operators
    (OMathExp, _) -> returnA -< SafeAny
    (OMathLog, _) -> returnA -< SafeAny
    (OMathCos, _) -> returnA -< SafeAny
    (OMathSin, _) -> returnA -< SafeAny
    (OMathAbs, _) -> returnA -< SafeAny
    (OMathPow, _) -> returnA -< SafeAny
    -- object operators
    (OHasOwnProp, _) -> returnA -< SafeAny
    (OObjCanDelete, _) -> returnA -< SafeAny
    (OObjIterHasNext, _) -> returnA -< SafeAny
    (OObjIterNext, _) -> returnA -< SafeAny
    (OObjIterKey, _) -> returnA -< UnsafeString
    --
    (OSurfaceTypeof, _) -> returnA -< SafeString
    _ -> fail -< fromString $ "Unimplemented operator: " ++ (show op) ++ " with args: " ++ (show args)
