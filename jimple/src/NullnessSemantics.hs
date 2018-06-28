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
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NullnessSemantics where

import           Prelude hiding (lookup,read,fail,Bounded(..))

import qualified Data.Boolean as B
import           Data.Order
import           Data.String (fromString)

import qualified Data.Abstract.Boolean as Abs
import           Data.Abstract.Environment (Env)
import qualified Data.Abstract.Environment as E
import qualified Data.Abstract.Equality as Abs
import           Data.Abstract.Exception
import           Data.Abstract.HandleError
import qualified Data.Abstract.Store as S

import           Control.Category hiding ((.))

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Environment
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.Store
import           Control.Arrow.Abstract.Join
import qualified Control.Arrow.Utils as U

import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Abstract.HandleExcept
import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Store

import           Syntax
import           SharedSemantics

import           Data.GaloisConnection
import qualified Data.Concrete.Powerset as Con
import qualified ConcreteSemantics as Con

import           Text.Printf

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

instance Galois (Con.Pow Con.Val) Val where
  alpha = lifted $ \v -> case v of
    Con.NullVal -> Null
    _ -> NonNull
  gamma = error "noncomputable"

type Context = ([Statement],[CatchClause])

newtype Interp x y = Interp
  (Except (Exception Val)
    (Reader Context
      (Environment String Val
        (StoreArrow FieldSignature Val
          (Const [CompilationUnit]
            (->))))) x y)
  deriving (Category,Arrow,ArrowChoice)

deriving instance ArrowConst [CompilationUnit] Interp
deriving instance Complete y => ArrowExcept x y (Exception Val) Interp
deriving instance ArrowFail (Exception Val) Interp
deriving instance ArrowFix [Statement] (Maybe Val) Interp
deriving instance ArrowEnv String Val (Env String Val) Interp
deriving instance ArrowJoin Interp
deriving instance ArrowReader Context Interp
deriving instance ArrowRead FieldSignature Val x Val Interp
deriving instance ArrowWrite FieldSignature Val Interp

-- This instance is not in lib because it interferes with an instance in Stratego
instance (LowerBounded e,LowerBounded a) => LowerBounded (Error e a) where
  bottom = SuccessOrFail bottom bottom

deriving instance PreOrd y => PreOrd (Interp x y)
deriving instance (Complete y) => Complete (Interp x y)
deriving instance LowerBounded y => LowerBounded (Interp x y)

runInterp :: Interp x y ->
             [CompilationUnit] -> [(String,Val)] -> x ->
             Error (Exception Val) y
runInterp (Interp f) compilationUnits mem x =
  runConst compilationUnits
    (evalStore
      (runEnvironment'
        (runReader
          (runExcept f))))
  (fields,(mem,(([],[]),x)))
  where
    fields = S.fromList $ zip
      (concatMap (getFieldSignatures (\m -> Static `elem` m)) compilationUnits)
      (repeat Bottom)

type Out v = Error (Exception Val) v
type Mem = [(String,Val)]

runProgram' :: [CompilationUnit] -> (MethodSignature,[Immediate]) -> Out (Maybe Val)
runProgram' units = runInterp runProgram units []

runStatements' :: [CompilationUnit] -> Mem -> [Statement] -> Out (Maybe Val)
runStatements' = runInterp (initStatements runStatements)

eval' :: [CompilationUnit] -> Mem -> Expr -> Out Val
eval' = runInterp eval

evalBool' :: [CompilationUnit] -> Mem -> BoolExpr -> Out Abs.Bool
evalBool' = runInterp evalBool

evalImmediate' :: [CompilationUnit] -> Mem -> Immediate -> Out Val
evalImmediate' = runInterp evalImmediate

---- Instances -----------------------------------------------------------------

instance UseVal Val Interp where
  newSimple = proc t -> case t of
    RefType c -> do
      readCompilationUnit -< c
      returnA -< NonNull
    NullType -> returnA -< Null
    _ -> returnA -< NonNull
  newArray = proc _ -> joined returnA failStatic -<
    (NonNull,"Expected an integer array size")
  and = binopInteger
  or = binopInteger
  xor = binopInteger
  rem = binopNum
  cmp = proc _ -> joined returnA failStatic -<
    (NonNull,"Expected long variables for 'cmp'")
  cmpg = proc _ -> joined returnA failStatic -<
    (NonNull,"Expected floating variables for 'cmpg'")
  cmpl = proc _ -> joined returnA failStatic -<
    (NonNull,"Expected floating variables for 'cmpl'")
  shl = binopInteger
  shr = binopInteger
  ushr = binopInteger
  plus = binopNum
  minus = binopNum
  mult = binopNum
  div = proc (v1,v2) -> joined returnA (joined failDynamic failStatic) -< (v1 ⊔ v2,
    (NonNull,"Expected numeric variables for 'div'"))
  lengthOf = proc v -> joined returnA failStatic -<
    (v,"Expected an array variable for 'lengthOf'")
  neg = proc v -> joined returnA failStatic -<
    (v,"Expected a number as argument for -")
  doubleConstant = arr $ const NonNull
  floatConstant = arr $ const NonNull
  intConstant = arr $ const NonNull
  longConstant = arr $ const NonNull
  nullConstant = arr $ const Null
  stringConstant = arr $ const NonNull
  classConstant = arr $ const NonNull
  defaultValue = arr (\t -> case t of
    NullType      -> Null
    (RefType _)   -> Null
    (ArrayType _) -> Null
    UnknownType   -> top
    VoidType      -> bottom
    _             -> NonNull)
  instanceOf = arr $ const NonNull
  cast = proc ((v,_),_) -> joined returnA (joined failDynamic failStatic) -< (v,
    (NonNull,"Casting of primivites and arrays is not yet supported"))
  declare f = (\((l,v),x) -> (l,v,x)) ^>> extendEnv' f
  readVar = lookup_
  updateVar f = proc ((l,v),x) -> do
    lookup_ -< l
    extendEnv' f -< (l,v,x)
  readIndex = proc (v,_) -> case v of
    Bottom -> returnA -< bottom
    Null -> failDynamic -< NonNull
    NonNull -> returnA -< top
    Top -> joined returnA failDynamic -< (top,NonNull)
  updateIndex f = first (first readIndex) >>> U.pi2 >>> f
  readField = proc (v,FieldSignature _ t _) -> case v of
    Bottom -> returnA -< bottom
    Null -> failDynamic -< NonNull
    NonNull -> defaultValue >>^ (⊔ NonNull) -< t
    Top -> joined (defaultValue >>^ (⊔ NonNull)) failDynamic -< (t,NonNull)
  updateField f = first ((\(o,(v,_)) -> (o,v)) ^>> readField) >>> U.pi2 >>> f
  readStaticField = proc f -> read U.pi1 failStatic -<
    (f,printf "Field %s not bound" (show f))
  updateStaticField = proc (f,v) -> do
    readStaticField -< f
    write -< (f,v)
  case_ f = proc (_,cases) ->
    joined (lubA f) (joined failStatic failStatic) -< (map snd cases,
      ("No matching cases","Expected an integer as argument for switch"))

instance UseException Exception Val Interp where
  catch f = proc (ex,clauses) -> case ex of
    StaticException _ -> fail -< ex
    DynamicException v ->
      joined (lubA f) failDynamic -< (zip (repeat v) clauses,v)
  failDynamic = DynamicException ^>> fail
  failStatic = fromString ^>> fail

instance UseBool Abs.Bool Val Interp where
  eq = arr $ uncurry (Abs.==)
  neq = arr $ B.not . uncurry (Abs.==)
  gt = cmpBool
  ge = cmpBool
  lt = cmpBool
  le = cmpBool
  if_ f g = proc ((v,BoolExpr i1 op i2),(x,y)) -> case v of
    Abs.True -> f -< x
    Abs.False -> g -< y
    Abs.Top -> case (i1,op,i2) of
      (Local l,Cmpeq,NullConstant) -> narrow -< ((l,Null,x),(l,NonNull,y))
      (NullConstant,Cmpeq,Local l) -> narrow -< ((l,Null,x),(l,NonNull,y))
      (Local l,Cmpne,NullConstant) -> narrow -< ((l,NonNull,x),(l,Null,y))
      (NullConstant,Cmpne,Local l) -> narrow -< ((l,NonNull,x),(l,Null,y))
      _ -> joined f g -< (x,y)
    where
      narrow = joined (extendEnv' f) (extendEnv' g)

instance UseEnv (Env String Val) Interp where
  emptyEnv = arr $ const E.empty

instance UseConst Interp where
  askCompilationUnits = askConst

---- Helper Methods ------------------------------------------------------------

lookup_ :: Interp String Val
lookup_ = proc x -> lookup U.pi1 failStatic -<
  (x,printf "Variable %s not bound" (show x))

initStatements :: Interp [Statement] (Maybe Val) -> Interp [Statement] (Maybe Val)
initStatements f = (\s -> ((s,[]),s)) ^>> local f

binopInteger :: Interp (Val,Val) Val
binopInteger = proc _ -> joined returnA failStatic -<
  (NonNull,"Expected integer variables for op")

binopNum :: Interp (Val,Val) Val
binopNum = proc _ -> joined returnA (joined failStatic failStatic) -<
  (NonNull,("Expected integer variables for op",
            "Expected floating variables for op"))

cmpBool :: Interp (Val,Val) Abs.Bool
cmpBool = proc _ -> joined returnA failStatic -<
  (Abs.Top,"Expected numeric variables for comparison")
