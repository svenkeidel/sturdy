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
module NullnessSemantics where

import           Prelude hiding (id,lookup,read,fail,Bounded(..))

import qualified Data.Boolean as B
import           Data.Exception
import           Data.Order

import qualified Data.Abstract.Boolean as Abs
import           Data.Abstract.Environment (Env)
import qualified Data.Abstract.Environment as E
import qualified Data.Abstract.Equality as Abs
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

instance (LowerBounded e, LowerBounded a) => LowerBounded (Error e a) where
  bottom = SuccessOrFail bottom bottom

deriving instance PreOrd y => PreOrd (Interp x y)
deriving instance (Complete y) => Complete (Interp x y)
deriving instance LowerBounded y => LowerBounded (Interp x y)

runInterp :: Interp x y ->
             [CompilationUnit] -> Mem -> x ->
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

runProgram' :: [CompilationUnit] -> (Method,[Immediate]) -> Out (Maybe Val)
runProgram' units = runInterp runProgram units []

runStatements' :: Mem -> [Statement] -> Out (Maybe Val)
runStatements' = runInterp (initStatements runStatements) []

eval' :: Mem -> Expr -> Out Val
eval' = runInterp eval []

evalBool' :: Mem -> BoolExpr -> Out Abs.Bool
evalBool' = runInterp evalBool []

evalImmediate' :: Mem -> Immediate -> Out Val
evalImmediate' = runInterp evalImmediate []

---- Instances -----------------------------------------------------------------

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
  declare f = (\((l,v),x) -> (l,v,x)) ^>> extendEnv' f
  readVar = lookup_
  updateVar f = proc ((l,v),x) -> do
    lookup_ -< l
    extendEnv' f -< (l,v,x)
  readIndex = arr fst
  updateIndex f = U.pi2 >>> f
  readField = arr fst
  updateField f = U.pi2 >>> f
  readStaticField = proc f -> read U.pi1 fail -<
    (f, StaticException $ printf "FieldReference %s not bound" (show f))
  updateStaticField = proc _ -> fail -< StaticException "Not implemented yet"
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
lookup_ = proc x -> lookup U.pi1 fail -<
  (x, StaticException $ printf "Variable %s not bound" (show x))

initStatements :: Interp [Statement] (Maybe Val) -> Interp [Statement] (Maybe Val)
initStatements f = (\s -> ((s,[]),s)) ^>> local f
