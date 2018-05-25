{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Shared where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Control.Category

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.MaybeEnvironment
import           Control.Arrow.Fail
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.MaybeStore
import           Control.Arrow.TryCatch
import           Control.Arrow.Utils

import           Text.Printf
import           Syntax

data Exception v = StaticException String | DynamicException v deriving (Eq)

instance Show v => Show (Exception v) where
  show (StaticException s) = "StaticException " ++ s
  show (DynamicException v) = "DynamicException " ++ show v

exceptionToEither :: Exception v -> Either String v
exceptionToEither (StaticException s) = Left s
exceptionToEither (DynamicException v) = Right v

type Addr = Int
type PointerEnv e = e String Addr
type CompilationUnits = Map String CompilationUnit
type Fields = Map FieldSignature Addr
type MethodReader = Method

type CanFail v c = (ArrowChoice c, ArrowFail (Exception v) c)
type CanUseEnv env c = ArrowMaybeEnv String Addr (PointerEnv env) c
type CanUseStore v c = ArrowMaybeStore Addr v c
type CanUseReader c = ArrowReader MethodReader c
type CanUseState c = ArrowState Addr c
type CanUseConst c = ArrowConst (CompilationUnits, Fields) c
type CanCatch e x y c = ArrowTryCatch e x (Maybe y) c

type CanUseMem env v c = (CanUseEnv env c, CanUseStore v c)

type CanInterp env v c = (IsVal v c,
                          CanFail v c,
                          CanUseMem env v c,
                          CanUseConst c,
                          CanUseReader c,
                          CanUseState c,
                          CanCatch (Exception v) EInvoke v c,
                          CanCatch (Exception v) [Statement] v c,
                          CanCatch (Exception v) (Method, Maybe v, [Expr]) v c)

eval :: CanInterp e v c => c Expr v
eval = proc e -> case e of
  -- NewExpr t -> do
  --   assert -< (isBaseType t, "Expected a base type for new")
  --   case t of
  --     RefType c -> newSimple -< c
  --     _ -> returnA -< (defaultValue t)
  -- NewArrayExpr t i -> do
  --   assert -< (isNonvoidType t, "Expected a nonvoid type for newarray")
  --   v <- eval -< i
  --   n <- toInt -< v
  --   assert -< (n > 0, "Expected a positive integer for newarray size")
  --   newArray -< (t, [n])
  -- NewMultiArrayExpr t is -> do
  --   assert -< (isBaseType t, "Expected a nonvoid base type for newmultiarray")
  --   vs <- mapA eval -< is
  --   ns <- mapA toInt -< vs
  --   assert -< (all (>0) ns, "Expected positive integers for newmultiarray sizes")
  --   newArray -< (t, ns)
  -- CastExpr t i -> do
  --   v <- eval -< i
  --   b <- isInstanceof -< (v, t)
  --   if b
  --   then do
  --     v' <- unbox -< v
  --     case v' of
  --       ObjectVal _ _ -> returnA -< v
  --       _ -> failA -< StringVal "Casting of primivites and arrays is not yet supported"
  --       -- https://docs.oracle.com/javase/specs/jls/se7/html/jls-5.html#jls-5.1.2
  --   else throw -< ("java.lang.ClassCastException", printf "Cannot cast %s to type %s" (show v) (show t))
  -- InstanceOfExpr i t -> do
  --   v <- eval -< i
  --   b <- isInstanceof -< (v, t)
  --   returnA -< BoolVal b
  -- InvokeExpr invokeExpr -> do
  --   v <- tryCatchA evalInvoke (pi2 >>> failA) -< invokeExpr
  --   case v of
  --     Just v' -> returnA -< v'
  --     Nothing -> failA -< StringVal "Method returned nothing"
  -- ArrayRef localName i -> do
  --   n <- evalIndex -< i
  --   (_, ArrayVal xs) <- fetchArrayWithAddr -< localName
  --   if n >= 0 && n < length xs
  --   then returnA -< xs !! n
  --   else throw -< ("java.lang.ArrayIndexOutOfBoundsException", printf "Index %d out of bounds" (show n))
  -- FieldRef localName fieldSignature -> do
  --   (_, ObjectVal _ m) <- fetchObjectWithAddr -< localName
  --   case Map.lookup fieldSignature m of
  --     Just x -> returnA -< x
  --     Nothing -> failA -< StringVal $ printf "Field %s not defined for object %s" (show fieldSignature) (show localName)
  -- SignatureRef fieldSignature -> do
  --   (_, val) <- fetchFieldWithAddr -< fieldSignature
  --   returnA -< val
  -- BinopExpr i1 op i2 -> do
  --   v1 <- eval -< i1
  --   v2 <- eval -< i2
  --   case op of
  --     Cmpeq -> returnA -< BoolVal (v1 == v2)
  --     Cmpne -> returnA -< BoolVal (v1 /= v2)
  --     _ -> do
  --       let toFloatVal :: Float -> Val
  --           toFloatVal x = case (v1, v2) of
  --             (DoubleVal _, _) -> DoubleVal x
  --             (_, DoubleVal _) -> DoubleVal x
  --             (_, _) -> FloatVal x
  --       let toIntVal :: Int -> Val
  --           toIntVal x = case (v1, v2) of
  --             (LongVal _, _) -> LongVal x
  --             (_, LongVal _) -> LongVal x
  --             (_, _) -> IntVal x
  --       case (isNumVal v1, isNumVal v2) of
  --         (Nothing, _) -> failA -< StringVal $ printf "Expected two numbers as argument for %s" (show op)
  --         (_, Nothing) -> failA -< StringVal $ printf "Expected two numbers as argument for %s" (show op)
  --         (Just (Right x1), Just (Right x2)) -> evalBinopFractional -< (toFloatVal, op, x1, x2)
  --         (Just (Right x1), Just (Left x2))  -> evalBinopFractional -< (toFloatVal, op, x1, fromIntegral x2)
  --         (Just (Left x1),  Just (Right x2)) -> evalBinopFractional -< (toFloatVal, op, fromIntegral x1, x2)
  --         (Just (Left x1),  Just (Left x2))  -> evalBinopIntegral   -< (toIntVal,   op, x1, x2)
  -- UnopExpr op i -> do
  --   v <- eval -< i
  --   case op of
  --     Lengthof -> case v of
  --       RefVal addr -> do
  --         v' <- read' -< addr
  --         case v' of
  --           ArrayVal xs -> returnA -< (IntVal (length xs))
  --           _ -> failA -< StringVal "Expected an array as argument for lengthof"
  --       _ -> failA -< StringVal "Expected an array as argument for lengthof"
  --     Neg -> case v of
  --       IntVal n -> returnA -< (IntVal (-n))
  --       LongVal l -> returnA -< (LongVal (-l))
  --       FloatVal f -> returnA -< (FloatVal (-f))
  --       DoubleVal d -> returnA -< (DoubleVal (-d))
  --       _ -> failA -< StringVal "Expected a number as argument for -"
  -- ThisRef -> eval -< (Local "@this")
  -- ParameterRef n -> eval -< (Local ("@parameter" ++ show n))
  -- CaughtExceptionRef -> eval -< (Local "@caughtexception")
  -- Local localName -> fetchLocal -< localName
  DoubleConstant f -> doubleConstant -< f
  FloatConstant f -> floatConstant -< f
  IntConstant n -> intConstant -< n
  LongConstant f -> longConstant -< f
  NullConstant -> nullConstant -< ()
  StringConstant s -> stringConstant -< s
  ClassConstant c -> classConstant -< c
  -- MethodHandle _ -> failA -< StringVal "Evaluation of method handles is not implemented"
  _ -> failA -< StaticException "Not implemented"

class Arrow c => IsVal v c | c -> v where
  doubleConstant :: c Float v
  floatConstant :: c Float v
  intConstant :: c Int v
  longConstant :: c Int v
  nullConstant :: c () v
  stringConstant :: c String v
  classConstant :: c String v
