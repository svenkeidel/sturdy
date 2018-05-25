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

import           Prelude hiding (lookup,read,mod,rem,div)

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
  show (StaticException s) = "Static: " ++ s
  show (DynamicException v) = "Dynamic: " ++ show v

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

type CanInterp env v c = (UseVal v c,
                          CanFail v c,
                          CanUseMem env v c,
                          CanUseConst c,
                          CanUseReader c,
                          CanUseState c,
                          CanCatch (Exception v) EInvoke v c,
                          CanCatch (Exception v) [Statement] v c,
                          CanCatch (Exception v) (Method, Maybe v, [Expr]) v c)

lookup' :: (Show name, CanFail val c, ArrowMaybeEnv name addr env c) => c name addr
lookup' = proc x -> do
  v <- lookup -< x
  case v of
    Just y -> returnA -< y
    Nothing -> failA -< StaticException $ printf "Variable %s not bounded" (show x)

read' :: (Show var, CanFail val c, ArrowMaybeStore var val c) => c var val
read' = proc x -> do
  v <- read -< x
  case v of
    Just y -> returnA -< y
    Nothing -> failA -< StaticException $ printf "Address %s not bounded" (show x)

assert :: (CanFail v c) => c (Bool, String) ()
assert = proc (prop, msg) -> if prop
  then returnA -< ()
  else failA -< StaticException msg

eval :: CanInterp e v c => c Expr v
eval = proc e -> case e of
  NewExpr t -> do
    assert -< (isBaseType t, "Expected a base type for new")
    newSimple -< t
  NewArrayExpr t i -> do
    assert -< (isNonvoidType t, "Expected a nonvoid type for newarray")
    v <- eval -< i
    newArray -< (t, [v])
  NewMultiArrayExpr t is -> do
    assert -< (isBaseType t, "Expected a nonvoid base type for newmultiarray")
    vs <- mapA eval -< is
    newArray -< (t, vs)
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
  BinopExpr i1 op i2 -> do
    v1 <- eval -< i1
    v2 <- eval -< i2
    case op of
      -- and :: c (v,v) v
      -- or :: c (v,v) v
      -- xor :: c (v,v) v
      Rem -> rem -< (v1,v2)
      Mod -> mod -< (v1,v2)
      Cmp -> cmp -< (v1,v2)
      Cmpg -> cmpg -< (v1,v2)
      Cmpl -> cmpl -< (v1,v2)
      Cmpeq -> eq -< (v1,v2)
      Cmpne -> neq -< (v1,v2)
      Cmpgt -> gt -< (v1,v2)
      Cmpge -> ge -< (v1,v2)
      Cmplt -> lt -< (v1,v2)
      Cmple -> le -< (v1,v2)
      -- shl :: c (v,v) v
      -- shr :: c (v,v) v
      -- ushr :: c (v,v) v
      Plus -> plus -< (v1,v2)
      Minus -> minus -< (v1,v2)
      Mult -> mult -< (v1,v2)
      Div -> div -< (v1,v2)
  UnopExpr op i -> do
    v <- eval -< i
    case op of
      Lengthof -> lengthOf -< v
      Neg -> neg -< v
  ThisRef -> eval -< (Local "@this")
  ParameterRef n -> eval -< (Local ("@parameter" ++ show n))
  CaughtExceptionRef -> eval -< (Local "@caughtexception")
  Local localName -> lookup' >>> read' -< localName
  DoubleConstant f -> doubleConstant -< f
  FloatConstant f -> floatConstant -< f
  IntConstant n -> intConstant -< n
  LongConstant f -> longConstant -< f
  NullConstant -> nullConstant -< ()
  StringConstant s -> stringConstant -< s
  ClassConstant c -> classConstant -< c
  MethodHandle _ -> failA -< StaticException "Evaluation of method handles is not implemented"
  _ -> failA -< StaticException "Not implemented"

class Arrow c => UseVal v c | c -> v where
  doubleConstant :: c Float v
  floatConstant :: c Float v
  intConstant :: c Int v
  longConstant :: c Int v
  nullConstant :: c () v
  stringConstant :: c String v
  classConstant :: c String v
  newSimple :: c Type v
  newArray :: c (Type, [v]) v
  -- and :: c (v,v) v
  -- or :: c (v,v) v
  -- xor :: c (v,v) v
  rem :: c (v,v) v
  mod :: c (v,v) v
  cmp :: c (v,v) v
  cmpg :: c (v,v) v
  cmpl :: c (v,v) v
  eq :: c (v,v) v
  neq :: c (v,v) v
  gt :: c (v,v) v
  ge :: c (v,v) v
  lt :: c (v,v) v
  le :: c (v,v) v
  -- shl :: c (v,v) v
  -- shr :: c (v,v) v
  -- ushr :: c (v,v) v
  plus :: c (v,v) v
  minus :: c (v,v) v
  mult :: c (v,v) v
  div :: c (v,v) v
  lengthOf :: c v v
  neg :: c v v
