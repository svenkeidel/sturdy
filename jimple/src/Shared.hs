{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Shared where

import           Prelude hiding (rem,div,id,or,and,fail)

import           Data.List (find,elemIndex)
import           Data.Exception

import           Control.Category

import           Control.Arrow
import           Control.Arrow.Environment
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import qualified Control.Arrow.Utils as U

import           Text.Printf
import           Syntax

type CanFail val c = (ArrowChoice c,ArrowFail (Exception val) c)

type CanInterp env var' val' const val bool c = (
  UseVal val c,
  UseBool bool val c,
  UseConst c,
  UseEnv (env var' val') c,
  CanFail val c,
  ArrowReader Method c,
  ArrowEnv var' val' (env var' val') c,
  ArrowFix [Statement] (Maybe val) c,
  ArrowExcept EInvoke (Maybe val) (Exception val) c,
  ArrowExcept ([Statement],[CatchClause]) (Maybe val) (Exception val) c)

assert :: (CanFail v c) => c (Bool,String) ()
assert = proc (prop,msg) -> if prop
  then returnA -< ()
  else fail -< StaticException msg

justOrFail :: (CanFail v c) => c (Maybe x,String) x
justOrFail = proc (x,e) -> case x of
  Just v -> returnA -< v
  Nothing -> fail -< StaticException e

liftAMaybe :: ArrowChoice c => c x z -> c (Maybe x) (Maybe z)
liftAMaybe f = proc m -> case m of
  Just x -> f >>^ Just -< x
  Nothing -> returnA -< Nothing

getFieldSignatures :: ([Modifier] -> Bool) -> CompilationUnit -> [FieldSignature]
getFieldSignatures p unit =
  [fieldSignature unit m | FieldMember m <- fileBody unit, p (fieldModifiers m)]

readCompilationUnit :: (CanFail v c,UseConst c) => c String CompilationUnit
readCompilationUnit = proc n -> do
  compilationUnits <- askCompilationUnits -< ()
  justOrFail -< (find (\u -> fileName u == n) compilationUnits,printf "CompilationUnit %s not loaded" (show n))

evalInvoke :: CanInterp env var' val' const val bool c => c EInvoke (Maybe val)
evalInvoke = proc e -> case e of
  SpecialInvoke localName m args -> ev -< (Just localName,m,args)
  VirtualInvoke localName m args -> ev -< (Just localName,m,args)
  InterfaceInvoke localName m args -> ev -< (Just localName,m,args)
  StaticInvoke m args -> ev -< (Nothing,m,args)
  DynamicInvoke{} -> fail -< StaticException "DynamicInvoke is not implemented"
  where
    ev = proc (localName,m,args) -> do
      method <- readMethod -< m
      this <- liftAMaybe readVar -< localName
      case this of
        Just _ -> assert -< (Static `notElem` methodModifiers method,"Expected a non-static method for non-static invoke")
        Nothing -> assert -< (Static `elem` methodModifiers method,"Expected a static method for static invoke")
      runMethod -< (method,this,args)
    readMethod = proc (MethodSignature c retType n argTypes) -> do
      unit <- readCompilationUnit -< c
      case find (matchesSignature retType n argTypes) (fileBody unit) of
        Just (MethodMember m) -> returnA -< m
        _ -> fail -< StaticException $ printf "Method %s not defined for class %s" (show n) (show c)
    matchesSignature :: Type -> String -> [Type] -> Member -> Bool
    matchesSignature retType n argTypes (MethodMember m) =
      methodName m == n && returnType m == retType && parameters m == argTypes
    matchesSignature _ _ _ _ = False

evalImmediate :: (ArrowChoice c,UseVal val c) => c Immediate val
evalImmediate = proc i -> case i of
  Local localName -> readVar -< localName
  DoubleConstant f -> doubleConstant -< f
  FloatConstant f -> floatConstant -< f
  IntConstant n -> intConstant -< n
  LongConstant f -> longConstant -< f
  NullConstant -> nullConstant -< ()
  StringConstant s -> stringConstant -< s
  ClassConstant c -> classConstant -< c

evalAtIdentifier :: (ArrowChoice c,UseVal val c) => c AtIdentifier val
evalAtIdentifier = proc i -> case i of
  ThisRef -> evalImmediate -< Local "@this"
  ParameterRef n -> evalImmediate -< Local ("@parameter" ++ show n)
  CaughtExceptionRef -> evalImmediate -< Local "@caughtexception"

evalBool :: (ArrowChoice c,UseBool bool val c,UseVal val c) => c BoolExpr bool
evalBool = proc (BoolExpr i1 op i2) -> do
  v1 <- evalImmediate -< i1
  v2 <- evalImmediate -< i2
  case op of
    Cmpeq -> eq -< (v1,v2)
    Cmpne -> neq -< (v1,v2)
    Cmpgt -> gt -< (v1,v2)
    Cmpge -> ge -< (v1,v2)
    Cmplt -> lt -< (v1,v2)
    Cmple -> le -< (v1,v2)

eval :: CanInterp env var' val' const val bool c => c Expr val
eval = proc e -> case e of
  NewExpr t -> do
    assert -< (isBaseType t,"Expected a base type for new")
    newSimple -< t
  NewArrayExpr t i -> do
    assert -< (isNonvoidType t,"Expected a nonvoid type for newarray")
    v <- evalImmediate -< i
    newArray -< (t,[v])
  NewMultiArrayExpr t is -> do
    assert -< (isBaseType t,"Expected a nonvoid base type for newmultiarray")
    vs <- U.map evalImmediate -< is
    newArray -< (t,vs)
  InstanceOfExpr i t -> first evalImmediate >>> instanceOf -< (i,t)
  CastExpr t i -> first evalImmediate >>> (id &&& instanceOf) >>> cast -< (i,t)
  InvokeExpr invokeExpr -> do
    v <- tryCatch evalInvoke (U.pi2 >>> fail) -< invokeExpr
    justOrFail -< (v,"Method returned nothing")
  RefExpr refExpr -> case refExpr of
    ArrayRef l i -> (readVar *** evalImmediate) >>> readIndex -< (l,i)
    FieldRef l f -> first readVar >>> readField -< (l,f)
    SignatureRef f -> readStaticField -< f
  BinopExpr i1 op i2 -> do
    v1 <- evalImmediate -< i1
    v2 <- evalImmediate -< i2
    case op of
      And -> and -< (v1,v2)
      Or -> or -< (v1,v2)
      Xor -> xor -< (v1,v2)
      Rem -> rem -< (v1,v2)
      Cmp -> cmp -< (v1,v2)
      Cmpg -> cmpg -< (v1,v2)
      Cmpl -> cmpl -< (v1,v2)
      Shl -> shl -< (v1,v2)
      Shr -> shr -< (v1,v2)
      Ushr -> ushr -< (v1,v2)
      Plus -> plus -< (v1,v2)
      Minus -> minus -< (v1,v2)
      Mult -> mult -< (v1,v2)
      Div -> div -< (v1,v2)
  UnopExpr op i -> do
    v <- evalImmediate -< i
    case op of
      Lengthof -> lengthOf -< v
      Neg -> neg -< v
  ImmediateExpr i -> evalImmediate -< i
  MethodHandle _ -> fail -< StaticException "Evaluation of method handles is not implemented"

runStatements :: CanInterp env var' val' const val bool c => c [Statement] (Maybe val)
runStatements = fix $ \run -> proc stmts -> case stmts of
  [] -> returnA -< Nothing
  (stmt:rest) -> case stmt of
    Label labelName -> do
      body <- currentMethodBody -< ()
      let clauses = filter (\clause -> fromLabel clause == labelName && Label (toLabel clause) `elem` stmts) (catchClauses body)
      tryCatch (U.pi1 >>> run) (catchException run) -< (rest,clauses)
    Tableswitch i cases -> runSwitch run -< (i,cases)
    Lookupswitch i cases -> runSwitch run -< (i,cases)
    If e label -> first (evalBool &&& id) >>> if_ (atLabel run) run -< (e,(label,rest))
    Goto label -> (atLabel run) -< label
    Ret i -> liftAMaybe evalImmediate -< i
    Return i -> liftAMaybe evalImmediate -< i
    Throw i -> evalImmediate >>> DynamicException ^>> fail -< i
    Identity l i _ -> first (second evalAtIdentifier) >>> updateVar run -< ((l,i),rest)
    IdentityNoType l i -> first (second evalAtIdentifier) >>> updateVar run -< ((l,i),rest)
    Assign var e -> do
      v <- eval -< e
      case var of
        LocalVar l -> updateVar run -< ((l,v),rest)
        ReferenceVar ref -> case ref of
          ArrayRef l i -> first (first (readVar *** evalImmediate)) >>> updateIndex run -< (((l,i),v),rest)
          FieldRef l f -> first (first readVar) >>> updateField run -< ((l,(f,v)),rest)
          SignatureRef f -> do
            updateStaticField -< (f,v)
            run -< rest
    Invoke e -> do
      evalInvoke -< e
      run -< rest
    Nop -> run -< rest
    Breakpoint -> run -< rest
  where
    currentMethodBody = ask >>> proc m -> case methodBody m of
      EmptyBody -> fail -< StaticException $ printf "Empty body for method %s" (show m)
      FullBody{} -> returnA -< methodBody m
    atLabel f = statementsFromLabel >>> f
    statementsFromLabel = proc label -> do
      b <- currentMethodBody -< ()
      case Label label `elemIndex` statements b of
        Just i -> returnA -< drop i (statements b)
        Nothing -> fail -< StaticException $ printf "Undefined label: %s" label
    catchException f = proc ((_,clauses),exception) -> case exception of
        StaticException _ -> fail -< exception
        DynamicException val -> catch (handleException f) -< (val,clauses)
    handleException f = proc (val,clause) ->
      declare (atLabel f) -< (("@caughtexception",val),withLabel clause)
    runSwitch f = first evalImmediate >>> case_ (atLabel f)

runMethod :: CanInterp env var' val' const val bool c => c (Method,Maybe val,[Immediate]) (Maybe val)
runMethod = proc (method,this,args) -> case methodBody method of
  EmptyBody -> returnA -< Nothing
  FullBody{declarations=decs,statements=stmts} -> do
    argVals <- U.map evalImmediate -< args
    let thisBinding = maybe [] (\x -> [("@this",x)]) this
    let paramBindings = zip (map (\i -> "@parameter" ++ show i) [(0 :: Int)..]) argVals
    decBindings <- U.map (second defaultValue) -< concatMap (\(t,d) -> zip d (repeat t)) decs
    env <- emptyEnv -< ()
    localEnv runWithBindings -< (env,(thisBinding ++ paramBindings ++ decBindings,(method,stmts)))
  where
    runWithBindings = proc (bs,x) -> case bs of
      [] -> local runStatements -< x
      (binding:rest) -> declare runWithBindings -< (binding,(rest,x))

runProgram :: CanInterp env var' val' const val bool c => c [Immediate] (Maybe val)
runProgram = proc args -> do
  units <- askCompilationUnits -< ()
  U.map (second defaultValue >>> updateStaticField) -< concatMap staticFieldsWithType units
  U.map runMethod -< concatMap clinitMethodWithArgs units
  mainMethod <- ask -< ()
  runMethod -< (mainMethod,Nothing,args)
  where
    staticFieldsWithType u =
      [(fieldSignature u m,fieldType m) | FieldMember m <- fileBody u, Static `elem` fieldModifiers m]
    clinitMethodWithArgs u =
      [(m,Nothing,[]) | MethodMember m <- fileBody u, methodName m == "<clinit>"]

class Arrow c => UseVal v c | c -> v where
  doubleConstant :: c Float v
  floatConstant :: c Float v
  intConstant :: c Int v
  longConstant :: c Int v
  nullConstant :: c () v
  stringConstant :: c String v
  classConstant :: c String v
  newSimple :: c Type v
  newArray :: c (Type,[v]) v
  and :: c (v,v) v
  or :: c (v,v) v
  xor :: c (v,v) v
  rem :: c (v,v) v
  cmp :: c (v,v) v
  cmpg :: c (v,v) v
  cmpl :: c (v,v) v
  shl :: c (v,v) v
  shr :: c (v,v) v
  ushr :: c (v,v) v
  plus :: c (v,v) v
  minus :: c (v,v) v
  mult :: c (v,v) v
  div :: c (v,v) v
  lengthOf :: c v v
  neg :: c v v
  instanceOf :: c (v,Type) v
  cast :: c ((v,Type),v) v
  defaultValue :: c Type v
  declare :: c x (Maybe v) -> c ((String,v),x) (Maybe v)
  readVar :: c String v
  updateVar :: c [Statement] (Maybe v) -> c ((String,v),[Statement]) (Maybe v)
  readIndex :: c (v,v) v
  updateIndex :: c [Statement] (Maybe v) -> c (((v,v),v),[Statement]) (Maybe v)
  readField :: c (v,FieldSignature) v
  updateField :: c [Statement] (Maybe v) -> c ((v,(FieldSignature,v)),[Statement]) (Maybe v)
  readStaticField :: c FieldSignature v
  updateStaticField :: c (FieldSignature,v) ()
  case_ :: c String (Maybe v) -> c (v,[CaseStatement]) (Maybe v)
  catch :: c (v,CatchClause) (Maybe v) -> c (v,[CatchClause]) (Maybe v)

class Arrow c => UseBool b v c | c -> b v where
  eq :: c (v,v) b
  neq :: c (v,v) b
  gt :: c (v,v) b
  ge :: c (v,v) b
  lt :: c (v,v) b
  le :: c (v,v) b
  if_ :: c String (Maybe v) -> c [Statement] (Maybe v) -> c ((b,BoolExpr),(String,[Statement])) (Maybe v)

class Arrow c => UseEnv env c | c -> env where
  emptyEnv :: c () env

class Arrow c => UseConst c where
  askCompilationUnits :: c () [CompilationUnit]
