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

import           Prelude hiding (lookup,read,rem,div,id,or,and,fail)

import           Data.List (find,elemIndex)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Order
import           Data.String

import           Control.Category

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Environment
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Store
import qualified Control.Arrow.Utils as U

import           Text.Printf
import           Syntax

data Exception v = StaticException String | DynamicException v deriving (Eq)

instance Show v => Show (Exception v) where
  show (StaticException s) = "Static: " ++ s
  show (DynamicException v) = "Dynamic: " ++ show v

instance PreOrd v => PreOrd (Exception v) where
  StaticException s1 ⊑ StaticException s2 = s1 == s2
  _ ⊑ StaticException _ = True
  DynamicException v1 ⊑ DynamicException v2 = v1 ⊑ v2
  _ ⊑ _ = False

instance LowerBounded v => LowerBounded (Exception v) where
  bottom = StaticException "Bottom exception"

instance Complete v => Complete (Exception v) where
  StaticException s1 ⊔ StaticException s2 = StaticException $ s1 ++ "\n" ++ s2
  StaticException s ⊔ _ = StaticException s
  _ ⊔ StaticException s = StaticException s
  DynamicException v1 ⊔ DynamicException v2 = DynamicException $ v1 ⊔ v2

instance IsString (Exception v) where
  fromString = StaticException

type CompilationUnits = Map String CompilationUnit
type Fields = Map FieldSignature Int
type MethodReader = Method

type CanFail val c = (ArrowChoice c,ArrowFail (Exception val) c)
type CanUseEnv env addr c = ArrowEnv String addr (env String addr) c
type CanUseReader c = ArrowReader MethodReader c
type CanUseStore addr val c = (ArrowStore addr val c,ArrowRead addr val (Exception val) val c)
type CanUseState addr c = (Enum addr,ArrowState addr c)
type CanUseConst const c = (ArrowConst const c,UseConst c)
type CanUseMem env addr val c = (CanUseEnv env addr c,CanUseStore addr val c,UseMem env addr c)
type CanCatch x val c = ArrowExcept x (Maybe val) (Exception val) c

type CanInterp env addr const val bool c = (
  Show val,
  Eq val,
  Show addr,
  Eq addr,
  UseVal val c,
  UseBool bool val c,
  CanFail val c,
  CanUseMem env addr val c,
  CanUseConst const c,
  CanUseReader c,
  CanUseState addr c,
  ArrowExcept (val,val) val (Exception val) c,
  CanCatch EInvoke val c,
  CanCatch ([Statement],[CatchClause]) val c,
  CanCatch (Method,Maybe val,[Immediate]) val c)

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

getFieldSignatures :: CompilationUnit -> ([Modifier] -> Bool) -> [FieldSignature]
getFieldSignatures unit p =
  concatMap toFieldSignature (fileBody unit)
  where
    toFieldSignature :: Member -> [FieldSignature]
    toFieldSignature (FieldMember f) =
      [FieldSignature (fileName unit) (fieldType f) (fieldName f) | p (fieldModifiers f)]
    toFieldSignature _ = []

readCompilationUnit :: (CanFail v c,CanUseConst const c) => c String CompilationUnit
readCompilationUnit = proc n -> do
  compilationUnits <- askCompilationUnits -< ()
  justOrFail -< (Map.lookup n compilationUnits,printf "CompilationUnit %s not loaded" (show n))

matchesSignature :: Type -> String -> [Type] -> Member -> Bool
matchesSignature retType n argTypes (MethodMember m) =
  methodName m == n && returnType m == retType && parameters m == argTypes
matchesSignature _ _ _ _ = False

readMethod :: (CanFail v c,CanUseConst const c) => c MethodSignature Method
readMethod = proc (MethodSignature c retType n argTypes) -> do
  unit <- readCompilationUnit -< c
  case find (matchesSignature retType n argTypes) (fileBody unit) of
    Just (MethodMember m) -> returnA -< m
    _ -> fail -< StaticException $ printf "Method %s not defined for class %s" (show n) (show c)

evalInvoke :: CanInterp env addr const val bool c => c EInvoke (Maybe val)
evalInvoke = proc e -> case e of
  SpecialInvoke localName methodSignature args -> ev -< (Just localName,methodSignature,args)
  VirtualInvoke localName methodSignature args -> ev -< (Just localName,methodSignature,args)
  InterfaceInvoke localName methodSignature args -> ev -< (Just localName,methodSignature,args)
  StaticInvoke methodSignature args -> ev -< (Nothing,methodSignature,args)
  DynamicInvoke{} -> fail -< StaticException "DynamicInvoke is not implemented"
  where
    ev = proc (localName,methodSignature,args) -> do
      method <- readMethod -< methodSignature
      this <- liftAMaybe readVar -< localName
      case this of
        Just _ -> assert -< (Static `notElem` methodModifiers method,"Expected a non-static method for non-static invoke")
        Nothing -> assert -< (Static `elem` methodModifiers method,"Expected a static method for static invoke")
      runMethod -< (method,this,args)

evalImmediate :: CanInterp env addr const val bool c => c Immediate val
evalImmediate = proc i -> case i of
  Local localName -> readVar -< localName
  DoubleConstant f -> doubleConstant -< f
  FloatConstant f -> floatConstant -< f
  IntConstant n -> intConstant -< n
  LongConstant f -> longConstant -< f
  NullConstant -> nullConstant -< ()
  StringConstant s -> stringConstant -< s
  ClassConstant c -> classConstant -< c

evalAtIdentifier :: CanInterp env addr const val bool c => c AtIdentifier val
evalAtIdentifier = proc i -> case i of
  ThisRef -> evalImmediate -< Local "@this"
  ParameterRef n -> evalImmediate -< Local ("@parameter" ++ show n)
  CaughtExceptionRef -> evalImmediate -< Local "@caughtexception"

evalBool :: CanInterp env addr const val bool c => c BoolExpr bool
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

eval :: CanInterp env addr const val bool c => c Expr val
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

runStatements :: CanInterp env addr const val bool c => c [Statement] (Maybe val)
runStatements = proc stmts -> case stmts of
  [] -> returnA -< Nothing
  (stmt:rest) -> case stmt of
    Label labelName -> do
      body <- currentMethodBody -< ()
      let clauses = filter (\clause -> fromLabel clause == labelName && Label (toLabel clause) `elem` stmts) (catchClauses body)
      tryCatch (U.pi1 >>> runStatements) catchException -< (rest,clauses)
    Tableswitch i cases -> runSwitch -< (i,cases)
    Lookupswitch i cases -> runSwitch -< (i,cases)
    If e label -> first (evalBool &&& id) >>> if_ runStatementsFromLabel runStatements -< (e,(label,rest))
    Goto label -> runStatementsFromLabel -< label
    Ret i -> liftAMaybe evalImmediate -< i
    Return i -> liftAMaybe evalImmediate -< i
    Throw i -> evalImmediate >>> DynamicException ^>> fail -< i
    Identity l i _ -> first (second evalAtIdentifier) >>> updateVar runStatements -< ((l,i),rest)
    IdentityNoType l i -> first (second evalAtIdentifier) >>> updateVar runStatements -< ((l,i),rest)
    Assign var e -> do
      v <- eval -< e
      case var of
        LocalVar l -> updateVar runStatements -< ((l,v),rest)
        ReferenceVar ref -> case ref of
          ArrayRef l i -> first (first (readVar *** evalImmediate)) >>> updateIndex runStatements -< (((l,i),v),rest)
          FieldRef l f -> first (first readVar) >>> updateField runStatements -< ((l,(f,v)),rest)
          SignatureRef f -> updateStaticField runStatements -< ((f,v),rest)
    Invoke e -> do
      evalInvoke -< e
      runStatements -< rest
    Nop -> runStatements -< rest
    Breakpoint -> runStatements -< rest
  where
    runStatementsFromLabel = statementsFromLabel >>> runStatements
    currentMethodBody = ask >>> proc m -> case methodBody m of
      EmptyBody -> fail -< StaticException $ printf "Empty body for method %s" (show m)
      FullBody{} -> returnA -< methodBody m
    statementsFromLabel = proc label -> do
      b <- currentMethodBody -< ()
      case Label label `elemIndex` statements b of
        Just i -> returnA -< drop i (statements b)
        Nothing -> fail -< StaticException $ printf "Undefined label: %s" label
    catchException = proc ((_,clauses),exception) -> case exception of
        StaticException _ -> fail -< exception
        DynamicException val -> catch handleException -< (val,clauses)
    handleException = proc (val,clause) ->
      declare runStatementsFromLabel -< (("@caughtexception",val),withLabel clause)
    runSwitch = first evalImmediate >>> case_ runStatementsFromLabel

runMethod :: CanInterp env addr const val bool c => c (Method,Maybe val,[Immediate]) (Maybe val)
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

runProgram :: CanInterp env addr const val bool c => c [Immediate] (Maybe val)
runProgram = proc args -> do
  units <- askCompilationUnits -< ()
  fields <- askFields >>> Map.toList ^>> U.map (second addrFromInt) -< ()
  U.map (second defaultValue >>> write) -< map (\(FieldSignature _ t _,a) -> (a,t)) fields
  U.map ((\u -> find (\m -> methodName m == "<clinit>") [m | MethodMember m <- fileBody u])
    ^>> liftAMaybe ((\m -> (m,Nothing,[])) ^>> runMethod)) -< Map.elems units
  mainMethod <- ask -< ()
  runMethod -< (mainMethod,Nothing,args)

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
  updateStaticField :: c [Statement] (Maybe v) -> c ((FieldSignature,v),[Statement]) (Maybe v)
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

class Arrow c => UseMem env addr c | c -> env addr where
  emptyEnv :: c () (env String addr)
  addrFromInt :: c Int addr

class Arrow c => UseConst c where
  askCompilationUnits :: c () CompilationUnits
  askFields :: c () Fields
