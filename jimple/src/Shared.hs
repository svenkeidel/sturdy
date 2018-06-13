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

type CanInterp env addr const val c = (
  Show val,
  LowerBounded val,
  Eq val,
  Show addr,
  Eq addr,
  UseVal val c,
  UseFlow val c,
  CanFail val c,
  CanUseMem env addr val c,
  CanUseConst const c,
  CanUseReader c,
  CanUseState addr c,
  ArrowExcept (val,val) val (Exception val) c,
  CanCatch EInvoke val c,
  CanCatch ([Statement],[CatchClause]) val c,
  CanCatch (Method,Maybe val,[Expr]) val c)

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

getInitializedFields :: (UseVal v c,CanFail v c,CanUseConst const c) => c String [(FieldSignature,v)]
getInitializedFields = readCompilationUnit >>> proc unit -> do
  let fieldSignatures = getFieldSignatures unit (\m -> Static `notElem` m)
  ownFields <- U.map (second defaultValue) -< map (\s@(FieldSignature _ t' _) -> (s,t')) fieldSignatures
  case extends unit of
    Just p -> do
      parentFields <- getInitializedFields -< p
      returnA -< parentFields ++ ownFields
    Nothing -> returnA -< ownFields

lookupField :: (UseVal v c,CanFail v c,CanUseConst const c,UseMem env addr c) => c FieldSignature addr
lookupField = proc x -> do
  fields <- askFields -< ()
  justOrFail >>> addrFromInt -< (Map.lookup x fields,printf "Field %s not bound" (show x))

lookup_ :: (Eq addr,CanFail v c,CanUseEnv env addr c,UseMem env addr c) => c String addr
lookup_ = proc x -> lookup U.pi1 fail -< (x, StaticException $ printf "Variable %s not bound" (show x))

read_ :: (Show addr,Eq v,LowerBounded v,CanFail v c,CanUseStore addr v c) => c addr v
read_ = proc addr -> read U.pi1 fail -< (addr, StaticException $ printf "Address %s not bound" (show addr))

readLocal :: (Eq addr,Show addr,Eq val,Show val,LowerBounded val,CanFail val c,CanUseMem env addr val c) => c String val
readLocal = lookup_ >>> read_

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

alloc :: (UseVal val c,CanUseState addr c,CanUseStore addr val c) => c val addr
alloc = proc val -> do
  addr <- get -< ()
  write -< (addr,val)
  put -< succ addr
  returnA -< addr

evalInvoke :: CanInterp env addr const val c => c EInvoke (Maybe val)
evalInvoke =
  let ev = proc (localName,methodSignature,args) -> do
        method <- readMethod -< methodSignature
        case localName of
          Just n -> do
            assert -< (Static `notElem` methodModifiers method,"Expected a non-static method for non-static invoke")
            this <- readLocal -< n
            runMethod -< (method,Just this,args)
          Nothing -> do
            assert -< (Static `elem` methodModifiers method,"Expected a static method for static invoke")
            runMethod -< (method,Nothing,args)
  in proc e -> case e of
    SpecialInvoke localName methodSignature args -> ev -< (Just localName,methodSignature,args)
    VirtualInvoke localName methodSignature args -> ev -< (Just localName,methodSignature,args)
    InterfaceInvoke localName methodSignature args -> ev -< (Just localName,methodSignature,args)
    StaticInvoke methodSignature args -> ev -< (Nothing,methodSignature,args)
    DynamicInvoke{} -> fail -< StaticException "DynamicInvoke is not implemented"

eval :: CanInterp env addr const val c => c Expr val
eval = proc e -> case e of
  NewExpr t -> do
    assert -< (isBaseType t,"Expected a base type for new")
    newSimple -< t
  NewArrayExpr t i -> do
    assert -< (isNonvoidType t,"Expected a nonvoid type for newarray")
    v <- eval -< i
    newArray -< (t,[v])
  NewMultiArrayExpr t is -> do
    assert -< (isBaseType t,"Expected a nonvoid base type for newmultiarray")
    vs <- U.map eval -< is
    newArray -< (t,vs)
  CastExpr t i -> first eval >>> (id &&& instanceOf) >>> cast -< (i,t)
  InstanceOfExpr i t -> first eval >>> instanceOf -< (i,t)
  InvokeExpr invokeExpr -> do
    v <- tryCatch evalInvoke (U.pi2 >>> fail) -< invokeExpr
    justOrFail -< (v,"Method returned nothing")
  ArrayRef l i -> ((readLocal >>> deref) *** eval) >>> readIndex -< (l,i)
  FieldRef l f -> first (readLocal >>> deref) >>> readField -< (l,f)
  SignatureRef fieldSignature -> lookupField >>> read_ -< fieldSignature
  BinopExpr i1 op i2 -> do
    v1 <- eval -< i1
    v2 <- eval -< i2
    case op of
      And -> and -< (v1,v2)
      Or -> or -< (v1,v2)
      Xor -> xor -< (v1,v2)
      Rem -> rem -< (v1,v2)
      Cmp -> cmp -< (v1,v2)
      Cmpg -> cmpg -< (v1,v2)
      Cmpl -> cmpl -< (v1,v2)
      Cmpeq -> eq -< (v1,v2)
      Cmpne -> neq -< (v1,v2)
      Cmpgt -> gt -< (v1,v2)
      Cmpge -> ge -< (v1,v2)
      Cmplt -> lt -< (v1,v2)
      Cmple -> le -< (v1,v2)
      Shl -> shl -< (v1,v2)
      Shr -> shr -< (v1,v2)
      Ushr -> ushr -< (v1,v2)
      Plus -> plus -< (v1,v2)
      Minus -> minus -< (v1,v2)
      Mult -> mult -< (v1,v2)
      Div -> div -< (v1,v2)
  UnopExpr op i -> do
    v <- eval -< i
    case op of
      Lengthof -> deref >>> lengthOf -< v
      Neg -> neg -< v
  ThisRef -> eval -< (Local "@this")
  ParameterRef n -> eval -< (Local ("@parameter" ++ show n))
  CaughtExceptionRef -> eval -< (Local "@caughtexception")
  Local localName -> readLocal -< localName
  DoubleConstant f -> doubleConstant -< f
  FloatConstant f -> floatConstant -< f
  IntConstant n -> intConstant -< n
  LongConstant f -> longConstant -< f
  NullConstant -> nullConstant -< ()
  StringConstant s -> stringConstant -< s
  ClassConstant c -> classConstant -< c
  MethodHandle _ -> fail -< StaticException "Evaluation of method handles is not implemented"

runStatements :: CanInterp env addr const val c => c [Statement] (Maybe val)
runStatements = proc stmts -> case stmts of
  [] -> returnA -< Nothing
  (stmt:rest) -> case stmt of
    Label labelName -> do
      body <- currentMethodBody -< ()
      let clauses = filter (\clause -> fromLabel clause == labelName && Label (toLabel clause) `elem` stmts) (catchClauses body)
      tryCatch (U.pi1 >>> runStatements) catchException -< (rest,clauses)
    Tableswitch e cases -> runSwitch -< (e,cases)
    Lookupswitch e cases -> runSwitch -< (e,cases)
    Identity l e _ -> first ((lookup_ *** eval) >>> write) >>> U.pi2 >>> runStatements -< ((l,e),rest)
    IdentityNoType l e -> first ((lookup_ *** eval) >>> write) >>> U.pi2 >>> runStatements -< ((l,e),rest)
    If e label -> first eval >>> if_ runStatementsFromLabel runStatements -< (e,(label,rest))
    Goto label -> runStatementsFromLabel -< label
    Ret e -> liftAMaybe eval -< e
    Return e -> liftAMaybe eval -< e
    Throw e -> eval >>> DynamicException ^>> fail -< e
    Assign var e -> do
      v <- eval -< e
      case var of
        LocalVar l -> first lookup_ >>> write -< (l,v)
        ReferenceVar ref -> case ref of
          ArrayRef l i -> first (readLocal *** eval) >>> updateIndex -< ((l,i),v)
          FieldRef l f -> first readLocal >>> updateField -< (l,(f,v))
          SignatureRef f -> first lookupField >>> write -< (f,v)
          _ -> fail -< StaticException $ printf "Can only assign a reference or a local variable"
      runStatements -< rest
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
    handleException = proc (val,clause) -> do
      addr <- alloc -< val
      extendEnv' runStatementsFromLabel -< ("@caughtexception",addr,withLabel clause)
    runSwitch = first eval >>> case_
        runStatementsFromLabel
        ((StaticException . printf "No cases match value %s" . show) ^>> fail)

runMethod :: CanInterp env addr const val c => c (Method,Maybe val,[Expr]) (Maybe val)
runMethod = proc (method,this,args) -> case methodBody method of
  EmptyBody -> returnA -< Nothing
  FullBody{declarations=decs,statements=stmts} -> do
    argVals <- U.map eval -< args
    let thisPair = maybe [] (\x -> [("@this",x)]) this
    let paramPairs = zip (map (\i -> "@parameter" ++ show i) [(0 :: Int)..]) argVals
    decPairs <- U.map (second defaultValue) -< concatMap (\(t,d) -> zip d (repeat t)) decs
    env <- createMethodEnv -< thisPair ++ paramPairs ++ decPairs
    localEnv (local runStatements) -< (env,(method,stmts))
  where
    createMethodEnv = proc pairs -> do
      e <- emptyEnv -< ()
      U.fold (proc (env,(s,v)) -> do
        addr <- alloc -< v
        extendEnv -< (s,addr,env)) -< (pairs,e)

runProgram :: CanInterp env addr const val c => c [Expr] (Maybe val)
runProgram = proc args -> do
  units <- askCompilationUnits -< ()
  fields <- askFields >>> Map.toList ^>> U.map (second addrFromInt) -< ()
  U.map (second defaultValue >>> write) -< map (\(FieldSignature _ t _,a) -> (a,t)) fields
  U.map ((\u -> find (\m -> methodName m == "<clinit>") [m | MethodMember m <- fileBody u])
    ^>> liftAMaybe ((\m -> (m,Nothing,[])) ^>> runMethod)) -< Map.elems units
  mainMethod <- ask -< ()
  tryCatch runMethod (U.pi2 >>> proc exception -> case exception of
    DynamicException v -> deepDeref >>> DynamicException ^>> fail -< v
    StaticException _ -> fail -< exception) -< (mainMethod,Nothing,args)

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
  eq :: c (v,v) v
  neq :: c (v,v) v
  gt :: c (v,v) v
  ge :: c (v,v) v
  lt :: c (v,v) v
  le :: c (v,v) v
  shl :: c (v,v) v
  shr :: c (v,v) v
  ushr :: c (v,v) v
  plus :: c (v,v) v
  minus :: c (v,v) v
  mult :: c (v,v) v
  div :: c (v,v) v
  lengthOf :: c v v
  neg :: c v v
  deref :: c v v
  deepDeref :: c v v
  defaultValue :: c Type v
  instanceOf :: c (v,Type) v
  cast :: c ((v,Type),v) v
  readIndex :: c (v,v) v
  updateIndex :: c ((v,v),v) ()
  readField :: c (v,FieldSignature) v
  updateField :: c (v,(FieldSignature,v)) ()

class Arrow c => UseFlow v c | c -> v where
  if_ :: c String (Maybe v) -> c [Statement] (Maybe v) -> c (v,(String,[Statement])) (Maybe v)
  case_ :: c String (Maybe v) -> c v (Maybe v) -> c (v,[CaseStatement]) (Maybe v)
  catch :: c (v,CatchClause) (Maybe v) -> c (v,[CatchClause]) (Maybe v)

class Arrow c => UseMem env addr c | c -> env addr where
  emptyEnv :: c () (env String addr)
  addrFromInt :: c Int addr

class Arrow c => UseConst c where
  askCompilationUnits :: c () CompilationUnits
  askFields :: c () Fields
