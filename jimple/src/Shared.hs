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

import           Prelude hiding (lookup,read,mod,rem,div,id)

import           Data.List (find,elemIndex)
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

type Addr = Int
type PointerEnv e = e String Addr
type CompilationUnits = Map String CompilationUnit
type Fields = Map FieldSignature Addr
type MethodReader = Method

type CanFail v c = (ArrowChoice c,ArrowFail (Exception v) c)
type CanUseEnv env c = ArrowMaybeEnv String Addr (PointerEnv env) c
type CanUseStore v c = ArrowMaybeStore Addr v c
type CanUseReader c = ArrowReader MethodReader c
type CanUseState c = ArrowState Addr c
type CanUseConst const c = ArrowConst const c
type CanCatch e x y c = ArrowTryCatch e x (Maybe y) c

type CanUseMem env v c = (CanUseEnv env c,CanUseStore v c)

type CanInterp env const v c = (Show v,
                                UseVal v c,
                                UseFlow v c,
                                UseMem env c,
                                UseConst c,
                                CanFail v c,
                                CanUseMem env v c,
                                CanUseConst const c,
                                CanUseReader c,
                                CanUseState c,
                                CanCatch (Exception v) EInvoke v c,
                                CanCatch (Exception v) [Statement] v c,
                                CanCatch (Exception v) (Method,Maybe v,[Expr]) v c)

getFieldSignatures :: CompilationUnit -> ([Modifier] -> Bool) -> [FieldSignature]
getFieldSignatures unit p =
  let toFieldSignature :: Member -> [FieldSignature]
      toFieldSignature (FieldMember f) =
        [FieldSignature (fileName unit) (fieldType f) (fieldName f) | p (fieldModifiers f)]
      toFieldSignature _ = []
  in concatMap toFieldSignature (fileBody unit)

getInitializedFields :: (UseVal v c,CanFail v c,UseConst c,CanUseConst const c) => c String [(FieldSignature,v)]
getInitializedFields = proc c -> do
  unit <- readCompilationUnit -< c
  let fieldSignatures = getFieldSignatures unit (\m -> Static `notElem` m)
  ownFields <- mapA (second defaultValue) -< map (\s@(FieldSignature _ t' _) -> (s,t')) fieldSignatures
  case extends unit of
    Just p -> do
      parentFields <- getInitializedFields -< p
      returnA -< parentFields ++ ownFields
    Nothing -> returnA -< ownFields

lookupLocal :: (Show name,CanFail val c,ArrowMaybeEnv name addr env c) => c name addr
lookupLocal = proc x -> do
  v <- lookup -< x
  case v of
    Just y -> returnA -< y
    Nothing -> failA -< StaticException $ printf "Variable %s not bounded" (show x)

lookupField :: (UseVal v c,UseConst c,CanFail v c,CanUseConst const c) => c FieldSignature Addr
lookupField = proc x -> do
  fields <- askFields -< ()
  case Map.lookup x fields of
    Just addr -> returnA -< addr
    Nothing -> failA -< StaticException $ printf "Field %s not bounded" (show x)

readVar :: (Show var,CanFail val c,ArrowMaybeStore var val c) => c var val
readVar = proc x -> do
  v <- read -< x
  case v of
    Just y -> returnA -< y
    Nothing -> failA -< StaticException $ printf "Address %s not bounded" (show x)

readLocal :: (Show name,Show addr,CanFail val c,ArrowMaybeEnv name addr env c,ArrowMaybeStore addr val c) => c name val
readLocal = lookupLocal >>> readVar

readCompilationUnit :: (CanFail v c,UseConst c,CanUseConst const c) => c String CompilationUnit
readCompilationUnit = proc n -> do
  compilationUnits <- askCompilationUnits -< ()
  case Map.lookup n compilationUnits of
    Just x -> returnA -< x
    Nothing -> failA -< StaticException $ printf "CompilationUnit %s not loaded" (show n)

matchesSignature :: Type -> String -> [Type] -> Member -> Bool
matchesSignature retType n argTypes (MethodMember m) =
  methodName m == n && returnType m == retType && parameters m == argTypes
matchesSignature _ _ _ _ = False

readMethod :: (CanFail v c,UseConst c,CanUseConst const c) => c MethodSignature Method
readMethod = proc (MethodSignature c retType n argTypes) -> do
  unit <- readCompilationUnit -< c
  case find (matchesSignature retType n argTypes) (fileBody unit) of
    Just (MethodMember m) -> returnA -< m
    _ -> failA -< StaticException $ printf "Method %s not defined for class %s" (show n) (show c)

alloc :: (UseVal v c,CanUseState c,CanUseStore v c) => c v Addr
alloc = proc val -> do
  addr <- getA -< ()
  write -< (addr,val)
  putA -< succ addr
  returnA -< addr

assert :: (CanFail v c) => c (Bool,String) ()
assert = proc (prop,msg) -> if prop
  then returnA -< ()
  else failA -< StaticException msg

evalInvoke :: CanInterp e const v c => c EInvoke (Maybe v)
evalInvoke =
  let ev = proc (localName,methodSignature,args) -> do
        method <- readMethod -< methodSignature
        case localName of
          Just n -> do
            assert -< (Static `notElem` methodModifiers method,"Expected a non-static method for non-static invoke")
            this <- lookupLocal >>> readVar -< n
            runMethod -< (method,Just this,args)
          Nothing -> do
            assert -< (Static `elem` methodModifiers method,"Expected a static method for static invoke")
            runMethod -< (method,Nothing,args)
  in proc e -> case e of
    SpecialInvoke localName methodSignature args -> ev -< (Just localName,methodSignature,args)
    VirtualInvoke localName methodSignature args -> ev -< (Just localName,methodSignature,args)
    InterfaceInvoke localName methodSignature args -> ev -< (Just localName,methodSignature,args)
    StaticInvoke methodSignature args -> ev -< (Nothing,methodSignature,args)
    DynamicInvoke{} -> failA -< StaticException "DynamicInvoke is not implemented"

eval :: CanInterp e const v c => c Expr v
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
    vs <- mapA eval -< is
    newArray -< (t,vs)
  CastExpr t i -> do
    v <- eval -< i
    b <- instanceOf -< (v,t)
    cast -< (v,t,b)
  InstanceOfExpr i t -> first eval >>> instanceOf -< (i,t)
  InvokeExpr invokeExpr -> do
    v <- tryCatchA evalInvoke (pi2 >>> failA) -< invokeExpr
    case v of
      Just v' -> returnA -< v'
      Nothing -> failA -< StaticException "Method returned nothing"
  ArrayRef l i -> (readLocal *** eval) >>> readIndex -< (l,i)
  FieldRef l f -> first readLocal >>> readField -< (l,f)
  SignatureRef fieldSignature -> lookupField >>> readVar -< fieldSignature
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
  Local localName -> lookupLocal >>> readVar -< localName
  DoubleConstant f -> doubleConstant -< f
  FloatConstant f -> floatConstant -< f
  IntConstant n -> intConstant -< n
  LongConstant f -> longConstant -< f
  NullConstant -> nullConstant -< ()
  StringConstant s -> stringConstant -< s
  ClassConstant c -> classConstant -< c
  MethodHandle _ -> failA -< StaticException "Evaluation of method handles is not implemented"

runStatements :: CanInterp e const v c => c [Statement] (Maybe v)
runStatements = proc stmts -> case stmts of
  [] -> returnA -< Nothing
  (stmt:rest) -> case stmt of
    Label _ -> tryCatchA ((\(_:r) -> r) ^>> runStatements) (proc (Label labelName:stmts,exception) ->
      case exception of
        StaticException _ -> failA -< exception
        DynamicException val -> do
          body <- currentMethodBody -< ()
          let clauses = filter (\clause -> fromLabel clause == labelName && Label (toLabel clause) `elem` stmts) (catchClauses body)
          catch (((id >>^ (\x -> ("@caughtexception",x))) *** (withLabel ^>> statementsFromLabel)) >>> runIdentity) -< (val,clauses)) -< stmts
    Tableswitch e cases -> first eval >>> runCases -< (e,cases)
    Lookupswitch e cases -> first eval >>> runCases -< (e,cases)
    Identity l e _ -> first (second eval) >>> runIdentity -< ((l,e),rest)
    IdentityNoType l e -> first (second eval) >>> runIdentity -< ((l,e),rest)
    If e label -> do
      v <- eval -< e
      if_ (statementsFromLabel >>> runStatements) runStatements -< (v,(label,rest))
    Goto label -> (statementsFromLabel >>> runStatements) -< label
    -- -- Nop
    Ret e -> runReturn -< e
    Return e -> runReturn -< e
    Throw immediate -> do
      v <- eval -< immediate
      failA -< DynamicException v
    _ -> do
      runStatement -< stmt
      runStatements -< rest
  where
    currentMethodBody = proc () -> do
      m <- askA -< ()
      case methodBody m of
        EmptyBody -> failA -< StaticException $ printf "Empty body for method %s" (show m)
        FullBody{} -> returnA -< methodBody m
    statementsFromLabel = proc label -> do
      b <- currentMethodBody -< ()
      case Label label `elemIndex` statements b of
        Just i -> returnA -< drop i (statements b)
        Nothing -> failA -< StaticException $ printf "Undefined label: %s" label
    runCases = case_ (statementsFromLabel >>> runStatements)
    runReturn = proc e -> case e of
      Just immediate -> eval >>^ Just -< immediate
      Nothing -> returnA -< Nothing
    runStatement = proc stmt -> case stmt of
      Assign var e -> do
        v <- eval -< e
        case var of
          LocalVar l -> first lookupLocal >>> write -< (l,v)
          ReferenceVar ref -> case ref of
            ArrayRef l i -> first (readLocal *** eval) >>> updateIndex -< ((l,i),v)
            FieldRef l f -> first readLocal >>> updateField -< (l,(f,v))
            SignatureRef f -> first lookupField >>> write -< (f,v)
            _ -> failA -< StaticException $ printf "Can only assign a reference or a local variable"
      Invoke e -> voidA evalInvoke -< e
      _ -> returnA -< ()
    runIdentity = proc ((l,v),rest) -> do
      addr <- alloc -< v
      env <- getEnv -< ()
      env' <- extendEnv -< (l,addr,env)
      localEnv runStatements -< (env',rest)

runMethod :: CanInterp e const v c => c (Method,Maybe v,[Expr]) (Maybe v)
runMethod = proc (method,this,args) -> case methodBody method of
  EmptyBody -> returnA -< Nothing
  FullBody{declarations=decs,statements=stmts} -> do
    argVals <- mapA eval -< args
    let thisPair = case this of
          Just x -> [("@this",x)]
          Nothing -> []
    let paramPairs = zip (map (\i -> "@parameter" ++ show i) [0..]) argVals
    decPairs <- mapA (second defaultValue) -< concatMap (\(t,d) -> zip d (repeat t)) decs
    env <- createMethodEnv -< thisPair ++ paramPairs ++ decPairs
    localEnv (localA runStatements) -< (env,(method,stmts))
  where
    createMethodEnv = proc pairs -> do
      e <- emptyEnv -< ()
      foldA (proc (env,(s,v)) -> do
        addr <- alloc -< v
        extendEnv -< (s,addr,env)) -< (pairs,e)

runProgram :: CanInterp e const v c => c [Expr] (Maybe v)
runProgram = proc args -> do
  units <- askCompilationUnits -< ()
  fields <- askFields -< ()
  mapA (second defaultValue >>> write) -< map (\(FieldSignature _ t _,a) -> (a,t)) (Map.toList fields)
  mapA (proc unit -> do
    let getMethod :: Member -> [Method]
        getMethod (MethodMember m) = [m]
        getMethod _ = []
        getMethods = concatMap getMethod
    case find (\m -> methodName m == "<clinit>") $ getMethods (fileBody unit) of
      Just classInitMethod -> runMethod -< (classInitMethod,Nothing,[])
      Nothing -> returnA -< Nothing) -< Map.elems units
  mainMethod <- askA -< ()
  tryCatchA runMethod (pi2 >>> proc exception -> case exception of
    DynamicException v -> do
      v' <- unbox -< v
      failA -< DynamicException v'
    StaticException _ -> failA -< exception) -< (mainMethod,Nothing,args)

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
  unbox :: c v v
  defaultValue :: c Type v
  instanceOf :: c (v,Type) v
  cast :: c (v,Type,v) v
  readIndex :: c (v,v) v
  updateIndex :: c ((v,v),v) ()
  readField :: c (v,FieldSignature) v
  updateField :: c (v,(FieldSignature,v)) ()

class Arrow c => UseFlow v c | c -> v where
  if_ :: c String (Maybe v) -> c [Statement] (Maybe v) -> c (v,(String,[Statement])) (Maybe v)
  case_ :: c String (Maybe v) -> c (v,[CaseStatement]) (Maybe v)
  catch :: c (v,CatchClause) (Maybe v) -> c (v,[CatchClause]) (Maybe v)

class Arrow c => UseMem e c | c -> e where
  emptyEnv :: c () (PointerEnv e)

class Arrow c => UseConst c where
  askCompilationUnits :: c () CompilationUnits
  askFields :: c () Fields
