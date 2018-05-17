{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Concrete where

import Prelude hiding (lookup, read)

import           Data.Fixed
import           Data.List (elemIndex, find)

import           Data.Concrete.Error

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Concrete.Environment (Env)
import qualified Data.Concrete.Environment as E
import qualified Data.Concrete.Store as S

import           Control.Category hiding ((.))

import           Control.Arrow
import           Control.Arrow.Const
import           Control.Arrow.Debug
import           Control.Arrow.MaybeEnvironment
import           Control.Arrow.Fail
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.MaybeStore
import           Control.Arrow.TryCatch
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Concrete.MaybeEnvironment
import           Control.Arrow.Transformer.Concrete.MaybeStore

import           Text.Printf
import           Debug.Trace

import           Syntax

-- TODO Use Text over String

data Val
  = VBottom
  | VInt Int
  | VFloat Float
  | VString String
  | VClass String
  | VBool Bool
  | VNull
  | VRef Addr
  | VArray [Val]
  | VObject String (Map FieldSignature Val)
  | VExceptional Val deriving (Eq)

instance Show Val where
  show VBottom = "⊥"
  show (VInt n) = show n
  show (VFloat f) = show f
  show (VString s) = s
  show (VClass c) = "<" ++ c ++ ">"
  show (VBool b) = show b
  show VNull = "null"
  show (VRef a) = "@" ++ show a
  show (VArray v) = show v
  show (VObject c m) = show c ++ "{" ++ show m ++ "}"
  show (VExceptional val) = show val

defaultValue :: Type -> Val
defaultValue TBoolean = VInt 0
defaultValue TByte = VInt 0
defaultValue TChar = VInt 0
defaultValue TShort = VInt 0
defaultValue TInt = VInt 0
defaultValue TLong = VInt 0
defaultValue TFloat = VFloat 0.0
defaultValue TDouble = VFloat 0.0
defaultValue TNull = VNull
defaultValue (TClass _) = VNull
defaultValue (TArray _) = VNull
defaultValue _ = VBottom

type Addr = Int
type PointerEnv = Env String Addr
type VariableStore = Map Addr Val
type CompilationUnits = Map String CompilationUnit
type Fields = Map FieldSignature Addr

newtype Interp x y = Interp
  (Except Val
    (Reader (Maybe Method, Int)
      (MaybeEnvironment String Addr
        (MaybeStoreArrow Addr Val
          (State Addr
            (Const (CompilationUnits, Fields) (->)))))) x y)
  deriving (Category,Arrow,ArrowChoice)

instance ArrowTryCatch Val x y z Interp where
  tryCatchA (Interp f) (Interp g) (Interp h) = Interp $ tryCatchA f g h

instance ArrowDebug Interp where
  debug s f = proc a -> do
    b <- f -< a
    returnA -< trace (printf "%s: %s -> %s" s (show a) (show b)) b

deriving instance ArrowConst (CompilationUnits, Fields) Interp
deriving instance ArrowFail Val Interp
deriving instance ArrowReader (Maybe Method, Int) Interp
deriving instance ArrowState Addr Interp
deriving instance ArrowMaybeEnv String Addr PointerEnv Interp
deriving instance ArrowMaybeStore Addr Val Interp

lookup' :: (Show var, ArrowChoice c, ArrowFail Val c, ArrowMaybeEnv var val env c) => c var val
lookup' = proc x -> do
  v <- lookup -< x
  case v of
    Just y -> returnA -< y
    Nothing -> failA -< VString $ printf "Variable %s not bounded" (show x)

read' :: (Show var, ArrowChoice c, ArrowFail Val c, ArrowMaybeStore var val c) => c var val
read' = proc x -> do
  v <- read -< x
  case v of
    Just y -> returnA -< y
    Nothing -> failA -< VString $ printf "Address %s not bounded" (show x)

---- End of Interp type ----

runInterp :: Interp x y -> [(String, CompilationUnit)] -> [(String, Addr)] -> [(Addr, Val)] -> x -> Error Val y
runInterp (Interp f) compilationUnits env store x =
  let fields = [] -- TODO implement field initialization
  in runConst (Map.fromList compilationUnits, Map.fromList fields)
      (evalState
        (evalMaybeStore
          (runMaybeEnvironment
            (runReader
              (runExcept f)))))
  (length env + length fields, (S.fromList store, (env, ((Nothing, 0), x))))

evalConcrete :: [(String, Addr)] -> [(Addr, Val)] -> Expr -> Error Val Val
evalConcrete = runInterp eval []

runStatementsConcrete :: [(String, Addr)] -> [(Addr, Val)] -> [Statement] -> Error Val (Maybe Val)
runStatementsConcrete env store stmts = runInterp runStatements [] env store (stmts, 0)

runProgramConcrete :: [(String, CompilationUnit)] -> CompilationUnit -> [Immediate] -> Error Val (Maybe Val)
runProgramConcrete compilationUnits mainUnit args =
  runInterp runProgram compilationUnits [] [] (mainUnit, args)

type InterpConstr c = (ArrowChoice c,
                       ArrowConst (CompilationUnits, Fields) c,
                       ArrowMaybeEnv String Addr PointerEnv c,
                       ArrowFail Val c,
                       ArrowReader (Maybe Method, Int) c,
                       ArrowState Addr c,
                       ArrowMaybeStore Addr Val c,
                       ArrowTryCatch Val InvokeExpr (Maybe Val) (Maybe Val) c,
                       ArrowTryCatch Val ([Statement], Int) (Maybe Val) (Maybe Val) c,
                       ArrowTryCatch Val (Method, Maybe Val, [Immediate]) (Maybe Val) (Maybe Val) c)

---- End of Boilerplate ----

assert :: (ArrowChoice c, ArrowFail Val c) => c Bool ()
assert = proc prop ->
    if prop
    then returnA -< ()
    else failA -< VString "Assertion failed"

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace 0 x (_:t) = x:t
replace n x (h:t) = h:(replace (n-1) x t)

numToNum :: (forall a. Num a => a -> a -> a) -> Val -> Val -> Maybe Val
numToNum op v1 v2 = case (v1, v2) of
  (VInt n1, VInt n2) -> Just (VInt (op n1 n2))
  (VFloat f, VInt n) -> Just (VFloat (op f (fromIntegral n)))
  (VInt n, VFloat f) -> Just (VFloat (op (fromIntegral n) f))
  (VFloat f1, VFloat f2) -> Just (VFloat (op f1 f2))
  (_, _) -> Nothing

numToBool :: (forall a. Ord a => a -> a -> Bool) -> Val -> Val -> Maybe Val
numToBool op v1 v2 = case (v1, v2) of
  (VInt n1, VInt n2) -> Just (VBool (op n1 n2))
  (VFloat f, VInt n) -> Just (VBool (op f (fromIntegral n)))
  (VInt n, VFloat f) -> Just (VBool (op (fromIntegral n) f))
  (VFloat f1, VFloat f2) -> Just (VBool (op f1 f2))
  (_, _) -> Nothing

newArray :: (ArrowFail Val c, ArrowChoice c, ArrowState Addr c, ArrowMaybeStore Addr Val c) => c (Type, [Int]) Val
newArray =
  let createVals :: (ArrowFail Val c, ArrowChoice c, ArrowState Addr c, ArrowMaybeStore Addr Val c) => c (Int, Type, [Int]) [Val]
      createVals = proc (s', t, sizes') -> case s' of
        0 -> returnA -< []
        n -> do
          v <- newArray -< (t, sizes')
          vs <- createVals -< (n - 1, t, sizes')
          returnA -< (v:vs)
  in proc (t, sizes) -> case sizes of
    (s:sizes') -> do
      vals <- createVals -< (s, t, sizes')
      addr <- alloc -< VArray vals
      returnA -< VRef addr
    [] -> returnA -< defaultValue t

toIntList :: (ArrowChoice c, ArrowFail Val c) => c [Val] [Int]
toIntList = proc vs -> case vs of
  (VInt x:xs) -> do
    xs' <- toIntList -< xs
    returnA -< (x:xs')
  (_:_) -> failA -< VString "Expected an integer valued array for toIntList"
  [] -> returnA -< []

evalImmediateList :: (ArrowChoice c, ArrowFail Val c, ArrowMaybeEnv String Addr env c, ArrowState Addr c, ArrowMaybeStore Addr Val c) => c [Immediate] [Val]
evalImmediateList = proc xs -> case xs of
  (x':xs') -> do
    v <- evalImmediate -< x'
    vs <- evalImmediateList -< xs'
    returnA -< (v:vs)
  [] -> returnA -< []

evalImmediate :: (ArrowChoice c, ArrowFail Val c, ArrowMaybeEnv String Addr env c, ArrowState Addr c, ArrowMaybeStore Addr Val c) => c Immediate Val
evalImmediate = proc i -> case i of
  ILocalName localName -> fetchLocal -< localName
  IInt n -> returnA -< (VInt n)
  IFloat f -> returnA -< (VFloat f)
  IString s -> returnA -< (VString s)
  IClass c -> returnA -< (VClass c)
  INull -> returnA -< VNull

evalIndex :: (ArrowChoice c, ArrowFail Val c, ArrowMaybeEnv String Addr env c, ArrowState Addr c, ArrowMaybeStore Addr Val c) => c Immediate Int
evalIndex = proc i -> do
  v <- evalImmediate -< i
  case v of
    VInt n -> returnA -< n
    _ -> failA -< VString "Expected an integer array index"

evalRef :: (ArrowChoice c, ArrowFail Val c, ArrowMaybeEnv String Addr PointerEnv c, ArrowState Addr c, ArrowMaybeStore Addr Val c, ArrowConst (CompilationUnits, Fields) c) => c Reference Val
evalRef = proc ref -> case ref of
  ArrayReference localName i -> do
    n <- evalIndex -< i
    (_, VArray xs) <- fetchArrayWithAddr -< localName
    if n >= 0 && n < length xs
    then returnA -< xs !! n
    else failA -< VString "ArrayIndexOutOfBoundsException"
  FieldReference localName fieldSignature -> do
    (_, VObject _ m) <- fetchObjectWithAddr -< localName
    case Map.lookup fieldSignature m of
      Just x -> returnA -< x
      Nothing -> failA -< VString $ printf "Field %s not defined for object %s" (show fieldSignature) (show localName)
  SignatureReference fieldSignature -> fetchField -< fieldSignature

evalMethod :: InterpConstr c => c (Method, Maybe Val, [Immediate]) (Maybe Val)
evalMethod = proc (method, this, args) -> do
  argVals <- evalImmediateList -< args
  env <- createMethodEnv -< (this, parameters method, argVals)
  localEnv (proc method -> do localA runMethodBody -< ((Just method, 0), methodBody method)) -< (env, method)

evalInvoke :: InterpConstr c => c InvokeExpr (Maybe Val)
evalInvoke = proc e -> case e of
  StaticInvoke methodSignature args -> do
    method <- fetchMethod -< methodSignature
    assert -< (Static `elem` (methodModifiers method))
    evalMethod -< (method, Nothing, args)
  VirtualInvoke localName methodSignature args -> do
    method <- fetchMethod -< methodSignature
    assert -< (not (Static `elem` (methodModifiers method)))
    this <- fetchLocal -< localName
    evalMethod -< (method, Just this, args)
  SpecialInvoke localName methodSignature args -> do
    method <- fetchMethod -< methodSignature
    assert -< (not (Static `elem` (methodModifiers method)))
    this <- fetchLocal -< localName
    evalMethod -< (method, Just this, args)
  _ -> failA -< VString "Not implemented"

eval :: InterpConstr c => c Expr Val
eval = proc e -> case e of
  ENew newExpr -> case newExpr of
    NewSimple t -> if isBaseType t
      then case t of
        TClass c -> do
          compilationUnit <- fetchCompilationUnit -< c
          let fields = foldl (\acc member -> case member of
                                FieldMember f -> (FieldSignature c (fieldType f) (fieldName f), defaultValue (fieldType f)):acc
                                _ -> acc) [] (fileBody compilationUnit)
          addr <- alloc -< (VObject c (Map.fromList fields))
          returnA -< VRef addr
        _ -> returnA -< (defaultValue t)
      else failA -< VString "Expected a nonvoid base type for new"
    NewArray t i -> if isNonvoidType t
      then do
        v <- evalImmediate -< i
        ns <- toIntList -< [v]
        if all (>0) ns
          then newArray -< (t, ns)
          else failA -< VString "Expected a positive integer for newarray size"
      else failA -< VString "Expected a nonvoid type for newarray"
    NewMulti t is -> if isBaseType t
      then do
        vs <- evalImmediateList -< is
        ns <- toIntList -< vs
        if all (>0) ns
          then newArray -< (t, ns)
          else failA -< VString "Expected positive integers for newmultiarray sizes"
      else failA -< VString "Expected a nonvoid base type for newmultiarray"
  -- ECast NonvoidType Immediate
  -- EInstanceof Immediate NonvoidType
  EInvoke invokeExpr -> do
    v <- tryCatchA evalInvoke returnA failA -< invokeExpr
    case v of
      Just v' -> returnA -< v'
      Nothing -> failA -< VString "Method returned nothing"
  EReference ref -> evalRef -< ref
  EBinop i1 op i2 -> do
    v1 <- evalImmediate -< i1
    v2 <- evalImmediate -< i2
    case op of
      -- And ->
      -- Or ->
      -- Xor ->
      Mod -> case (v1, v2) of
        (VInt x1, VInt x2) -> returnA -< (VInt (x1 `mod` x2))
        (VInt x1, VFloat x2) -> returnA -< (VFloat (fromIntegral x1 `mod'` x2))
        (VFloat x1, VInt x2) -> returnA -< (VFloat (x1 `mod'` fromIntegral x2))
        (VFloat x1, VFloat x2) -> returnA -< (VFloat (x1 `mod'` x2))
        (_, _) -> failA -< VString "Expected two numbers as arguments for mod"
      -- Rem ->
      -- Cmp ->
      -- Cmpg ->
      -- Cmpl ->
      Cmpeq -> returnA -< (VBool (v1 == v2))
      Cmpne -> returnA -< (VBool (v1 /= v2))
      Cmpgt -> case numToBool (>) v1 v2 of
        Just v -> returnA -< v
        Nothing -> failA -< VString "Expected two numbers as arguments for >"
      Cmpge -> case numToBool (>=) v1 v2 of
        Just v -> returnA -< v
        Nothing -> failA -< VString "Expected two numbers as arguments for >="
      Cmplt -> case numToBool (<) v1 v2 of
        Just v -> returnA -< v
        Nothing -> failA -< VString "Expected two numbers as arguments for <"
      Cmple -> case numToBool (<=) v1 v2 of
        Just v -> returnA -< v
        Nothing -> failA -< VString "Expected two numbers as arguments for <="
      -- Shl ->
      -- Shr ->
      -- Ushr ->
      Plus -> case numToNum (+) v1 v2 of
        Just v -> returnA -< v
        Nothing -> failA -< VString "Expected two numbers as arguments for +"
      Minus -> case numToNum (-) v1 v2 of
        Just v -> returnA -< v
        Nothing -> failA -< VString "Expected two numbers as arguments for -"
      Mult -> case numToNum (*) v1 v2 of
        Just v -> returnA -< v
        Nothing -> failA -< VString "Expected two numbers as arguments for *"
      Div -> case (v1, v2) of
        (_, VInt 0) -> failA -< VString "Cannot divide by zero"
        (_, VFloat 0.0) -> failA -< VString "Cannot divide by zero"
        (VInt n1, VInt n2) -> returnA -< (VInt (n1 `div` n2))
        (VFloat f, VInt n) -> returnA -< (VFloat (f / fromIntegral n))
        (VInt n, VFloat f) -> returnA -< (VFloat (fromIntegral n / f))
        (VFloat f1, VFloat f2) -> returnA -< (VFloat (f1 / f2))
        (_, _) -> failA -< VString "Expected two numbers as arguments for /"
  EUnop op i -> do
    v <- evalImmediate -< i
    case op of
      Lengthof -> case v of
        VRef addr -> do
          v' <- read' -< addr
          case v' of
            VArray xs -> returnA -< (VInt (length xs))
            _ -> failA -< VString "Expected an array as argument for lengthof"
        _ -> failA -< VString "Expected an array as argument for lengthof"
      Neg -> case v of
        VInt n -> returnA -< (VInt (-n))
        VFloat f -> returnA -< (VFloat (-f))
        _ -> failA -< VString "Expected a number as argument for -"
  EImmediate i -> do
    v <- evalImmediate -< i
    returnA -< v
  _ -> failA -< VString "Undefined expression"

unbox :: (ArrowChoice c, ArrowFail Val c, ArrowMaybeEnv String Addr PointerEnv c, ArrowState Addr c, ArrowMaybeStore Addr Val c) => c Val Val
unbox = proc val -> do
  case val of
    VRef addr -> read' -< addr
    _ -> returnA -< val

fetchLocal :: (ArrowChoice c, ArrowFail Val c, ArrowMaybeEnv String Addr env c, ArrowMaybeStore Addr Val c) => c String Val
fetchLocal = proc x -> do
  addr <- lookup' -< x
  read' -< addr

fetchField :: (ArrowChoice c, ArrowFail Val c, ArrowMaybeStore Addr Val c, ArrowConst (CompilationUnits, Fields) c) => c FieldSignature Val
fetchField = proc x -> do
  (_,fields) <- askConst -< ()
  let addr = Map.lookup x fields
  case addr of
    Just a -> do
      read' -< a
    Nothing -> failA -< VString $ printf "Field %s not bound" (show x)

fetchCompilationUnit :: (ArrowChoice c, ArrowFail Val c, ArrowConst (CompilationUnits, Fields) c) => c String CompilationUnit
fetchCompilationUnit = proc n -> do
  (compilationUnits, _) <- askConst -< ()
  let compilationUnit = Map.lookup n compilationUnits
  case compilationUnit of
    Just x -> returnA -< x
    Nothing -> failA -< VString $ printf "CompilationUnit %s not loaded" (show n)

alloc :: (ArrowState Addr c, ArrowMaybeStore Addr Val c) => c Val Addr
alloc = proc val -> do
  addr <- getA -< ()
  write -< (addr, val)
  putA -< succ addr
  returnA -< addr

matchesSignature :: Type -> String -> [Type] -> Member -> Bool
matchesSignature retType n argTypes (MethodMember m) =
  methodName m == n && returnType m == retType && parameters m == argTypes
matchesSignature _ _ _ _ = False

fetchMethod :: (ArrowChoice c, ArrowFail Val c, ArrowConst (CompilationUnits, Fields) c) => c MethodSignature Method
fetchMethod = proc (MethodSignature c retType n argTypes) -> do
  (compilationUnits, _) <- askConst -< ()
  let compilationUnit = Map.lookup c compilationUnits
  case compilationUnit of
    Just v -> case find (matchesSignature retType n argTypes) (fileBody v) of
      Just (MethodMember m) -> returnA -< m
      _ -> failA -< VString $ printf "Method %s not defined for class %s" (show n) (show c)
    Nothing -> failA -< VString $ printf "Undefined class %s" (show c)

fetchRefValWithAddr :: (ArrowChoice c, ArrowFail Val c, ArrowMaybeEnv String Addr env c, ArrowMaybeStore Addr Val c) => c String (Addr, Val)
fetchRefValWithAddr = proc localName -> do
  v <- fetchLocal -< localName
  case v of
    VRef addr -> do
      v' <- read' -< addr
      returnA -< (addr, v')
    _ -> failA -< VString $ printf "Variable %s is not a reference" (show localName)

fetchArrayWithAddr :: (ArrowChoice c, ArrowFail Val c, ArrowMaybeEnv String Addr PointerEnv c, ArrowMaybeStore Addr Val c) => c String (Addr, Val)
fetchArrayWithAddr = proc localName -> do
  (addr, v) <- fetchRefValWithAddr -< localName
  case v of
    VArray _ -> returnA -< (addr, v)
    _ -> failA -< VString $ printf "Variable %s not bound to an array" (show localName)

fetchObjectWithAddr :: (ArrowChoice c, ArrowFail Val c, ArrowMaybeEnv String Addr PointerEnv c, ArrowMaybeStore Addr Val c) => c String (Addr, Val)
fetchObjectWithAddr = proc localName -> do
  (addr, v) <- fetchRefValWithAddr -< localName
  case v of
    VObject _ _ -> returnA -< (addr, v)
    _ -> failA -< VString $ printf "Variable %s not bound to an object" (show localName)

goto :: InterpConstr c => c ([Statement], String) (Maybe Val)
goto = proc (stmts, label) -> case Label label `elemIndex` stmts of
  Just i -> runStatements -< (stmts, i)
  Nothing -> failA -< VString $ printf "Undefined label: %s" label

matchCases :: (ArrowChoice c, ArrowFail Val c, ArrowMaybeEnv String Addr PointerEnv c, ArrowState Addr c, ArrowMaybeStore Addr Val c) => c ([CaseStatement], Int) String
matchCases = proc (cases, v) -> case cases of
  ((CLConstant n, label): cases') -> if v == n
    then returnA -< label
    else matchCases -< (cases', v)
  ((CLDefault, label): _) -> returnA -< label
  [] -> failA -< VString $ printf "No cases match value %s" (show v)

createParamEnv :: (ArrowChoice c, ArrowFail Val c, ArrowMaybeEnv String Addr PointerEnv c, ArrowState Addr c, ArrowMaybeStore Addr Val c) => c (Int, [Val]) PointerEnv
createParamEnv = proc (i, params) -> case params of
  (param:rest) -> do
    let toParam :: Int -> String
        toParam n = "@parameter" ++ show n

    env <- createParamEnv -< (i + 1, rest)
    addr <- alloc -< param
    extendEnv -< (toParam i, addr, env)
  [] -> returnA -< E.empty

createMethodEnv :: (ArrowChoice c, ArrowFail Val c, ArrowMaybeEnv String Addr PointerEnv c, ArrowState Addr c, ArrowMaybeStore Addr Val c) => c (Maybe Val, [Type], [Val]) PointerEnv
createMethodEnv = proc (this, _, params) -> do
  paramEnv <- createParamEnv -< (0, params)
  case this of
    Just val -> do
      addr <- alloc -< val
      extendEnv -< ("@this", addr, paramEnv)
    Nothing -> returnA -< paramEnv

getMethodBody :: (ArrowChoice c, ArrowFail Val c, ArrowReader (Maybe Method, Addr) c) => c () MethodBody
getMethodBody = proc () -> do
  (m, _) <- askA -< ()
  case m of
    Just m' ->
      case methodBody m' of
        MEmpty -> failA -< VString $ printf "Method %s has no body" (show m)
        MFull{} -> returnA -< methodBody m'
    Nothing -> failA -< VString "No method currently running"

catchExceptions :: InterpConstr c => [CatchClause] -> c Val (Maybe Val)
catchExceptions cs = proc (val) -> do
  case val of
    VRef addr -> do
      o <- read' -< addr
      case o of
        VObject c _ -> case filter (\clause -> className clause == c) cs of
          (clause:_) -> do
            (_, i) <- askA -< ()
            body <- getMethodBody -< ()
            let stmts = statements body
            let iFrom = elemIndex (Label (fromLabel clause)) stmts
            let iTo   = elemIndex (Label (toLabel clause)) stmts
            let iWith = elemIndex (Label (withLabel clause)) stmts
            case (iFrom, iTo, iWith) of
              (Just iFrom', Just iTo', Just iWith') -> if i >= iFrom' && i < iTo'
                then do
                  env <- getEnv -< ()
                  addr' <- alloc -< val
                  env' <- extendEnv -< ("@caughtexception", addr', env)
                  localEnv runStatements -< (env', (stmts, iWith'))
                else failA -< val
              _ -> failA -< val
          [] -> failA -< val
        _ -> failA -< val
    _ -> failA -< val

runStatements :: InterpConstr c => c ([Statement], Int) (Maybe Val)
runStatements = proc (stmts, i) -> if i == length stmts
  then returnA -< Nothing
  else case stmts !! i of
    -- Label labelName -> do
      -- let fltr = proc (n, b) ->
      --       returnA -< (filter (\c -> n == fromLabel c) (catchClauses b))
      -- body <- getMethodBody -< ()
      -- cs <- fltr -< (labelName, body)
      -- tryCatchA runStatements returnA (catchExceptions cs) -< (stmts, i + 1)
    -- Breakpoint
    -- Entermonitor Immediate
    -- Exitmonitor Immediate
    Tableswitch immediate cases -> do
      v <- evalImmediate -< immediate
      case v of
        VInt x -> do
          label <- matchCases -< (cases, x)
          goto -< (stmts, label)
        _ -> failA -< VString "Expected an integer as argument for switch"
    Lookupswitch immediate cases -> do
      v <- evalImmediate -< immediate
      case v of
        VInt x -> do
          label <- matchCases -< (cases, x)
          goto -< (stmts, label)
        _ -> failA -< VString "Expected an integer as argument for switch"
    Identity localName atId _ -> do
      addr <- lookup' -< (show atId)
      env <- getEnv -< ()
      env' <- extendEnv -< (localName, addr, env)
      localEnv runStatements -< (env', (stmts, i + 1))
    IdentityNoType localName atId -> do
      addr <- lookup' -< (show atId)
      env <- getEnv -< ()
      env' <- extendEnv -< (localName, addr, env)
      localEnv runStatements -< (env', (stmts, i + 1))
    Assign var e -> do
      v <- eval -< e
      case var of
        VLocal localName -> do
          addr <- lookup' -< localName
          write -< (addr, v)
          runStatements -< (stmts, i + 1)
        VReference ref -> case ref of
          ArrayReference localName index -> do
            n <- evalIndex -< index
            (addr, VArray xs) <- fetchArrayWithAddr -< localName
            if n >= 0 && n < length xs
            then do
              let xs' = replace n v xs
              write -< (addr, VArray xs')
              runStatements -< (stmts, i + 1)
            else failA -< VString "ArrayIndexOutOfBoundsException"
          FieldReference localName fieldSignature -> do
            (addr, VObject c m) <- fetchObjectWithAddr -< localName
            case m Map.!? fieldSignature of
              Just _ -> do
                let m' = Map.insert fieldSignature v m
                write -< (addr, VObject c m')
                runStatements -< (stmts, i + 1)
              Nothing -> failA -< VString $ printf "FieldSignature %s not defined on object %s: (%s)" (show fieldSignature) (show localName) (show m)
          SignatureReference fieldSignature -> failA -< VString "SignatureReference is not yet implemented"
    If e label -> do
      v <- eval -< e
      case v of
        VBool True -> goto -< (stmts, label)
        VBool False -> runStatements -< (stmts, i + 1)
        _ -> failA -< VString "Expected a boolean expression for if statement"
    Goto label -> goto -< (stmts, label)
    -- Nop
    Ret e -> case e of
      Just immediate -> do
        v <- evalImmediate -< immediate
        returnA -< Just v
      Nothing -> returnA -< Nothing
    Return e -> case e of
      Just immediate -> do
        v <- evalImmediate -< immediate
        returnA -< Just v
      Nothing -> returnA -< Nothing
    Throw immediate -> do
      v <- evalImmediate -< immediate
      failA -< v
    Invoke e -> do
      evalInvoke -< e
      runStatements -< (stmts, i + 1)
    _ -> runStatements -< (stmts, i + 1)

runDeclaration :: (ArrowChoice c, ArrowFail Val c, ArrowMaybeEnv String Addr PointerEnv c, ArrowState Addr c, ArrowMaybeStore Addr Val c) => c (Env String Addr, Declaration) PointerEnv
runDeclaration = proc (env, dec) -> case dec of
  (t, (d:rest)) -> do
    env' <- runDeclaration -< (env, (t, rest))
    addr <- alloc -< defaultValue t
    env'' <- extendEnv -< (d, addr, env')
    returnA -< env''
  (_, []) -> returnA -< env

runDeclarations :: (ArrowChoice c, ArrowFail Val c, ArrowMaybeEnv String Addr PointerEnv c, ArrowState Addr c, ArrowMaybeStore Addr Val c) => c (Env String Addr, [Declaration]) PointerEnv
runDeclarations = proc (env, decs) -> case decs of
  (dec:rest) -> do
    env' <- runDeclarations -< (env, rest)
    env'' <- runDeclaration -< (env', dec)
    returnA -< env''
  [] -> returnA -< env

runMethodBody :: InterpConstr c => c MethodBody (Maybe Val)
runMethodBody = proc body -> case body of
  MEmpty -> returnA -< Nothing
  MFull{declarations=d,statements=s} -> do
    env <- getEnv -< ()
    env' <- runDeclarations -< (env, d)
    v <- localEnv runStatements -< (env', (s, 0))
    returnA -< v

unboxErrorRef :: (ArrowChoice c, ArrowFail Val c, ArrowMaybeEnv String Addr PointerEnv c, ArrowState Addr c, ArrowMaybeStore Addr Val c) => c Val (Maybe Val)
unboxErrorRef = unbox >>> failA

unboxResultRef :: (ArrowChoice c, ArrowFail Val c, ArrowMaybeEnv String Addr PointerEnv c, ArrowState Addr c, ArrowMaybeStore Addr Val c) => c (Maybe Val) (Maybe Val)
unboxResultRef = proc val -> case val of
  Just x -> do
    x' <- unbox -< x
    returnA -< Just x'
  Nothing -> returnA -< Nothing

runProgram :: InterpConstr c => c (CompilationUnit, [Immediate]) (Maybe Val)
runProgram = proc (mainUnit, args) -> do
  let findMethodByName :: [Member] -> String -> Maybe Method
      findMethodByName (MethodMember m:rest) name =
        if methodName m == name
        then Just m
        else findMethodByName rest name
      findMethodByName (_:rest) name = findMethodByName rest name
      findMethodByName [] _ = Nothing
  case findMethodByName (fileBody mainUnit) "<clinit>" of
    Just classInitMethod -> do
      evalMethod -< (classInitMethod, Nothing, [])
      case findMethodByName (fileBody mainUnit) "main" of
        Just mainMethod -> (evalMethod >>> unboxResultRef) -< (mainMethod, Nothing, args)
        Nothing -> unboxResultRef -< Nothing
    Nothing -> do
      case findMethodByName (fileBody mainUnit) "main" of
        Just mainMethod -> tryCatchA (evalMethod >>> unboxResultRef) (proc x -> returnA -< x) unboxErrorRef -< (mainMethod, Nothing, args)
        Nothing -> unboxResultRef -< Nothing
