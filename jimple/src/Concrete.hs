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

import           Control.Category

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

---- Values ----
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

---- End of Values ----

---- Interp Type ----

type Addr = Int
type PointerEnv = Env String Addr
type VariableStore = Map Addr Val
type CompilationUnits = Map String CompilationUnit
type Fields = Map FieldSignature Addr
type MethodReader = (Maybe Method, Int, [CatchClause])

newtype Interp x y = Interp
  (Except Val
    (Reader MethodReader
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
deriving instance ArrowReader MethodReader Interp
deriving instance ArrowState Addr Interp
deriving instance ArrowMaybeEnv String Addr PointerEnv Interp
deriving instance ArrowMaybeStore Addr Val Interp

---- End of Interp type ----

---- Program Boilerplate ----

runInterp :: Interp x y -> [CompilationUnit] -> [(String, Addr)] -> [(Addr, Val)] -> Maybe Method -> x -> Error Val y
runInterp (Interp f) files env store mainMethod x =
  let compilationUnits = map (\file -> (fileName file, file)) files
      toFieldSignature :: String -> Member -> [FieldSignature]
      toFieldSignature c (FieldMember field) =
        [FieldSignature c (fieldType field) (fieldName field) | Static `elem` fieldModifiers field]
      toFieldSignature _ _ = []
      getFields unit = concatMap (toFieldSignature (fileName unit)) (fileBody unit)
      latestAddr = case map snd env ++ map fst store of
        [] -> 0
        addrs -> maximum addrs
      fields = zip (concatMap getFields files) [latestAddr..]
  in runConst (Map.fromList compilationUnits, Map.fromList fields)
      (evalState
        (evalMaybeStore
          (runMaybeEnvironment
            (runReader
              (runExcept f)))))
  (latestAddr + length fields, (S.fromList store, (env, ((mainMethod, 0, []), x))))

---- End of Program Boilerplate ----

---- Constraint Boilerplate ----

type CanFail c = (ArrowChoice c, ArrowFail Val c)
type CanEnv c = ArrowMaybeEnv String Addr PointerEnv c
type CanStore c = ArrowMaybeStore Addr Val c
type CanUseReader c = ArrowReader MethodReader c
type CanUseState c = ArrowState Addr c
type CanUseConst c = ArrowConst (CompilationUnits, Fields) c
type CanCatch x c = ArrowTryCatch Val x (Maybe Val) (Maybe Val) c

type CanUseMem c = (CanEnv c, CanStore c)

type CanInterp c = (CanFail c,
                    CanUseMem c,
                    CanUseConst c,
                    CanUseReader c,
                    CanUseState c,
                    CanCatch InvokeExpr c,
                    CanCatch ([Statement], Int) c,
                    CanCatch (Method, Maybe Val, [Immediate]) c)

---- End of Constraint Boilerplate ----

---- Boilerplate Methods ----

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace 0 x (_:t) = x:t
replace n x (h:t) = h:replace (n-1) x t

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

lookup' :: (Show var, CanFail c, ArrowMaybeEnv var val env c) => c var val
lookup' = proc x -> do
  v <- lookup -< x
  case v of
    Just y -> returnA -< y
    Nothing -> failA -< VString $ printf "Variable %s not bounded" (show x)

read' :: (Show var, CanFail c, ArrowMaybeStore var val c) => c var val
read' = proc x -> do
  v <- read -< x
  case v of
    Just y -> returnA -< y
    Nothing -> failA -< VString $ printf "Address %s not bounded" (show x)

assert :: (CanFail c) => c Bool ()
assert = proc prop ->
    if prop
    then returnA -< ()
    else failA -< VString "Assertion failed"

toIntList :: (CanFail c) => c [Val] [Int]
toIntList = proc vs -> case vs of
  (VInt x:xs) -> do
    xs' <- toIntList -< xs
    returnA -< (x:xs')
  (_:_) -> failA -< VString "Expected an integer valued array for toIntList"
  [] -> returnA -< []

unbox :: (CanFail c, CanUseMem c) => c Val Val
unbox = proc val -> case val of
  VRef addr -> read' -< addr
  _ -> returnA -< val

fetchLocal :: (CanFail c, CanUseMem c) => c String Val
fetchLocal = proc x -> do
  addr <- lookup' -< x
  read' -< addr

fetchField :: (CanFail c, CanStore c, CanUseConst c) => c FieldSignature Val
fetchField = proc x -> do
  (_,fields) <- askConst -< ()
  let addr = Map.lookup x fields
  case addr of
    Just a -> read' -< a
    Nothing -> failA -< VString $ printf "Field %s not bound" (show x)

fetchCompilationUnit :: (CanFail c, CanUseConst c) => c String CompilationUnit
fetchCompilationUnit = proc n -> do
  (compilationUnits, _) <- askConst -< ()
  let compilationUnit = Map.lookup n compilationUnits
  case compilationUnit of
    Just x -> returnA -< x
    Nothing -> failA -< VString $ printf "CompilationUnit %s not loaded" (show n)

alloc :: (CanUseState c, CanStore c) => c Val Addr
alloc = proc val -> do
  addr <- getA -< ()
  write -< (addr, val)
  putA -< succ addr
  returnA -< addr

matchesSignature :: Type -> String -> [Type] -> Member -> Bool
matchesSignature retType n argTypes (MethodMember m) =
  methodName m == n && returnType m == retType && parameters m == argTypes
matchesSignature _ _ _ _ = False

fetchMethod :: (CanFail c, CanUseConst c) => c MethodSignature Method
fetchMethod = proc (MethodSignature c retType n argTypes) -> do
  (compilationUnits, _) <- askConst -< ()
  let compilationUnit = Map.lookup c compilationUnits
  case compilationUnit of
    Just v -> case find (matchesSignature retType n argTypes) (fileBody v) of
      Just (MethodMember m) -> returnA -< m
      _ -> failA -< VString $ printf "Method %s not defined for class %s" (show n) (show c)
    Nothing -> failA -< VString $ printf "Undefined class %s" (show c)

fetchRefValWithAddr :: (CanFail c, CanUseMem c) => c String (Addr, Val)
fetchRefValWithAddr = proc localName -> do
  v <- fetchLocal -< localName
  case v of
    VRef addr -> do
      v' <- read' -< addr
      returnA -< (addr, v')
    _ -> failA -< VString $ printf "Variable %s is not a reference" (show localName)

fetchArrayWithAddr :: (CanFail c, CanUseMem c) => c String (Addr, Val)
fetchArrayWithAddr = proc localName -> do
  (addr, v) <- fetchRefValWithAddr -< localName
  case v of
    VArray _ -> returnA -< (addr, v)
    _ -> failA -< VString $ printf "Variable %s not bound to an array" (show localName)

fetchObjectWithAddr :: (CanFail c, CanUseMem c) => c String (Addr, Val)
fetchObjectWithAddr = proc localName -> do
  (addr, v) <- fetchRefValWithAddr -< localName
  case v of
    VObject _ _ -> returnA -< (addr, v)
    _ -> failA -< VString $ printf "Variable %s not bound to an object" (show localName)

getCurrentMethodBody :: (CanFail c, CanUseReader c) => c () MethodBody
getCurrentMethodBody = proc () -> do
  (m, _, _) <- askA -< ()
  case m of
    Just m' ->
      case methodBody m' of
        MEmpty -> failA -< VString $ printf "Empty body for method %s" (show m')
        MFull{} -> returnA -< methodBody m'
    Nothing -> failA -< VString "No method running"

---- End of Boilerplate Methods ----

---- Actual Evaluation Methods ----

newArray :: (CanFail c, CanUseState c, CanStore c) => c (Type, [Int]) Val
newArray =
  let createVals :: (CanFail c, CanUseState c, CanStore c) => c (Int, Type, [Int]) [Val]
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

evalImmediateList :: (CanFail c, CanUseMem c) => c [Immediate] [Val]
evalImmediateList = proc xs -> case xs of
  (x':xs') -> do
    v <- evalImmediate -< x'
    vs <- evalImmediateList -< xs'
    returnA -< (v:vs)
  [] -> returnA -< []

evalImmediate :: (CanFail c, CanUseMem c) => c Immediate Val
evalImmediate = proc i -> case i of
  ILocalName localName -> fetchLocal -< localName
  IInt n -> returnA -< (VInt n)
  IFloat f -> returnA -< (VFloat f)
  IString s -> returnA -< (VString s)
  IClass c -> returnA -< (VClass c)
  INull -> returnA -< VNull

evalIndex :: (CanFail c, CanUseMem c) => c Immediate Int
evalIndex = proc i -> do
  v <- evalImmediate -< i
  case v of
    VInt n -> returnA -< n
    _ -> failA -< VString "Expected an integer array index"

evalRef :: (CanFail c, CanUseMem c, CanUseConst c) => c Reference Val
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

evalMethod :: CanInterp c => c (Method, Maybe Val, [Immediate]) (Maybe Val)
evalMethod = proc (method, this, args) -> do
  argVals <- evalImmediateList -< args
  env <- createMethodEnv -< (this, parameters method, argVals)
  localEnv (proc method -> localA runMethodBody -< ((Just method, 0, []), methodBody method)) -< (env, method)

evalInvoke :: CanInterp c => c InvokeExpr (Maybe Val)
evalInvoke = proc e -> case e of
  StaticInvoke methodSignature args -> do
    method <- fetchMethod -< methodSignature
    assert -< (Static `elem` methodModifiers method)
    evalMethod -< (method, Nothing, args)
  VirtualInvoke localName methodSignature args -> do
    method <- fetchMethod -< methodSignature
    assert -< (Static `notElem` methodModifiers method)
    this <- fetchLocal -< localName
    evalMethod -< (method, Just this, args)
  SpecialInvoke localName methodSignature args -> do
    method <- fetchMethod -< methodSignature
    assert -< (Static `notElem` methodModifiers method)
    this <- fetchLocal -< localName
    evalMethod -< (method, Just this, args)
  _ -> failA -< VString "Not implemented"

eval :: CanInterp c => c Expr Val
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

goto :: CanInterp c => c ([Statement], String) (Maybe Val)
goto = proc (stmts, label) -> case Label label `elemIndex` stmts of
  Just i -> runStatements -< (stmts, i)
  Nothing -> failA -< VString $ printf "Undefined label: %s" label

matchCases :: (CanFail c) => c ([CaseStatement], Int) String
matchCases = proc (cases, v) -> case cases of
  ((CLConstant n, label): cases') -> if v == n
    then returnA -< label
    else matchCases -< (cases', v)
  ((CLDefault, label): _) -> returnA -< label
  [] -> failA -< VString $ printf "No cases match value %s" (show v)

createParamEnv :: (CanFail c, CanUseMem c, CanUseState c) => c (Int, [Val]) PointerEnv
createParamEnv = proc (i, params) -> case params of
  (param:rest) -> do
    let toParam :: Int -> String
        toParam n = "@parameter" ++ show n
    env <- createParamEnv -< (i + 1, rest)
    addr <- alloc -< param
    extendEnv -< (toParam i, addr, env)
  [] -> returnA -< E.empty

createMethodEnv :: (CanFail c, CanUseMem c, CanUseState c) => c (Maybe Val, [Type], [Val]) PointerEnv
createMethodEnv = proc (this, _, params) -> do
  paramEnv <- createParamEnv -< (0, params)
  case this of
    Just val -> do
      addr <- alloc -< val
      extendEnv -< ("@this", addr, paramEnv)
    Nothing -> returnA -< paramEnv

getObject :: (CanFail c, CanStore c) =>c Val Val
getObject = proc val -> case val of
  VRef addr -> do
    o <- read' -< addr
    case o of
      VObject _ _ -> returnA -< o
      _ -> failA -< val
  _ -> failA -< val

getMatchingClause :: CanFail c => c (String, [CatchClause], Val) CatchClause
getMatchingClause = proc (c, cs, val) -> case filter (\clause -> className clause == c) cs of
  (clause:_) -> returnA -< clause
  [] -> failA -< val

getMethodBody :: CanFail c => c (Maybe Method, Val) MethodBody
getMethodBody = proc (m, val) -> case m of
  Just m' ->
    case methodBody m' of
      MEmpty -> failA -< val
      MFull{} -> returnA -< methodBody m'
  Nothing -> failA -< val

catchExceptions :: CanInterp c => c Val (Maybe Val)
catchExceptions = proc val -> do
  (VObject c _) <- getObject -< val
  (m, i, cs) <- askA -< ()
  clause <- getMatchingClause -< (c, cs, val)
  body <- getMethodBody -< (m, val)
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

runStatements :: CanInterp c => c ([Statement], Int) (Maybe Val)
runStatements = proc (stmts, i) -> if i == length stmts
  then returnA -< Nothing
  else case stmts !! i of
    Label labelName -> do
      body <- getCurrentMethodBody -< ()
      (m, a, _) <- askA -< ()
      let cs = (filter (\c -> labelName == fromLabel c) (catchClauses body))
      localA (tryCatchA runStatements returnA catchExceptions) -< ((m, a, cs), (stmts, i + 1))
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

runDeclaration :: (CanFail c, CanUseMem c, CanUseState c) => c (Env String Addr, Declaration) PointerEnv
runDeclaration = proc (env, dec) -> case dec of
  (t, d:rest) -> do
    env' <- runDeclaration -< (env, (t, rest))
    addr <- alloc -< defaultValue t
    env'' <- extendEnv -< (d, addr, env')
    returnA -< env''
  (_, []) -> returnA -< env

runDeclarations :: (CanFail c, CanUseMem c, CanUseState c) => c (Env String Addr, [Declaration]) PointerEnv
runDeclarations = proc (env, decs) -> case decs of
  (dec:rest) -> do
    env' <- runDeclarations -< (env, rest)
    env'' <- runDeclaration -< (env', dec)
    returnA -< env''
  [] -> returnA -< env

runMethodBody :: CanInterp c => c MethodBody (Maybe Val)
runMethodBody = proc body -> case body of
  MEmpty -> returnA -< Nothing
  MFull{declarations=d,statements=s} -> do
    env <- getEnv -< ()
    env' <- runDeclarations -< (env, d)
    v <- localEnv runStatements -< (env', (s, 0))
    returnA -< v

unboxResultRef :: (CanFail c, CanUseMem c) => c (Maybe Val) (Maybe Val)
unboxResultRef = proc val -> case val of
  Just x -> do
    x' <- unbox -< x
    returnA -< Just x'
  Nothing -> returnA -< Nothing

runProgram :: CanInterp c => c (CompilationUnit, [Immediate]) (Maybe Val)
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
        Just mainMethod -> tryCatchA evalMethod unboxResultRef (unbox >>> failA) -< (mainMethod, Nothing, args)
        Nothing -> unboxResultRef -< Nothing
    Nothing ->
      case findMethodByName (fileBody mainUnit) "main" of
        Just mainMethod -> tryCatchA evalMethod unboxResultRef (unbox >>> failA) -< (mainMethod, Nothing, args)
        Nothing -> unboxResultRef -< Nothing

---- End of Actual Evaluation Methods ----
