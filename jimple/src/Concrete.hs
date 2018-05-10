{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Concrete where

import Prelude hiding (lookup, read)

import           Data.Fixed
import           Data.List (replicate, elemIndex, find)

import           Data.Concrete.Error

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Hashable
import qualified Data.Concrete.Store as S
import           Data.Concrete.Environment (Env)
import qualified Data.Concrete.Environment as E

import           Control.Category hiding ((.))

import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.State
import           Control.Arrow.Store
import           Control.Arrow.Environment
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Concrete.Store
import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Concrete.Environment

import           GHC.Generics (Generic)

import           Syntax

-- Use Text over String

data Val
  = VBottom
  | VInt Int
  | VFloat Float
  | VString String
  | VClass String
  | VBool Bool
  | VNull
  | VArray [Val]
  | VObject String (Map String Val) deriving (Eq)

instance Show Val where
  show VBottom = "⊥"
  show (VInt n) = show n
  show (VFloat f) = show f
  show (VString s) = s
  show (VClass c) = "<" ++ c ++ ">"
  show (VBool b) = show b
  show VNull = "null"
  show (VArray v) = show v
  show (VObject t m) = show t ++ "(" ++ show m ++ ")"

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
defaultValue _ = VBottom

defaultArray :: Type -> [Val] -> Val
defaultArray t (VInt d:ds) = VArray (replicate d (defaultArray t ds))
defaultArray _ (_:_) = VBottom
defaultArray t [] = defaultValue t

data Addr
  = LocalAddr Int
  | FieldAddr FieldSignature
  | FileAddr String deriving (Show, Eq, Generic)

instance Hashable Addr

data GeneralVal
  = FileVal File
  | VariableVal (Type, Val) deriving (Show, Eq)

-- Use BoundedEnvironment for store

newtype Interp x y = Interp (StoreArrow Addr GeneralVal (Environment String Addr (State Int (Except String (->)))) x y)
deriving instance Category Interp
deriving instance Arrow Interp
deriving instance ArrowChoice Interp
deriving instance ArrowFail String Interp
deriving instance ArrowState Int Interp
deriving instance ArrowEnv String Addr (Env String Addr) Interp
deriving instance ArrowStore Addr GeneralVal () Interp
deriving instance ArrowEnv String Addr (Env String Addr) (StoreArrow Addr GeneralVal (Environment String Addr (State Int (Except String (->)))))

runInterp :: Interp x y -> [(String, Addr)] -> [(Addr, GeneralVal)] -> x -> Error String y
runInterp (Interp f) env store x =
  runExcept
    (evalState
      (runEnvironment
        (evalStore f)))
  (length env, (env, (S.fromList store, x)))

evalConcrete :: [(String, Addr)] -> [(Addr, GeneralVal)] -> Expr -> Error String Val
evalConcrete = runInterp eval

runStatementsConcrete :: [(String, Addr)] -> [(Addr, GeneralVal)] -> [Statement] -> Error String (Maybe Val)
runStatementsConcrete env store stmts = runInterp runStatements env store (stmts, 0)

runFileConcrete :: File -> [Immediate] -> Error String (Maybe Val)
runFileConcrete file args =
  let store = [(FileAddr (fileName file), FileVal file)]
  in runInterp runFile [] store (file, args)

---- End of Boilerplate ----

assert :: (ArrowChoice c, ArrowFail String c) => c Bool ()
assert = proc prop ->
    if prop
    then returnA -< ()
    else failA -< "Assertion failed"

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

isPositiveVInt :: Val -> Bool
isPositiveVInt (VInt n) = n > 0
isPositiveVInt _ = False

isLocalVal :: GeneralVal -> Bool
isLocalVal (FileVal _) = False
isLocalVal (VariableVal _) = True

evalImmediateList :: (ArrowChoice c, ArrowFail String c, ArrowEnv String Addr env c, ArrowStore Addr GeneralVal () c, ArrowState Int c) => c [Immediate] [Val]
evalImmediateList = proc xs -> case xs of
  (x':xs') -> do
    v <- evalImmediate -< x'
    vs <- evalImmediateList -< xs'
    returnA -< (v:vs)
  [] -> returnA -< []

evalImmediate :: (ArrowChoice c, ArrowFail String c, ArrowEnv String Addr env c, ArrowStore Addr GeneralVal () c, ArrowState Int c) => c Immediate Val
evalImmediate = proc i -> case i of
  ILocalName localName -> do
    (_, v) <- fetchLocal -< localName
    returnA -< v
  IInt n -> returnA -< (VInt n)
  IFloat f -> returnA -< (VFloat f)
  IString s -> returnA -< (VString s)
  IClass c -> returnA -< (VClass c)
  INull -> returnA -< VNull

evalRef :: (ArrowChoice c, ArrowFail String c, ArrowEnv String Addr env c, ArrowStore Addr GeneralVal () c, ArrowState Int c) => c Reference Val
evalRef = proc ref -> case ref of
  ArrayReference localName i -> do
    v1 <- evalImmediate -< i
    case v1 of
      VInt n -> do
        (_, v2) <- fetchLocal -< localName
        case v2 of
          VArray xs -> if n >= 0 && n < length xs
            then returnA -< xs !! n
            else failA -< "ArrayIndexOutOfBoundsException"
          _ -> failA -< "Expected an array to lookup in"
      _ -> failA -< "Expected an integer as array index"
  FieldReference localName (FieldSignature c _ n) -> do
    (_, v) <- fetchLocal -< localName
    case v of
      VObject c' m -> if c == c'
        then case Map.lookup n m of
          Just x -> returnA -< x
          Nothing -> failA -< "Field " ++ n ++ " not defined for class " ++ c'
        else failA -< "ClassNames do not correspond"
      _ -> failA -< "Expected an object to lookup in"
  SignatureReference fieldSignature -> do
    (_, v) <- fetchField -< fieldSignature
    returnA -< v

evalMethod :: (ArrowChoice c, ArrowFail String c, ArrowEnv String Addr (Env String Addr) c, ArrowStore Addr GeneralVal () c, ArrowState Int c) => c (Method, Maybe Addr, [Immediate]) (Maybe Val)
evalMethod = proc (method, this, args) -> do
  env <- createMethodEnv -< (this, parameters method, args)
  v <- localEnv runMethodBody -< (env, methodBody method)
  returnA -< v

evalInvoke :: (ArrowChoice c, ArrowFail String c, ArrowEnv String Addr (Env String Addr) c, ArrowStore Addr GeneralVal () c, ArrowState Int c) => c InvokeExpr (Maybe Val)
evalInvoke = proc e -> case e of
  StaticInvoke methodSignature args -> do
    method <- fetchMethod -< methodSignature
    assert -< (Static `elem` (methodModifiers method))
    evalMethod -< (method, Nothing, args)
  VirtualInvoke localName methodSignature args -> do
    method <- fetchMethod -< methodSignature
    assert -< (not (Static `elem` (methodModifiers method)))
    thisAddr <- lookup -< localName
    evalMethod -< (method, Just thisAddr, args)
  SpecialInvoke localName methodSignature args -> do
    method <- fetchMethod -< methodSignature
    assert -< (not (Static `elem` (methodModifiers method)))
    thisAddr <- lookup -< localName
    evalMethod -< (method, Just thisAddr, args)
  _ -> failA -< "Not implemented"

eval :: (ArrowChoice c, ArrowFail String c, ArrowEnv String Addr (Env String Addr) c, ArrowStore Addr GeneralVal () c, ArrowState Int c) => c Expr Val
eval = proc e -> case e of
  ENew newExpr -> case newExpr of
    NewSimple t -> if isBaseType t
      then returnA -< (defaultValue t)
      else failA -< "Expected a nonvoid base type for new"
    NewArray t i -> if isNonvoidType t
      then do
        v <- evalImmediate -< i
        if isPositiveVInt v
          then returnA -< (defaultArray t [v])
          else failA -< "Expected a positive integer for newarray size"
      else failA -< "Expected a nonvoid type for newarray"
    NewMulti t is -> if isBaseType t
      then do
        vs <- evalImmediateList -< is
        if all isPositiveVInt vs
          then returnA -< (defaultArray t vs)
          else failA -< "Expected positive integers for newmultiarray sizes"
      else failA -< "Expected a nonvoid base type for newmultiarray"
  -- ECast NonvoidType Immediate
  -- EInstanceof Immediate NonvoidType
  EInvoke invokeExpr -> do
    v <- evalInvoke -< invokeExpr
    case v of
      Just v' -> returnA -< v'
      Nothing -> failA -< "Method returned nothing"
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
        (_, _) -> failA -< "Expected two numbers as arguments for mod"
      -- Rem ->
      -- Cmp ->
      -- Cmpg ->
      -- Cmpl ->
      Cmpeq -> returnA -< (VBool (v1 == v2))
      Cmpne -> returnA -< (VBool (v1 /= v2))
      Cmpgt -> case numToBool (>) v1 v2 of
        Just v -> returnA -< v
        Nothing -> failA -< "Expected two numbers as arguments for >"
      Cmpge -> case numToBool (>=) v1 v2 of
        Just v -> returnA -< v
        Nothing -> failA -< "Expected two numbers as arguments for >="
      Cmplt -> case numToBool (<) v1 v2 of
        Just v -> returnA -< v
        Nothing -> failA -< "Expected two numbers as arguments for <"
      Cmple -> case numToBool (<=) v1 v2 of
        Just v -> returnA -< v
        Nothing -> failA -< "Expected two numbers as arguments for <="
      -- Shl ->
      -- Shr ->
      -- Ushr ->
      Plus -> case numToNum (+) v1 v2 of
        Just v -> returnA -< v
        Nothing -> failA -< "Expected two numbers as arguments for +"
      Minus -> case numToNum (-) v1 v2 of
        Just v -> returnA -< v
        Nothing -> failA -< "Expected two numbers as arguments for -"
      Mult -> case numToNum (*) v1 v2 of
        Just v -> returnA -< v
        Nothing -> failA -< "Expected two numbers as arguments for *"
      Div -> case (v1, v2) of
        (_, VInt 0) -> failA -< "Cannot divide by zero"
        (_, VFloat 0.0) -> failA -< "Cannot divide by zero"
        (VInt n1, VInt n2) -> returnA -< (VInt (n1 `div` n2))
        (VFloat f, VInt n) -> returnA -< (VFloat (f / fromIntegral n))
        (VInt n, VFloat f) -> returnA -< (VFloat (fromIntegral n / f))
        (VFloat f1, VFloat f2) -> returnA -< (VFloat (f1 / f2))
        (_, _) -> failA -< "Expected two numbers as arguments for /"
  EUnop op i -> do
    v <- evalImmediate -< i
    case op of
      Lengthof -> case v of
        VArray xs -> returnA -< (VInt (length xs))
        _ -> failA -< "Expected an array as argument for lengthof"
      Neg -> case v of
        VInt n -> returnA -< (VInt (-n))
        VFloat f -> returnA -< (VFloat (-f))
        _ -> failA -< "Expected a number as argument for -"
  EImmediate i -> do
    v <- evalImmediate -< i
    returnA -< v
  _ -> failA -< "Undefined expression"

fetchLocal :: (ArrowChoice c, ArrowFail String c, ArrowEnv String Addr env c, ArrowStore Addr GeneralVal () c, ArrowState Int c) => c String (Type, Val)
fetchLocal = proc s -> do
  addr <- lookup -< s
  val <- read -< (addr, ())
  case val of
    VariableVal v -> returnA -< v
    FileVal _ -> failA -< "Incorrect value bound"

fetchField :: (ArrowChoice c, ArrowFail String c, ArrowEnv String Addr env c, ArrowStore Addr GeneralVal () c, ArrowState Int c) => c FieldSignature (Type, Val)
fetchField = proc fs -> do
  gv <- read -< (FieldAddr fs, ())
  case gv of
    VariableVal v -> returnA -< v
    FileVal _ -> failA -< "Incorrect value bound"

alloc :: (ArrowState Int c) => c () Addr
alloc = proc _ -> do
  addr <- getA -< ()
  putA -< (succ addr)
  returnA -< (LocalAddr addr)

matchesSignature :: Type -> String -> [Type] -> Member -> Bool
matchesSignature retType n argTypes (MethodMember m) =
  methodName m == n && returnType m == retType && parameters m == argTypes
matchesSignature _ _ _ _ = False

fetchMethod :: (ArrowChoice c, ArrowFail String c, ArrowEnv String Addr (Env String Addr) c, ArrowStore Addr GeneralVal () c, ArrowState Int c) => c MethodSignature Method
fetchMethod = proc (MethodSignature c retType n argTypes) -> do
  gv <- read -< (FileAddr c, ())
  case gv of
    FileVal v -> case find (matchesSignature retType n argTypes) (fileBody v) of
      Just (MethodMember m) -> returnA -< m
      _ -> failA -< "Method " ++ show n ++ " not defined for class " ++ show c
    _ -> failA -< "Undefined class " ++ show c

goto :: (ArrowChoice c, ArrowFail String c, ArrowEnv String Addr (Env String Addr) c, ArrowStore Addr GeneralVal () c, ArrowState Int c) => c ([Statement], String) (Maybe Val)
goto = proc (stmts, label) -> case Label label `elemIndex` stmts of
  Just i -> runStatements -< (stmts, i)
  Nothing -> failA -< "Undefined label: " ++ label

matchCases :: (ArrowChoice c, ArrowFail String c, ArrowEnv String Addr (Env String Addr) c, ArrowStore Addr GeneralVal () c, ArrowState Int c) => c ([CaseStatement], Int) String
matchCases = proc (cases, v) -> case cases of
  ((CLConstant n, label): cases') -> if v == n
    then returnA -< label
    else matchCases -< (cases', v)
  ((CLDefault, label): _) -> returnA -< label
  [] -> failA -< "No cases match value " ++ show v

createParamEnv :: (ArrowChoice c, ArrowFail String c, ArrowEnv String Addr (Env String Addr) c, ArrowStore Addr GeneralVal () c, ArrowState Int c) => c (Int, [(Type, Val)]) (Env String Addr)
createParamEnv = proc (i, params) -> case params of
  (param:rest) -> do
    let toParam :: Int -> String
        toParam n = "@parameter" ++ show n

    env <- createParamEnv -< (i + 1, rest)
    addr <- alloc -< ()
    write -< (addr, VariableVal param, ())
    env' <- extendEnv -< (toParam i, addr, env)
    returnA -< env'
  [] -> returnA -< E.empty

createMethodEnv :: (ArrowChoice c, ArrowFail String c, ArrowEnv String Addr (Env String Addr) c, ArrowStore Addr GeneralVal () c, ArrowState Int c) => c (Maybe Addr, [Type], [Immediate]) (Env String Addr)
createMethodEnv = proc (this, paramTypes, params) -> do
  paramVals <- evalImmediateList -< params
  let typedParamVals = zip paramTypes paramVals
  paramEnv <- createParamEnv -< (0, typedParamVals)
  case this of
    Just addr -> do
      env <- extendEnv -< ("@this", addr, paramEnv)
      returnA -< env
    Nothing -> returnA -< paramEnv

runStatements :: (ArrowChoice c, ArrowFail String c, ArrowEnv String Addr (Env String Addr) c, ArrowStore Addr GeneralVal () c, ArrowState Int c) => c ([Statement], Int) (Maybe Val)
runStatements = proc (stmts, i) -> if i == length stmts
  then returnA -< Nothing
  else case stmts !! i of
    -- Label LabelName
    -- Breakpoint
    -- Entermonitor Immediate
    -- Exitmonitor Immediate
    Tableswitch immediate cases -> do
      v <- evalImmediate -< immediate
      case v of
        VInt x -> do
          label <- matchCases -< (cases, x)
          goto -< (stmts, label)
        _ -> failA -< "Expected an integer as argument for switch"
    Lookupswitch immediate cases -> do
      v <- evalImmediate -< immediate
      case v of
        VInt x -> do
          label <- matchCases -< (cases, x)
          goto -< (stmts, label)
        _ -> failA -< "Expected an integer as argument for switch"
    Identity localName atId t -> do
      (t', _) <- fetchLocal -< (show atId)
      if t == t'
      then do
        addr <- lookup -< (show atId)
        env <- getEnv -< ()
        env' <- extendEnv -< (localName, addr, env)
        localEnv runStatements -< (env', (stmts, i + 1))
      else
        failA -< "Incorrect type " ++ show t ++ " for variable"
    IdentityNoType localName atId -> do
      addr <- lookup -< (show atId)
      env <- getEnv -< ()
      env' <- extendEnv -< (localName, addr, env)
      localEnv runStatements -< (env', (stmts, i + 1))
    Assign var e -> do
      v <- eval -< e
      case var of
        VLocal localName -> do
          (t, _) <- fetchLocal -< localName
          addr <- lookup -< localName
          write -< (addr, VariableVal (t, v), ())
          runStatements -< (stmts, i + 1)
        VReference _ -> failA -< "Undefined yet" -- evalRef -< ref
    If e label -> do
      v <- eval -< e
      case v of
        VBool True -> goto -< (stmts, label)
        VBool False -> runStatements -< (stmts, i + 1)
        _ -> failA -< "Expected a boolean expression for if statement"
    Goto label -> goto -< (stmts, label)
    -- Nop
    -- Ret (Maybe Immediate)
    Return e -> case e of
      Just immediate -> do
        v <- evalImmediate -< immediate
        returnA -< Just v
      Nothing -> returnA -< Nothing
    -- Throw Immediate
    Invoke e -> do
      evalInvoke -< e
      runStatements -< (stmts, i + 1)
    _ -> runStatements -< (stmts, i + 1)

runDeclaration :: (ArrowChoice c, ArrowFail String c, ArrowEnv String Addr (Env String Addr) c, ArrowStore Addr GeneralVal () c, ArrowState Int c) => c (Env String Addr, Declaration) (Env String Addr)
runDeclaration = proc (env, dec) -> case dec of
  (t, (d:rest)) -> do
    env' <- runDeclaration -< (env, (t, rest))
    addr <- alloc -< ()
    write -< (addr, VariableVal (t, defaultValue t), ())
    env'' <- extendEnv -< (d, addr, env')
    returnA -< env''
  (_, []) -> returnA -< env

runDeclarations :: (ArrowChoice c, ArrowFail String c, ArrowEnv String Addr (Env String Addr) c, ArrowStore Addr GeneralVal () c, ArrowState Int c) => c (Env String Addr, [Declaration]) (Env String Addr)
runDeclarations = proc (env, decs) -> case decs of
  (dec:rest) -> do
    env' <- runDeclarations -< (env, rest)
    env'' <- runDeclaration -< (env', dec)
    returnA -< env''
  [] -> returnA -< env

runMethodBody :: (ArrowChoice c, ArrowFail String c, ArrowEnv String Addr (Env String Addr) c, ArrowStore Addr GeneralVal () c, ArrowState Int c) => c MethodBody (Maybe Val)
runMethodBody = proc body -> case body of
  MEmpty -> returnA -< Nothing
  MFull{declarations=d,statements=s,catchClauses=c} -> do
    env <- getEnv -< ()
    env' <- runDeclarations -< (env, d)
    v <- localEnv runStatements -< (env', (s, 0))
    returnA -< v

runFile :: (ArrowChoice c, ArrowFail String c, ArrowEnv String Addr (Env String Addr) c, ArrowStore Addr GeneralVal () c, ArrowState Int c) => c (File, [Immediate]) (Maybe Val)
runFile = proc (file, args) -> do
  let findMethodByName :: [Member] -> String -> Maybe Method
      findMethodByName (MethodMember m:rest) name =
        if methodName m == name
        then Just m
        else findMethodByName rest name
      findMethodByName (_:rest) name = findMethodByName rest name
      findMethodByName [] _ = Nothing
  case findMethodByName (fileBody file) "<clinit>" of
    Just classInitMethod -> do
      evalMethod -< (classInitMethod, Nothing, [])
      case findMethodByName (fileBody file) "main" of
        Just mainMethod -> evalMethod -< (mainMethod, Nothing, args)
        Nothing -> returnA -< Nothing
    Nothing -> do
      case findMethodByName (fileBody file) "main" of
        Just mainMethod -> evalMethod -< (mainMethod, Nothing, args)
        Nothing -> returnA -< Nothing
