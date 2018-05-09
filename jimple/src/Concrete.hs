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

import Prelude hiding (lookup)

import           Data.Fixed
import           Data.List (replicate, elemIndex, find)

import           Data.Concrete.Error

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Hashable
import           Data.Concrete.Environment (Env)
import qualified Data.Concrete.Environment as E

import           Control.Category hiding ((.))

import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Environment
import           Control.Arrow.Transformer.Concrete.Environment
import           Control.Arrow.Transformer.Concrete.Except

import           GHC.Generics (Generic)

import           Syntax

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

data Pointer
  = FilePointer String
  | FieldPointer FieldSignature
  | LocalPointer String deriving (Show, Eq, Generic)

instance Hashable Pointer

data GeneralVal
  = FileVal File
  | FieldVal (Maybe Val)
  | LocalVal (Type, Maybe Val) deriving (Show, Eq)

newtype Interp x y = Interp (Environment Pointer GeneralVal (Except String (->)) x y)
deriving instance Category Interp
deriving instance Arrow Interp
deriving instance ArrowChoice Interp
deriving instance ArrowFail String Interp
deriving instance ArrowEnv Pointer GeneralVal (Env Pointer GeneralVal) Interp

runInterp :: Interp x y -> [(Pointer, GeneralVal)] -> x -> Error String y
runInterp (Interp f) env x =
  runExcept
    (runEnvironment f)
  (env, x)

evalConcrete :: [(Pointer, GeneralVal)] -> Expr -> Error String Val
evalConcrete = runInterp eval

runStatementsConcrete :: [(Pointer, GeneralVal)] -> [Statement] -> Error String (Maybe Val)
runStatementsConcrete env stmts = runInterp runStatements env (stmts, 0)

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
defaultValue (TClass c) = VObject c Map.empty
defaultValue _ = VBottom

defaultArray :: Type -> [Val] -> Val
defaultArray t (VInt d:ds) = VArray (replicate d (defaultArray t ds))
defaultArray _ (_:_) = VBottom
defaultArray t [] = defaultValue t

isPositiveVInt :: Val -> Bool
isPositiveVInt (VInt n) = n > 0
isPositiveVInt _ = False

isLocalVal :: GeneralVal -> Bool
isLocalVal (FileVal _) = False
isLocalVal (FieldVal _) = False
isLocalVal (LocalVal _) = True

evalImmediateList :: (ArrowChoice c, ArrowFail String c, ArrowEnv Pointer GeneralVal env c) => c [Immediate] [Val]
evalImmediateList = proc xs -> case xs of
  (x':xs') -> do
    v <- evalImmediate -< x'
    vs <- evalImmediateList -< xs'
    returnA -< (v:vs)
  [] -> returnA -< []

evalImmediate :: (ArrowChoice c, ArrowFail String c, ArrowEnv Pointer GeneralVal env c) => c Immediate Val
evalImmediate = proc i -> case i of
  ILocalName localName -> do
    (_, v) <- lookupLocal -< localName
    case v of
      Just v' -> returnA -< v'
      Nothing -> failA -< "Variable " ++ localName ++ " not yet initialized"
  IInt n -> returnA -< (VInt n)
  IFloat f -> returnA -< (VFloat f)
  IString s -> returnA -< (VString s)
  IClass c -> returnA -< (VClass c)
  INull -> returnA -< VNull

evalRef :: (ArrowChoice c, ArrowFail String c, ArrowEnv Pointer GeneralVal env c) => c Reference Val
evalRef = proc ref -> case ref of
  ArrayReference localName i -> do
    v1 <- evalImmediate -< i
    case v1 of
      VInt n -> do
        (_, v2) <- lookupLocal -< localName
        case v2 of
          Just v2' -> case v2' of
            VArray xs -> if n >= 0 && n < length xs
              then returnA -< xs !! n
              else failA -< "ArrayIndexOutOfBoundsException"
            _ -> failA -< "Expected an array to lookup in"
          Nothing -> failA -< "Array not yet initialized"
      _ -> failA -< "Expected an integer as array index"
  FieldReference localName (FieldSignature c _ n) -> do
    (_, v) <- lookupLocal -< localName
    case v of
      Just v' -> case v' of
        VObject c' m -> if c == c'
          then case Map.lookup n m of
            Just x -> returnA -< x
            Nothing -> failA -< "Field " ++ n ++ " not defined for class " ++ c'
          else failA -< "ClassNames do not correspond"
        _ -> failA -< "Expected an object to lookup in"
      Nothing -> failA -< "Object not yet initialized"
  SignatureReference fieldSignature -> do
    (_, v) <- lookupField -< fieldSignature
    case v of
      Just v' -> returnA -< v'
      Nothing -> failA -< "Field not yet initialized"

eval :: (ArrowChoice c, ArrowFail String c, ArrowEnv Pointer GeneralVal (Env Pointer GeneralVal) c) => c Expr Val
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
  EInvoke expr -> case expr of
    StaticInvoke methodSignature@(MethodSignature _ _ _ argTypes) args -> do
      body <- lookupMethod -< methodSignature
      env <- getEnv -< ()
      env' <- createMethodEnv -< (env, argTypes, args)
      v <- localEnv runMethodBody -< (env', body)
      case v of
        Just v' -> returnA -< v'
        Nothing -> failA -< "Should change method signature"
    _ -> failA -< "Not implemented"
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

lookupLocal :: (ArrowChoice c, ArrowFail String c, ArrowEnv Pointer GeneralVal env c) => c String (Type, Maybe Val)
lookupLocal = proc s -> do
  gv <- lookup -< (LocalPointer s)
  case gv of
    LocalVal v -> returnA -< v
    _ -> failA -< "Incorrect value bound"

lookupField :: (ArrowChoice c, ArrowFail String c, ArrowEnv Pointer GeneralVal env c) => c FieldSignature (Type, Maybe Val)
lookupField = proc fs@(FieldSignature _ t _) -> do
  gv <- lookup -< (FieldPointer fs)
  case gv of
    FieldVal v -> returnA -< (t, v)
    _ -> failA -< "Incorrect value bound"

matchesSignature :: Type -> String -> [Type] -> Member -> Bool
matchesSignature retType n argTypes Method{ methodName = a
                                          , returnType = b
                                          , parameters = c } = n == a && b == retType && c == argTypes
matchesSignature _ _ _ _ = False

lookupMethod :: (ArrowChoice c, ArrowFail String c, ArrowEnv Pointer GeneralVal env c) => c MethodSignature MethodBody
lookupMethod = proc (MethodSignature c retType n argTypes) -> do
  gv <- lookup -< (FilePointer c)
  case gv of
    FileVal v -> case find (matchesSignature retType n argTypes) (fileBody v) of
      Just m -> returnA -< (methodBody m)
      Nothing -> failA -< "Undefined method " ++ show n ++ " for class " ++ show c
    _ -> failA -< "Undefined class " ++ show c

goto :: (ArrowChoice c, ArrowFail String c, ArrowEnv Pointer GeneralVal (Env Pointer GeneralVal) c) => c ([Statement], String) (Maybe Val)
goto = proc (stmts, label) -> case Label label `elemIndex` stmts of
  Just i -> runStatements -< (stmts, i)
  Nothing -> failA -< "Undefined label: " ++ label

matchCases :: (ArrowChoice c, ArrowFail String c, ArrowEnv Pointer GeneralVal env c) => c ([CaseStatement], Int) String
matchCases = proc (cases, v) -> case cases of
  ((CLConstant n, label): cases') -> if v == n
    then returnA -< label
    else matchCases -< (cases', v)
  ((CLDefault, label): _) -> returnA -< label
  [] -> failA -< "No cases match value " ++ show v

createMethodEnv :: (ArrowChoice c, ArrowFail String c, ArrowEnv Pointer GeneralVal (Env Pointer GeneralVal) c) => c (Env Pointer GeneralVal, [Type], [Immediate]) (Env Pointer GeneralVal)
createMethodEnv = proc (env, paramTypes, args) -> do
  let filteredEnv = filter (\(_, v) -> (not . isLocalVal) v) (E.toList env)
  argVals <- evalImmediateList -< args
  let typedArgVals = zip paramTypes argVals
  let toParam :: Integer -> Pointer
      toParam n = LocalPointer ("@parameter" ++ show n)
  let toLocal (t, v) = LocalVal (t, Just v)
  let argEnv = zip (map toParam [0..]) (map toLocal typedArgVals)
  returnA -< (E.fromList (filteredEnv ++ argEnv))

runStatements :: (ArrowChoice c, ArrowFail String c, ArrowEnv Pointer GeneralVal (Env Pointer GeneralVal) c) => c ([Statement], Int) (Maybe Val)
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
      (t', v) <- lookupLocal -< (show atId)
      case v of
        Just v' -> if t == t'
          then do
            env <- getEnv -< ()
            env' <- extendEnv -< (LocalPointer localName, LocalVal (t, Just v'), env)
            localEnv runStatements -< (env', (stmts, i + 1))
          else failA -< "Incorrect type " ++ show t ++ " for variable"
        Nothing -> failA -< "Undefined identifier " ++ show atId
    IdentityNoType localName atId -> do
      (t', v) <- lookupLocal -< (show atId)
      case v of
        Just v' -> do
          env <- getEnv -< ()
          env' <- extendEnv -< (LocalPointer localName, LocalVal (t', Just v'), env)
          localEnv runStatements -< (env', (stmts, i + 1))
        Nothing -> failA -< "Undefined identifier " ++ show atId
    Assign var e -> do
      v <- eval -< e
      case var of
        VLocal localName -> do
          (t, _) <- lookupLocal -< localName
          env <- getEnv -< ()
          env' <- extendEnv -< (LocalPointer localName, LocalVal (t, Just v), env)
          localEnv runStatements -< (env', (stmts, i + 1))
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
      eval -< EInvoke e
      runStatements -< (stmts, i + 1)
    _ -> runStatements -< (stmts, i + 1)

runMethodBody :: (ArrowChoice c, ArrowFail String c, ArrowEnv Pointer GeneralVal (Env Pointer GeneralVal) c) => c MethodBody (Maybe Val)
runMethodBody = proc body -> case body of
  MEmpty -> returnA -< Nothing
  MFull{declarations=d,statements=s,catchClauses=c} -> do
    env <- getEnv -< ()
    env' <- initLocalEnv -< (env, d)
    v <- localEnv runStatements -< (env', (s, 0))
    returnA -< v

initLocalEnv :: (ArrowChoice c, ArrowFail String c, ArrowEnv Pointer GeneralVal (Env Pointer GeneralVal) c) => c (Env Pointer GeneralVal, [Declaration]) (Env Pointer GeneralVal)
initLocalEnv = proc (env, d) -> do
  let dec2env t s = (LocalPointer s, LocalVal (t, Nothing))
  let decs2env (t, decs) = map (dec2env t) decs
  let decEnv = concatMap decs2env d
  returnA -< (E.fromList (E.toList env ++ decEnv))
