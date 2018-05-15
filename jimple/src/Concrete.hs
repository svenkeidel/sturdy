{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Concrete where

import Prelude hiding (lookup, read)

import           Data.Fixed
import           Data.List (elemIndex, find)

import           Data.Concrete.Error

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Concrete.Environment (Env)
import qualified Data.Concrete.Environment as E

import           Control.Category hiding ((.))

import           Control.Arrow
import           Control.Arrow.Utils
import           Control.Arrow.Environment
import           Control.Arrow.Error
import           Control.Arrow.Fail
import           Control.Arrow.State
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Concrete.Environment

import           Text.Printf

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
  | VRef Addr
  | VArray [Val]
  | VObject (Map FieldSignature Val) deriving (Eq)

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
  show (VObject m) = "{" ++ show m ++ "}"

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

type Addr = Int

-- programState = (currentAddr, currentMethod, currentStatement, fileStore, fieldStore, variableStore)
type ProgramState = (Addr, Maybe Method, Maybe Int, Map String File, Map FieldSignature Addr, Map Addr Val)
type PointerEnv = Env String Addr

newtype Interp x y = Interp (Except Val (Environment String Addr (State ProgramState (->))) x y)
  deriving (Category,Arrow,ArrowChoice)

instance ArrowError Val (Method, Maybe Val, [Immediate]) (Maybe Val) Interp where
  tryWithErrorA (Interp f) (Interp g) (Interp h) = Interp (tryWithErrorA f g h)

instance ArrowError (ProgramState, (PointerEnv, Val)) (ProgramState, (PointerEnv, (Method, Maybe Val, [Immediate]))) (ProgramState, (PointerEnv, Maybe Val)) (Except Val (->)) where
  tryWithErrorA (Except f) (Except g) (Except h) =
    let st env = (length env, Nothing, Nothing, Map.empty, Map.empty, Map.empty)
    in Except $ (f &&& pi2) >>> hasSucceededRes' ^>> (g ||| ((\(v, (env, _)) -> (st env, (env, v))) ^>> h))

hasSucceededRes' :: (Error e b,a) -> Either b (e,a)
hasSucceededRes' (Fail e,a) = Right (e,a)
hasSucceededRes' (Success b,_) = Left b

deriving instance ArrowFail Val Interp
deriving instance ArrowState ProgramState Interp
deriving instance ArrowEnv String Addr PointerEnv Interp

runInterp :: Interp x y -> [(String, File)] -> [(String, Addr)] -> [(Addr, Val)] -> x -> Error Val y
runInterp (Interp f) files env store x =
  let state = (length env, Nothing, Nothing, Map.fromList files, Map.empty, Map.fromList store)
  in evalState
      (runEnvironment
        (runExcept f))
  (state, (env, x))

evalConcrete :: [(String, Addr)] -> [(Addr, Val)] -> Expr -> Error Val Val
evalConcrete = runInterp eval []

runStatementsConcrete :: [(String, Addr)] -> [(Addr, Val)] -> [Statement] -> Error Val (Maybe Val)
runStatementsConcrete env store stmts = runInterp runStatements [] env store (stmts, 0)

runProgramConcrete :: [(String, File)] -> File -> [Immediate] -> Error Val (Maybe Val)
runProgramConcrete files mainFile args =
  runInterp runProgram files [] [] (mainFile, args)

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

newArray :: (ArrowFail Val c, ArrowChoice c, ArrowState ProgramState c) => c (Type, [Int]) Val
newArray =
  let createVals :: (ArrowFail Val c, ArrowChoice c, ArrowState ProgramState c) => c (Int, Type, [Int]) [Val]
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

evalImmediateList :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr env c, ArrowState ProgramState c) => c [Immediate] [Val]
evalImmediateList = proc xs -> case xs of
  (x':xs') -> do
    v <- evalImmediate -< x'
    vs <- evalImmediateList -< xs'
    returnA -< (v:vs)
  [] -> returnA -< []

evalImmediate :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr env c, ArrowState ProgramState c) => c Immediate Val
evalImmediate = proc i -> case i of
  ILocalName localName -> fetchLocal -< localName
  IInt n -> returnA -< (VInt n)
  IFloat f -> returnA -< (VFloat f)
  IString s -> returnA -< (VString s)
  IClass c -> returnA -< (VClass c)
  INull -> returnA -< VNull

evalIndex :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr env c, ArrowState ProgramState c) => c Immediate Int
evalIndex = proc i -> do
  v <- evalImmediate -< i
  case v of
    VInt n -> returnA -< n
    _ -> failA -< VString "Expected an integer array index"

evalRef :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr PointerEnv c, ArrowState ProgramState c) => c Reference Val
evalRef = proc ref -> case ref of
  ArrayReference localName i -> do
    n <- evalIndex -< i
    (_, VArray xs) <- fetchArrayWithAddr -< localName
    if n >= 0 && n < length xs
    then returnA -< xs !! n
    else failA -< VString "ArrayIndexOutOfBoundsException"
  FieldReference localName fieldSignature -> do
    (_, VObject m) <- fetchObjectWithAddr -< localName
    case Map.lookup fieldSignature m of
      Just x -> returnA -< x
      Nothing -> failA -< VString $ printf "Field %s not defined for object %s" (show fieldSignature) (show localName)
  SignatureReference fieldSignature -> fetchField -< fieldSignature

evalMethod :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr PointerEnv c, ArrowState ProgramState c) => c (Method, Maybe Val, [Immediate]) (Maybe Val)
evalMethod = proc (method, this, args) -> do
  (a, m, s, files, fields, vars) <- getA -< ()
  argVals <- evalImmediateList -< args
  putA -< (a, Just method, s, files, fields, vars)
  env <- createMethodEnv -< (this, parameters method, argVals)
  v <- localEnv runMethodBody -< (env, methodBody method)
  putA -< (a, m, s, files, fields, vars)
  returnA -< v

evalInvoke :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr PointerEnv c, ArrowState ProgramState c) => c InvokeExpr (Maybe Val)
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

eval :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr PointerEnv c, ArrowState ProgramState c) => c Expr Val
eval = proc e -> case e of
  ENew newExpr -> case newExpr of
    NewSimple t -> if isBaseType t
      then case t of
        TClass c -> do
          file <- fetchFile -< c
          let fields = foldl (\acc member -> case member of
                                FieldMember f -> (FieldSignature c (fieldType f) (fieldName f), defaultValue (fieldType f)):acc
                                _ -> acc) [] (fileBody file)
          addr <- alloc -< (VObject (Map.fromList fields))
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
    v <- evalInvoke -< invokeExpr
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
          v' <- getLocal -< addr
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

lookupEnv :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr env c) => c String Addr
lookupEnv = proc x -> do
  maybeAddr <- lookup -< x
  case maybeAddr of
    Just addr -> returnA -< addr
    Nothing -> failA -< VString $ printf "Variable %s not bounded" (show x)

fetchLocal :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr env c, ArrowState ProgramState c) => c String Val
fetchLocal = proc x -> do
  addr <- lookupEnv -< x
  getLocal -< addr

getLocal :: (ArrowChoice c, ArrowFail Val c, ArrowState ProgramState c) => c Addr Val
getLocal = proc addr -> do
  (_, _, _, _, _, variableStore) <- getA -< ()
  let val = Map.lookup addr variableStore
  case val of
    Just v -> returnA -< v
    Nothing -> failA -< VString $ printf "Variable %s not bounded" (show addr)

fetchField :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr env c, ArrowState ProgramState c) => c FieldSignature Val
fetchField = proc x -> do
  (_, _, _, _, fieldStore, variableStore) <- getA -< ()
  let addr = Map.lookup x fieldStore
  case addr of
    Just a -> do
      let val = Map.lookup a variableStore
      case val of
        Just v -> returnA -< v
        Nothing -> failA -< VString $ printf "Field %s not bound" (show x)
    Nothing -> failA -< VString $ printf "Field %s not bound" (show x)

fetchFile :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr env c, ArrowState ProgramState c) => c String File
fetchFile = proc n -> do
  (_, _, _, fileStore, _, _) <- getA -< ()
  let file = Map.lookup n fileStore
  case file of
    Just x -> returnA -< x
    Nothing -> failA -< VString $ printf "File %s not loaded" (show n)

alloc :: (ArrowState ProgramState c) => c Val Addr
alloc = proc val -> do
  (addr, m, s, files, fields, vars) <- getA -< ()
  let vars' = Map.insert addr val vars
  putA -< (succ addr, m, s, files, fields, vars')
  returnA -< addr

writeVar :: (ArrowState ProgramState c) => c (Addr, Val) ()
writeVar = proc (addr, val) -> do
  (a, m, s, files, fields, vars) <- getA -< ()
  let vars' = Map.insert addr val vars
  putA -< (a, m, s, files, fields, vars')

matchesSignature :: Type -> String -> [Type] -> Member -> Bool
matchesSignature retType n argTypes (MethodMember m) =
  methodName m == n && returnType m == retType && parameters m == argTypes
matchesSignature _ _ _ _ = False

fetchMethod :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr PointerEnv c, ArrowState ProgramState c) => c MethodSignature Method
fetchMethod = proc (MethodSignature c retType n argTypes) -> do
  (_, _, _, fileStore, _, _) <- getA -< ()
  let file = Map.lookup c fileStore
  case file of
    Just v -> case find (matchesSignature retType n argTypes) (fileBody v) of
      Just (MethodMember m) -> returnA -< m
      _ -> failA -< VString $ printf "Method %s not defined for class %s" (show n) (show c)
    Nothing -> failA -< VString $ printf "Undefined class %s" (show c)

fetchRefValWithAddr :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr PointerEnv c, ArrowState ProgramState c) => c String (Addr, Val)
fetchRefValWithAddr = proc localName -> do
  v <- fetchLocal -< localName
  case v of
    VRef addr -> do
      v' <- getLocal -< addr
      returnA -< (addr, v')
    _ -> failA -< VString $ printf "Variable %s is not a reference" (show localName)

fetchArrayWithAddr :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr PointerEnv c, ArrowState ProgramState c) => c String (Addr, Val)
fetchArrayWithAddr = proc localName -> do
  (addr, v) <- fetchRefValWithAddr -< localName
  case v of
    VArray _ -> returnA -< (addr, v)
    _ -> failA -< VString $ printf "Variable %s not bound to an array" (show localName)

fetchObjectWithAddr :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr PointerEnv c, ArrowState ProgramState c) => c String (Addr, Val)
fetchObjectWithAddr = proc localName -> do
  (addr, v) <- fetchRefValWithAddr -< localName
  case v of
    VObject _ -> returnA -< (addr, v)
    _ -> failA -< VString $ printf "Variable %s not bound to an object" (show localName)

goto :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr PointerEnv c, ArrowState ProgramState c) => c ([Statement], String) (Maybe Val)
goto = proc (stmts, label) -> case Label label `elemIndex` stmts of
  Just i -> runStatements -< (stmts, i)
  Nothing -> failA -< VString $ printf "Undefined label: %s" label

matchCases :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr PointerEnv c, ArrowState ProgramState c) => c ([CaseStatement], Int) String
matchCases = proc (cases, v) -> case cases of
  ((CLConstant n, label): cases') -> if v == n
    then returnA -< label
    else matchCases -< (cases', v)
  ((CLDefault, label): _) -> returnA -< label
  [] -> failA -< VString $ printf "No cases match value %s" (show v)

createParamEnv :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr PointerEnv c, ArrowState ProgramState c) => c (Int, [Val]) PointerEnv
createParamEnv = proc (i, params) -> case params of
  (param:rest) -> do
    let toParam :: Int -> String
        toParam n = "@parameter" ++ show n

    env <- createParamEnv -< (i + 1, rest)
    addr <- alloc -< param
    extendEnv -< (toParam i, addr, env)
  [] -> returnA -< E.empty

createMethodEnv :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr PointerEnv c, ArrowState ProgramState c) => c (Maybe Val, [Type], [Val]) PointerEnv
createMethodEnv = proc (this, _, params) -> do
  -- let typedParamVals = zip paramTypes params
  paramEnv <- createParamEnv -< (0, params)
  case this of
    Just val -> do
      addr <- alloc -< val
      extendEnv -< ("@this", addr, paramEnv)
    Nothing -> returnA -< paramEnv

runStatements :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr PointerEnv c, ArrowState ProgramState c) => c ([Statement], Int) (Maybe Val)
runStatements = proc (stmts, i) -> if i == length stmts
  then returnA -< Nothing
  else case stmts !! i of
    -- Label labelName -> if
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
      addr <- lookupEnv -< (show atId)
      env <- getEnv -< ()
      env' <- extendEnv -< (localName, addr, env)
      localEnv runStatements -< (env', (stmts, i + 1))
    IdentityNoType localName atId -> do
      addr <- lookupEnv -< (show atId)
      env <- getEnv -< ()
      env' <- extendEnv -< (localName, addr, env)
      localEnv runStatements -< (env', (stmts, i + 1))
    Assign var e -> do
      v <- eval -< e
      case var of
        VLocal localName -> do
          addr <- lookupEnv -< localName
          writeVar -< (addr, v)
          runStatements -< (stmts, i + 1)
        VReference ref -> case ref of
          ArrayReference localName index -> do
            n <- evalIndex -< index
            (addr, VArray xs) <- fetchArrayWithAddr -< localName
            if n >= 0 && n < length xs
            then do
              let xs' = replace n v xs
              writeVar -< (addr, VArray xs')
              runStatements -< (stmts, i + 1)
            else failA -< VString "ArrayIndexOutOfBoundsException"
          FieldReference localName fieldSignature -> do
            (addr, VObject m) <- fetchObjectWithAddr -< localName
            case m Map.!? fieldSignature of
              Just _ -> do
                let m' = Map.insert fieldSignature v m
                writeVar -< (addr, VObject m')
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

runDeclaration :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr PointerEnv c, ArrowState ProgramState c) => c (Env String Addr, Declaration) PointerEnv
runDeclaration = proc (env, dec) -> case dec of
  (t, (d:rest)) -> do
    env' <- runDeclaration -< (env, (t, rest))
    addr <- alloc -< defaultValue t
    env'' <- extendEnv -< (d, addr, env')
    returnA -< env''
  (_, []) -> returnA -< env

runDeclarations :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr PointerEnv c, ArrowState ProgramState c) => c (Env String Addr, [Declaration]) PointerEnv
runDeclarations = proc (env, decs) -> case decs of
  (dec:rest) -> do
    env' <- runDeclarations -< (env, rest)
    env'' <- runDeclaration -< (env', dec)
    returnA -< env''
  [] -> returnA -< env

runMethodBody :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr PointerEnv c, ArrowState ProgramState c) => c MethodBody (Maybe Val)
runMethodBody = proc body -> case body of
  MEmpty -> returnA -< Nothing
  MFull{declarations=d,statements=s,catchClauses=c} -> do
    env <- getEnv -< ()
    env' <- runDeclarations -< (env, d)
    v <- localEnv runStatements -< (env', (s, 0))
    returnA -< v

unboxErrorRef :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr PointerEnv c, ArrowState ProgramState c) => c Val (Maybe Val)
unboxErrorRef = proc val -> case val of
  VRef addr -> do
    v <- getLocal -< addr
    failA -< v
  x -> failA -< x

unboxResultRef :: (ArrowChoice c, ArrowFail Val c, ArrowEnv String Addr PointerEnv c, ArrowState ProgramState c) => c (Maybe Val) (Maybe Val)
unboxResultRef = proc val -> case val of
  Just (VRef addr) -> do
    v <- getLocal -< addr
    returnA -< Just v
  x -> returnA -< x

runProgram :: (ArrowChoice c,
               ArrowFail Val c,
               ArrowEnv String Addr PointerEnv c,
               ArrowState ProgramState c,
               ArrowError Val (Method, Maybe Val, [Immediate]) (Maybe Val) c) => c (File, [Immediate]) (Maybe Val)
runProgram = proc (mainFile, args) -> do
  let findMethodByName :: [Member] -> String -> Maybe Method
      findMethodByName (MethodMember m:rest) name =
        if methodName m == name
        then Just m
        else findMethodByName rest name
      findMethodByName (_:rest) name = findMethodByName rest name
      findMethodByName [] _ = Nothing
  case findMethodByName (fileBody mainFile) "<clinit>" of
    Just classInitMethod -> do
      evalMethod -< (classInitMethod, Nothing, [])
      case findMethodByName (fileBody mainFile) "main" of
        Just mainMethod -> evalMethod -< (mainMethod, Nothing, args)
        Nothing -> returnA -< Nothing
    Nothing -> do
      case findMethodByName (fileBody mainFile) "main" of
        Just mainMethod -> tryWithErrorA evalMethod unboxResultRef unboxErrorRef -< (mainMethod, Nothing, args)
        Nothing -> returnA -< Nothing
