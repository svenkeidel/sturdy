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
module Interval where

import           Prelude hiding (lookup,read)

import           Data.Fixed
import           Data.List (elemIndex,find,replicate,repeat)

import           Data.Concrete.Error

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Concrete.Environment (Env)
import qualified Data.Concrete.Environment as E
import qualified Data.Concrete.Store as S
import qualified Data.Abstract.Boolean as B
import           Data.Order

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
import           Control.Arrow.Utils
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
  = BottomVal
  | TopVal
  | IntVal Int
  | LongVal Int
  | FloatVal Float
  | DoubleVal Float
  | StringVal String
  | ClassVal String
  | BoolVal B.Bool
  | NullVal
  | RefVal Addr
  | ArrayVal [Val]
  | ObjectVal String (Map FieldSignature Val) deriving (Eq)

instance Show Val where
  show BottomVal = "⊥"
  show TopVal = "⊤"
  show (IntVal n) = show n
  show (LongVal l) = show l
  show (FloatVal f) = show f
  show (DoubleVal d) = show d
  show (StringVal s) = s
  show (ClassVal c) = "<" ++ c ++ ">"
  show (BoolVal b) = show b
  show NullVal = "null"
  show (RefVal a) = "@" ++ show a
  show (ArrayVal xs) = show xs
  show (ObjectVal c m) = show c ++ "{" ++ show m ++ "}"

defaultValue :: Type -> Val
defaultValue BooleanType = BoolVal B.False
defaultValue ByteType = IntVal 0
defaultValue CharType = IntVal 0
defaultValue ShortType = IntVal 0
defaultValue IntType = IntVal 0
defaultValue LongType = LongVal 0
defaultValue FloatType = FloatVal 0.0
defaultValue DoubleType = DoubleVal 0.0
defaultValue NullType = NullVal
defaultValue (RefType _) = NullVal
defaultValue (ArrayType _) = NullVal
defaultValue _ = BottomVal

bool :: Bool -> B.Bool
bool True = B.True
bool False = B.False

boolVal :: Bool -> Val
boolVal True = BoolVal B.True
boolVal False = BoolVal B.False

instance PreOrd Val where
  _ ⊑ TopVal = True
  IntVal n1 ⊑ IntVal n2 = n1 == n2
  LongVal l1 ⊑ LongVal l2 = l1 == l2
  FloatVal f1 ⊑ FloatVal f2 = f1 == f2
  DoubleVal d1 ⊑ DoubleVal d2 = d1 == d2
  StringVal s1 ⊑ StringVal s2 = s1 == s2
  ClassVal c1 ⊑ ClassVal c2 = c1 == c2
  BoolVal b1 ⊑ BoolVal b2 = b1 ⊑ b2
  NullVal ⊑ NullVal = True
  ArrayVal xs ⊑ ArrayVal ys = all (\(x,y) -> x ⊑ y) (zip xs ys)
  ObjectVal c1 m1 ⊑ ObjectVal c2 m2 =
    c1 == c2 && all (\(x,y) -> x ⊑ y) (zip (Map.elems m1) (Map.elems m2))
  _ ⊑ _ = False

instance Complete Val where
  IntVal n1 ⊔ IntVal n2 = IntVal $ n1 ⊔ n2
  LongVal l1 ⊔ LongVal l2 = LongVal $ l1 ⊔ l2
  IntVal n ⊔ LongVal l = LongVal $ n ⊔ l
  LongVal l ⊔ IntVal n = LongVal $ n ⊔ l
  FloatVal f1 ⊔ FloatVal f2 = if f1 == f2 then FloatVal f1 else TopVal
  DoubleVal d1 ⊔ DoubleVal d2 = if d1 == d2 then DoubleVal d1 else TopVal
  StringVal s1 ⊔ StringVal s2 = if s1 == s2 then StringVal s else TopVal
  ClassVal c1 ⊔ ClassVal c2 = if c1 == c2 then ClassVal c1 else ClassVal "java.lang.Object"
  BoolVal b1 ⊔ BoolVal b2 = BoolVal $ b1 ⊔ b2
  NullVal ⊔ NullVal = NullVal
  ArrayVal xs ⊔ ArrayVal ys = ArrayVal $ xs ++ ys
  ObjectVal c1 m1 ⊔ ObjectVal c2 m2 =
    if c1 == c2
      then ObjectVal $ fromList zip (Map.keys m1) ((\(x,y) -> x ⊔ y) (zip (Map.elems m1) (Map.elems m2)))
      else TopVal
  _ ⊔ _ = TopVal

instance UpperBounded Val where
  top = TopVal

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
      latestAddr = case map snd env ++ map fst store of
        [] -> 0
        addrs -> maximum addrs
      fields = zip (concatMap (\u -> getFieldSignatures u (\m -> Static `elem` m)) files) [latestAddr..]
      store' = store ++ map (\(FieldSignature _ t _,a) -> (a,defaultValue t)) fields
  in runConst (Map.fromList compilationUnits, Map.fromList fields)
      (evalState
        (evalMaybeStore
          (runMaybeEnvironment
            (runReader
              (runExcept f)))))
  (latestAddr + length fields, (S.fromList store', (env, ((mainMethod, 0, []), x))))

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
                    CanCatch EInvoke c,
                    CanCatch ([Statement], Int) c,
                    CanCatch (Method, Maybe Val, [Expr]) c,
                    ArrowDebug c)

---- End of Constraint Boilerplate ----

---- Boilerplate Methods ----

getFieldSignatures :: CompilationUnit -> ([Modifier] -> Bool) -> [FieldSignature]
getFieldSignatures unit p =
  let toFieldSignature :: Member -> [FieldSignature]
      toFieldSignature (FieldMember f) =
        [FieldSignature (fileName unit) (fieldType f) (fieldName f) | p (fieldModifiers f)]
      toFieldSignature _ = []
  in concatMap toFieldSignature (fileBody unit)

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace 0 x (_:t) = x:t
replace n x (h:t) = h:replace (n-1) x t

numToNum :: (forall a. Num a => a -> a -> a) -> Val -> Val -> Maybe Val
numToNum op v1 v2 = case (v1, v2) of
  (IntVal n1, IntVal n2) -> Just (IntVal (op n1 n2))
  (FloatVal f, IntVal n) -> Just (FloatVal (op f (fromIntegral n)))
  (IntVal n, FloatVal f) -> Just (FloatVal (op (fromIntegral n) f))
  (FloatVal f1, FloatVal f2) -> Just (FloatVal (op f1 f2))
  (_, _) -> Nothing

numToBool :: (forall a. Ord a => a -> a -> Bool) -> Val -> Val -> Maybe Val
numToBool op v1 v2 = case (v1, v2) of
  (IntVal n1, IntVal n2) -> Just (boolVal (op n1 n2))
  (FloatVal f, IntVal n) -> Just (boolVal (op f (fromIntegral n)))
  (IntVal n, FloatVal f) -> Just (boolVal (op (fromIntegral n) f))
  (FloatVal f1, FloatVal f2) -> Just (boolVal (op f1 f2))
  (_, _) -> Nothing

lookup' :: (Show var, CanFail c, ArrowMaybeEnv var val env c) => c var val
lookup' = proc x -> do
  v <- lookup -< x
  case v of
    Just y -> returnA -< y
    Nothing -> failA -< StringVal $ printf "Variable %s not bounded" (show x)

read' :: (Show var, CanFail c, ArrowMaybeStore var val c) => c var val
read' = proc x -> do
  v <- read -< x
  case v of
    Just y -> returnA -< y
    Nothing -> failA -< StringVal $ printf "Address %s not bounded" (show x)

assert :: (CanFail c) => c Bool ()
assert = proc prop ->
    if prop
    then returnA -< ()
    else failA -< StringVal "Assertion failed"

toIntList :: (CanFail c) => c [Val] [Int]
toIntList = proc vs -> case vs of
  (IntVal x:xs) -> do
    xs' <- toIntList -< xs
    returnA -< (x:xs')
  (_:_) -> failA -< StringVal "Expected an integer valued array for toIntList"
  [] -> returnA -< []

unbox :: (CanFail c, CanStore c) => c Val Val
unbox = proc val -> case val of
  RefVal addr -> do
    v <- read' -< addr
    case v of
      ObjectVal c m -> do
        let (keys, vals) = unzip (Map.toList m)
        vals' <- mapA unbox -< vals
        returnA -< ObjectVal c (Map.fromList (zip keys vals'))
      ArrayVal xs -> do
        xs' <- mapA unbox -< xs
        returnA -< ArrayVal xs'
      _ -> returnA -< v
  _ -> returnA -< val

unboxMaybe :: (CanFail c, CanUseMem c) => c (Maybe Val) (Maybe Val)
unboxMaybe = proc val -> case val of
  Just x -> do
    x' <- unbox -< x
    returnA -< Just x'
  Nothing -> returnA -< Nothing

fetchLocal :: (CanFail c, CanUseMem c) => c String Val
fetchLocal = proc x -> do
  addr <- lookup' -< x
  read' -< addr

lookupField :: (CanFail c, CanUseConst c) => c FieldSignature Addr
lookupField = proc x -> do
  (_,fields) <- askConst -< ()
  case Map.lookup x fields of
    Just addr -> returnA -< addr
    Nothing -> failA -< StringVal $ printf "Field %s not bound" (show x)

fetchFieldWithAddr :: (CanFail c, CanStore c, CanUseConst c) => c FieldSignature (Addr, Val)
fetchFieldWithAddr = proc x -> do
  addr <- lookupField -< x
  val <- read' -< addr
  returnA -< (addr, val)

fetchCompilationUnit :: (CanFail c, CanUseConst c) => c String CompilationUnit
fetchCompilationUnit = proc n -> do
  (compilationUnits, _) <- askConst -< ()
  let compilationUnit = Map.lookup n compilationUnits
  case compilationUnit of
    Just x -> returnA -< x
    Nothing -> failA -< StringVal $ printf "CompilationUnit %s not loaded" (show n)

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
      _ -> failA -< StringVal $ printf "Method %s not defined for class %s" (show n) (show c)
    Nothing -> failA -< StringVal $ printf "Undefined class %s" (show c)

fetchRefValWithAddr :: (CanFail c, CanUseMem c) => c String (Addr, Val)
fetchRefValWithAddr = proc localName -> do
  v <- fetchLocal -< localName
  case v of
    RefVal addr -> do
      v' <- read' -< addr
      returnA -< (addr, v')
    _ -> failA -< StringVal $ printf "Variable %s is not a reference" (show localName)

fetchArrayWithAddr :: (CanFail c, CanUseMem c) => c String (Addr, Val)
fetchArrayWithAddr = proc localName -> do
  (addr, v) <- fetchRefValWithAddr -< localName
  case v of
    ArrayVal _ -> returnA -< (addr, v)
    _ -> failA -< StringVal $ printf "Variable %s not bound to an array" (show localName)

fetchObjectWithAddr :: (CanFail c, CanUseMem c) => c String (Addr, Val)
fetchObjectWithAddr = proc localName -> do
  (addr, v) <- fetchRefValWithAddr -< localName
  case v of
    ObjectVal _ _ -> returnA -< (addr, v)
    _ -> failA -< StringVal $ printf "Variable %s not bound to an object" (show localName)

getCurrentMethodBody :: (CanFail c, CanUseReader c) => c () MethodBody
getCurrentMethodBody = proc () -> do
  (m, _, _) <- askA -< ()
  case m of
    Just m' ->
      case methodBody m' of
        EmptyBody -> failA -< StringVal $ printf "Empty body for method %s" (show m')
        FullBody{} -> returnA -< methodBody m'
    Nothing -> failA -< StringVal "No method running"

throw :: CanInterp c => c (String, String) Val
throw = proc (clzz, message) -> do
  (RefVal addr) <- newSimple -< clzz
  v <- read' -< addr
  case v of
    ObjectVal c m -> do
      let m' = Map.insert (FieldSignature clzz (RefType "String") "message") (StringVal message) m
      write -< (addr, ObjectVal c m')
      failA -< RefVal addr
    _ -> failA -< StringVal $ printf "Undefined Exception %s" clzz

---- End of Boilerplate Methods ----

---- Evaluation Helper Methods ----

getInitializedFields :: (CanFail c, CanUseConst c) => c String [(FieldSignature, Val)]
getInitializedFields = proc c -> do
  unit <- fetchCompilationUnit -< c
  let fieldSignatures = getFieldSignatures unit (\m -> Static `notElem` m)
  let ownFields = map (\s@(FieldSignature _ t' _) -> (s, defaultValue t')) fieldSignatures
  case extends unit of
    Just p -> do
      parentFields <- getInitializedFields -< p
      returnA -< parentFields ++ ownFields
    Nothing -> returnA -< ownFields

newSimple :: (CanFail c, CanUseConst c, CanUseState c, CanStore c) => c String Val
newSimple = proc c -> do
  fields <- getInitializedFields -< c
  addr <- alloc -< (ObjectVal c (Map.fromList fields))
  returnA -< RefVal addr

newArray :: (CanFail c, CanUseState c, CanStore c) => c (Type, [Int]) Val
newArray = proc (t, sizes) -> case sizes of
  (s:sizes') -> do
    vals <- mapA newArray -< replicate s (t, sizes')
    addr <- alloc -< ArrayVal vals
    returnA -< RefVal addr
  [] -> returnA -< defaultValue t

isSuperClass :: (CanFail c, CanUseConst c) => c (String, String) B.Bool
isSuperClass = proc (c, p) -> if c == p
  then returnA -< B.True
  else do
    unit <- fetchCompilationUnit -< c
    case extends unit of
      Just c' -> isSuperClass -< (c', p)
      Nothing -> returnA -< B.False

isInstanceof :: (CanFail c, CanUseConst c, CanStore c) => c (Val, Type) B.Bool
isInstanceof = proc (v, t) -> do
  assert -< (isNonvoidType t)
  v' <- unbox -< v
  case (v', t) of
    (BoolVal _,     BooleanType)  -> returnA -< B.True
    (IntVal n,      ByteType)     -> returnA -< if n >= -128                 && n < 128                  then B.True else B.False
    (IntVal n,      CharType)     -> returnA -< if n >= 0                    && n < 65536                then B.True else B.False
    (IntVal n,      ShortType)    -> returnA -< if n >= -32768               && n < 32768                then B.True else B.False
    (IntVal n,      IntType)      -> returnA -< if n >= -2147483648          && n < 2147483648           then B.True else B.False
    (LongVal l,     LongType)     -> returnA -< if l >= -9223372036854775808 && l <= 9223372036854775807 then B.True else B.False
    (FloatVal _,    FloatType)    -> returnA -< B.True
    (DoubleVal _,   DoubleType)   -> returnA -< B.True
    (NullVal,       NullType)     -> returnA -< B.True
    (ObjectVal c _, RefType p)    -> isSuperClass -< (c, p)
    (ArrayVal xs,   ArrayType t') -> (mapA isInstanceof >>^ bool . all (==B.True)) -< zip xs (repeat t')
    (_, _) -> returnA -< B.False

evalIndex :: (CanInterp c) => c Expr Int
evalIndex = proc i -> do
  v <- eval -< i
  case v of
    IntVal n -> returnA -< n
    LongVal l -> returnA -< l
    _ -> failA -< StringVal "Expected an integer array index"

evalMethod :: CanInterp c => c (Method, Maybe Val, [Expr]) (Maybe Val)
evalMethod = proc (method, this, args) -> do
  argVals <- mapA eval -< args
  env <- createMethodEnv -< (this, parameters method, argVals)
  localEnv (proc method -> localA runMethodBody -< ((Just method, 0, []), methodBody method)) -< (env, method)

evalInvoke :: CanInterp c => c EInvoke (Maybe Val)
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
  _ -> failA -< StringVal "Not implemented"

goto :: CanInterp c => c ([Statement], String) (Maybe Val)
goto = proc (stmts, label) -> case Label label `elemIndex` stmts of
  Just i -> runStatements -< (stmts, i)
  Nothing -> failA -< StringVal $ printf "Undefined label: %s" label

matchCases :: (CanFail c) => c ([CaseStatement], Int) String
matchCases = proc (cases, v) -> case cases of
  ((ConstantCase n, label): cases') -> if v == n
    then returnA -< label
    else matchCases -< (cases', v)
  ((DefaultCase, label): _) -> returnA -< label
  [] -> failA -< StringVal $ printf "No cases match value %s" (show v)

createParamEnv :: (CanFail c, CanUseMem c, CanUseState c) => c (Int, [Val]) PointerEnv
createParamEnv = proc (i, params) -> case params of
  (param:rest) -> do
    env <- createParamEnv -< (i + 1, rest)
    addr <- alloc -< param
    extendEnv -< ("@parameter" ++ show i, addr, env)
  [] -> returnA -< E.empty

createMethodEnv :: (CanFail c, CanUseMem c, CanUseState c) => c (Maybe Val, [Type], [Val]) PointerEnv
createMethodEnv = proc (this, _, params) -> do
  paramEnv <- createParamEnv -< (0, params)
  case this of
    Just val -> do
      addr <- alloc -< val
      extendEnv -< ("@this", addr, paramEnv)
    Nothing -> returnA -< paramEnv

getObject :: (CanFail c, CanStore c) => c Val Val
getObject = proc val -> case val of
  RefVal addr -> do
    o <- read' -< addr
    case o of
      ObjectVal _ _ -> returnA -< o
      _ -> failA -< val
  _ -> failA -< val

getMatchingClause :: CanFail c => c (String, [CatchClause], Val) CatchClause
getMatchingClause = proc (c, cs, val) -> case find (\clause -> className clause == c) cs of
  Just clause -> returnA -< clause
  Nothing -> failA -< val

getMethodBody :: CanFail c => c (Maybe Method, Val) MethodBody
getMethodBody = proc (m, val) -> case m of
  Just m' ->
    case methodBody m' of
      EmptyBody -> failA -< val
      FullBody{} -> returnA -< methodBody m'
  Nothing -> failA -< val

catchExceptions :: CanInterp c => c Val (Maybe Val)
catchExceptions = proc val -> do
  (ObjectVal c _) <- getObject -< val
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

---- End of Evaluation Helper Methods ----

---- Actual Evaluation methods ----

eval :: CanInterp c => c Expr Val
eval = proc e -> case e of
  NewExpr t -> if isBaseType t
    then case t of
      RefType c -> newSimple -< c
      _ -> returnA -< (defaultValue t)
    else failA -< StringVal "Expected a nonvoid base type for new"
  NewArrayExpr t i -> if isNonvoidType t
    then do
      v <- eval -< i
      ns <- toIntList -< [v]
      if all (>0) ns
        then newArray -< (t, ns)
        else failA -< StringVal "Expected a positive integer for newarray size"
    else failA -< StringVal "Expected a nonvoid type for newarray"
  NewMultiArrayExpr t is -> if isBaseType t
    then do
      vs <- mapA eval -< is
      ns <- toIntList -< vs
      if all (>0) ns
        then newArray -< (t, ns)
        else failA -< StringVal "Expected positive integers for newmultiarray sizes"
    else failA -< StringVal "Expected a nonvoid base type for newmultiarray"
  CastExpr t i -> do
    v <- eval -< i
    b <- isInstanceof -< (v, t)
    if b == B.True || b == B.Top
    then do
      v' <- unbox -< v
      case v' of
        ObjectVal _ _ -> returnA -< v
        _ -> failA -< StringVal "Casting of primivites and arrays is not yet supported"
        -- https://docs.oracle.com/javase/specs/jls/se7/html/jls-5.html#jls-5.1.2
    else throw -< ("java.lang.ClassCastException", printf "Cannot cast %s to type %s" (show v) (show t))
  InstanceOfExpr i t -> do
    v <- eval -< i
    b <- isInstanceof -< (v, t)
    returnA -< BoolVal b
  InvokeExpr invokeExpr -> do
    v <- tryCatchA evalInvoke returnA failA -< invokeExpr
    case v of
      Just v' -> returnA -< v'
      Nothing -> failA -< StringVal "Method returned nothing"
  ArrayRef localName i -> do
    n <- evalIndex -< i
    (_, ArrayVal xs) <- fetchArrayWithAddr -< localName
    if n >= 0 && n < length xs
    then returnA -< xs !! n
    else throw -< ("java.lang.ArrayIndexOutOfBoundsException", printf "Index %d out of bounds" (show n))
  FieldRef localName fieldSignature -> do
    (_, ObjectVal _ m) <- fetchObjectWithAddr -< localName
    case Map.lookup fieldSignature m of
      Just x -> returnA -< x
      Nothing -> failA -< StringVal $ printf "Field %s not defined for object %s" (show fieldSignature) (show localName)
  SignatureRef fieldSignature -> do
    (_, val) <- fetchFieldWithAddr -< fieldSignature
    returnA -< val
  BinopExpr i1 op i2 -> do
    v1 <- eval -< i1
    v2 <- eval -< i2
    case op of
      -- And ->
      -- Or ->
      -- Xor ->
      Mod -> case (v1, v2) of
        (IntVal x1, IntVal x2) -> returnA -< (IntVal (x1 `mod` x2))
        (IntVal x1, FloatVal x2) -> returnA -< (FloatVal (fromIntegral x1 `mod'` x2))
        (FloatVal x1, IntVal x2) -> returnA -< (FloatVal (x1 `mod'` fromIntegral x2))
        (FloatVal x1, FloatVal x2) -> returnA -< (FloatVal (x1 `mod'` x2))
        (_, _) -> failA -< StringVal "Expected two numbers as arguments for mod"
      -- Rem ->
      -- Cmp ->
      -- Cmpg ->
      -- Cmpl ->
      Cmpeq -> returnA -< (BoolVal $ if (v1 == v2) then B.True else B.False)
      Cmpne -> returnA -< (BoolVal $ if (v1 /= v2) then B.True else B.False)
      Cmpgt -> case numToBool (>) v1 v2 of
        Just v -> returnA -< v
        Nothing -> failA -< StringVal "Expected two numbers as arguments for >"
      Cmpge -> case numToBool (>=) v1 v2 of
        Just v -> returnA -< v
        Nothing -> failA -< StringVal "Expected two numbers as arguments for >="
      Cmplt -> case numToBool (<) v1 v2 of
        Just v -> returnA -< v
        Nothing -> failA -< StringVal "Expected two numbers as arguments for <"
      Cmple -> case numToBool (<=) v1 v2 of
        Just v -> returnA -< v
        Nothing -> failA -< StringVal "Expected two numbers as arguments for <="
      -- Shl ->
      -- Shr ->
      -- Ushr ->
      Plus -> case numToNum (+) v1 v2 of
        Just v -> returnA -< v
        Nothing -> failA -< StringVal "Expected two numbers as arguments for +"
      Minus -> case numToNum (-) v1 v2 of
        Just v -> returnA -< v
        Nothing -> failA -< StringVal "Expected two numbers as arguments for -"
      Mult -> case numToNum (*) v1 v2 of
        Just v -> returnA -< v
        Nothing -> failA -< StringVal "Expected two numbers as arguments for *"
      Div -> case (v1, v2) of
        (_, IntVal 0) -> failA -< StringVal "Cannot divide by zero"
        (_, FloatVal 0.0) -> failA -< StringVal "Cannot divide by zero"
        (IntVal n1, IntVal n2) -> returnA -< (IntVal (n1 `div` n2))
        (FloatVal f, IntVal n) -> returnA -< (FloatVal (f / fromIntegral n))
        (IntVal n, FloatVal f) -> returnA -< (FloatVal (fromIntegral n / f))
        (FloatVal f1, FloatVal f2) -> returnA -< (FloatVal (f1 / f2))
        (_, _) -> failA -< StringVal "Expected two numbers as arguments for /"
  UnopExpr op i -> do
    v <- eval -< i
    case op of
      Lengthof -> case v of
        RefVal addr -> do
          v' <- read' -< addr
          case v' of
            ArrayVal xs -> returnA -< (IntVal (length xs))
            _ -> failA -< StringVal "Expected an array as argument for lengthof"
        _ -> failA -< StringVal "Expected an array as argument for lengthof"
      Neg -> case v of
        IntVal n -> returnA -< (IntVal (-n))
        FloatVal f -> returnA -< (FloatVal (-f))
        _ -> failA -< StringVal "Expected a number as argument for -"
  ThisRef -> eval -< (Local "@this")
  ParameterRef n -> eval -< (Local ("@parameter" ++ show n))
  CaughtExceptionRef -> eval -< (Local "@caughtexception")
  Local localName -> fetchLocal -< localName
  DoubleConstant f -> returnA -< (DoubleVal f)
  FloatConstant f -> returnA -< (FloatVal f)
  IntConstant n -> returnA -< (IntVal n)
  LongConstant f -> returnA -< (LongVal f)
  NullConstant -> returnA -< NullVal
  StringConstant s -> returnA -< (StringVal s)
  ClassConstant c -> returnA -< (ClassVal c)
  MethodHandle _ -> failA -< StringVal "Evaluation of method handles is not implemented"

runStatements :: CanInterp c => c ([Statement], Int) (Maybe Val)
runStatements = proc (stmts, i) -> if i >= length stmts
  then returnA -< Nothing
  else case stmts !! i of
    Label labelName -> do
      body <- getCurrentMethodBody -< ()
      (m, a, _) <- askA -< ()
      let cs = filter (\c -> labelName == fromLabel c) (catchClauses body)
      localA (tryCatchA runStatements returnA catchExceptions) -< ((m, a, cs), (stmts, i + 1))
    -- Breakpoint
    -- Entermonitor Expr
    -- Exitmonitor Expr
    Tableswitch immediate cases -> do
      v <- eval -< immediate
      case v of
        IntVal x -> do
          label <- matchCases -< (cases, x)
          goto -< (stmts, label)
        _ -> failA -< StringVal "Expected an integer as argument for switch"
    Lookupswitch immediate cases -> do
      v <- eval -< immediate
      case v of
        IntVal x -> do
          label <- matchCases -< (cases, x)
          goto -< (stmts, label)
        _ -> failA -< StringVal "Expected an integer as argument for switch"
    Identity localName e _ -> do
      addr <- eval >>> alloc -< e
      env <- getEnv -< ()
      env' <- extendEnv -< (localName, addr, env)
      localEnv runStatements -< (env', (stmts, i + 1))
    IdentityNoType localName e -> do
      addr <- eval >>> alloc -< e
      env <- getEnv -< ()
      env' <- extendEnv -< (localName, addr, env)
      localEnv runStatements -< (env', (stmts, i + 1))
    Assign var e -> do
      v <- eval -< e
      case var of
        LocalVar localName -> do
          addr <- lookup' -< localName
          write -< (addr, v)
          runStatements -< (stmts, i + 1)
        ReferenceVar ref -> case ref of
          ArrayRef localName index -> do
            n <- evalIndex -< index
            (addr, ArrayVal xs) <- fetchArrayWithAddr -< localName
            if n >= 0 && n < length xs
            then do
              let xs' = replace n v xs
              write -< (addr, ArrayVal xs')
              runStatements -< (stmts, i + 1)
            else failA -< StringVal "ArrayIndexOutOfBoundsException"
          FieldRef localName fieldSignature -> do
            (addr, ObjectVal c m) <- fetchObjectWithAddr -< localName
            case m Map.!? fieldSignature of
              Just _ -> do
                let m' = Map.insert fieldSignature v m
                write -< (addr, ObjectVal c m')
                runStatements -< (stmts, i + 1)
              Nothing -> failA -< StringVal $ printf "FieldSignature %s not defined on object %s: (%s)" (show fieldSignature) (show localName) (show m)
          SignatureRef fieldSignature -> do
            (addr, _) <- fetchFieldWithAddr -< fieldSignature
            write -< (addr, v)
            runStatements -< (stmts, i + 1)
          _ -> failA -< StringVal $ printf "Can only assign a reference or a local variable"
    If e label -> do
      v <- eval -< e
      case v of
        BoolVal B.True -> goto -< (stmts, label)
        BoolVal B.False -> runStatements -< (stmts, i + 1)
        BoolVal B.Top -> joined goto runStatements -< ((stmts, label), (stmts, i + 1))
        TopVal -> returnA -< Just top
        _ -> failA -< StringVal "Expected a boolean expression for if statement"
    Goto label -> goto -< (stmts, label)
    -- Nop
    Ret e -> case e of
      Just immediate -> do
        v <- eval -< immediate
        returnA -< Just v
      Nothing -> returnA -< Nothing
    Return e -> case e of
      Just immediate -> do
        v <- eval -< immediate
        returnA -< Just v
      Nothing -> returnA -< Nothing
    Throw immediate -> do
      v <- eval -< immediate
      failA -< v
    Invoke e -> do
      evalInvoke -< e
      runStatements -< (stmts, i + 1)
    _ -> runStatements -< (stmts, i + 1)

runDeclaration :: (CanFail c, CanUseMem c, CanUseState c) => c (PointerEnv, (Type, String)) PointerEnv
runDeclaration = proc (env, (t, d)) -> do
  addr <- alloc -< defaultValue t
  extendEnv -< (d, addr, env)

runMethodBody :: CanInterp c => c MethodBody (Maybe Val)
runMethodBody = proc body -> case body of
  EmptyBody -> returnA -< Nothing
  FullBody{declarations=decs,statements=s} -> do
    env <- getEnv -< ()
    env' <- foldA runDeclaration -< (concatMap (\(t, d) -> zip (repeat t) d) decs, env)
    v <- localEnv runStatements -< (env', (s, 0))
    returnA -< v

runProgram :: CanInterp c => c (CompilationUnit, [Expr]) (Maybe Val)
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
        Just mainMethod -> tryCatchA evalMethod returnA (unbox >>> failA) -< (mainMethod, Nothing, args)
        Nothing -> returnA -< Nothing
    Nothing ->
      case findMethodByName (fileBody mainUnit) "main" of
        Just mainMethod -> tryCatchA evalMethod returnA (unbox >>> failA) -< (mainMethod, Nothing, args)
        Nothing -> returnA -< Nothing

---- End of Actual Evaluation methods ----
