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
module TypeAnalysis where

import Prelude hiding (lookup,read)

import           Data.Fixed
import           Data.List (elemIndex,find,replicate,repeat)

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
  = ArrayType Val Int
  | BooleanType
  | BottomType
  | ByteType
  | CharType
  | DoubleType
  | FloatType
  | IntType
  | Integer127Type
  | Integer1Type
  | Integer32767Type
  | LongType
  | NullType
  | RefType String
  | ShortType deriving (Eq)

instance Show Val where
  show (ArrayType v n) = show v ++ concat (replicate n "[]")
  show BooleanType = "bool"
  show BottomType = "⊥"
  show ByteType = "byte"
  show CharType = "char"
  show DoubleType = "double"
  show FloatType = "float"
  show IntType = "int"
  show Integer127Type = "[0..127]"
  show Integer1Type = "[0..1]"
  show Integer32767Type = "[0..32767]"
  show LongType = "long"
  show NullType = "null"
  show (RefType s) = show s
  show ShortType = "short"

isIntegerType :: Val -> Bool
isIntegerType BooleanType = True
isIntegerType ByteType = True
isIntegerType CharType = True
isIntegerType IntType = True
isIntegerType ShortType = True
isIntegerType Integer1Type = True
isIntegerType Integer127Type = True
isIntegerType Integer32767Type = True
isIntegerType _ = False

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

assert :: (CanFail c) => c Bool ()
assert = proc prop ->
if prop
  then returnA -< ()
  else failA -< VString "Assertion failed"

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

unbox :: (CanFail c, CanStore c) => c Val Val
unbox = proc val -> case val of
  VRef addr -> do
    v <- read' -< addr
    case v of
      VObject c m -> do
        let (keys, vals) = unzip (Map.toList m)
        vals' <- mapA unbox -< vals
        returnA -< VObject c (Map.fromList (zip keys vals'))
      VArray xs -> do
        xs' <- mapA unbox -< xs
        returnA -< VArray xs'
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
    Nothing -> failA -< VString $ printf "Field %s not bound" (show x)

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

throw :: CanInterp c => c (String, String) Val
throw = proc (clzz, message) -> do
  (VRef addr) <- newSimple -< clzz
  v <- read' -< addr
  case v of
    VObject c m -> do
      let m' = Map.insert (FieldSignature clzz (TClass "String") "message") (VString message) m
      write -< (addr, VObject c m')
      failA -< VRef addr
    _ -> failA -< VString $ printf "Undefined Exception %s" clzz

---- End of Boilerplate Methods ----

---- Evaluation Helper Methods ----

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

toIntList :: (CanFail c) => c [Val] [Int]
toIntList = proc vs -> case vs of
  (VInt x:xs) -> do
    xs' <- toIntList -< xs
    returnA -< (x:xs')
  (_:_) -> failA -< VString "Expected an integer valued array for toIntList"
  [] -> returnA -< []

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
  addr <- alloc -< (VObject c (Map.fromList fields))
  returnA -< VRef addr

newArray :: (CanFail c, CanUseState c, CanStore c) => c (Type, [Int]) Val
newArray = proc (t, sizes) -> case sizes of
  (s:sizes') -> do
    vals <- mapA newArray -< replicate s (t, sizes')
    addr <- alloc -< VArray vals
    returnA -< VRef addr
  [] -> returnA -< defaultValue t

isSuperClass :: (CanFail c, CanUseConst c) => c (String, String) Bool
isSuperClass = proc (c, p) -> if c == p
  then returnA -< True
  else do
    unit <- fetchCompilationUnit -< c
    case extends unit of
      Just c' -> isSuperClass -< (c', p)
      Nothing -> returnA -< False

isInstanceof :: (CanFail c, CanUseConst c, CanStore c) => c (Val, Type) Bool
isInstanceof = proc (v, t) -> do
  assert -< (isNonvoidType t)
  v' <- unbox -< v
  case (v', t) of
    (VBool _,     TBoolean) -> returnA -< True
    (VInt n,      TByte)    -> returnA -< n >= -128                 && n < 128                 -- n >= (-2)^7  && n < 2^7
    (VInt n,      TChar)    -> returnA -< n >= 0                    && n < 65536               -- n >= 0       && n < 2^16
    (VInt n,      TShort)   -> returnA -< n >= -32768               && n < 32768               -- n >= (-2)^15 && n < 2^15
    (VInt n,      TInt)     -> returnA -< n >= -2147483648          && n < 2147483648          -- n >= (-2)^31 && n < 2^31
    (VLong l,     TLong)    -> returnA -< l >= -9223372036854775808 && l <= 9223372036854775807 -- l >= (-2)^63 && l < 2^63
    (VFloat _,    TFloat)   -> returnA -< True
    (VDouble _,   TDouble)  -> returnA -< True
    (VNull,       TNull)    -> returnA -< True
    (VObject c _, TClass p) -> isSuperClass -< (c, p)
    (VArray xs,   TArray t') -> (mapA isInstanceof >>^ all (==True)) -< zip xs (repeat t')
    (_, _) -> returnA -< False

evalIndex :: (CanInterp c) => c Expr Int
evalIndex = proc i -> do
  v <- eval -< i
  case v of
    VInt n -> returnA -< n
    _ -> failA -< VString "Expected an integer array index"

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
  _ -> failA -< VString "Not implemented"

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
  VRef addr -> do
    o <- read' -< addr
    case o of
      VObject _ _ -> returnA -< o
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

---- End of Evaluation Helper Methods ----

---- Specific Type Analysis Helper Methods ----

evalBinaryOp :: c (Val, Val) -> Val
evalBinaryOp = proc (tl, tr) -> if (isIntegerType tl && isIntegerType tr)
  then case (tl, tr) of
    (BooleanType, _) -> returnA -< tr
    (_, BooleanType) -> returnA -< tl
    (_, _) -> failA -< VString "Not yet supported"
      -- TODO: Multiple types might come in here
      -- Collection<Type> rs = AugHierarchy.lcas_(tl, tr);
      -- // AugHierarchy.lcas_ is single-valued
      -- for (Type r : rs) {
      --   return r;
      -- }
      -- throw new RuntimeException();
  else returnA -< tl

getIntConstantType :: Int -> Val
getIntConstantType n
  | n >= 0 && n < 2         = Integer1Type
  | n >= 2 && n < 128       = Integer127Type
  | n >= -128 && n < 0      = ByteType
  | n >= 128 && n < 32768   = Integer32767Type
  | n >= -32768 && n < -128 = ShortType
  | n >= 32768 && n < 65536 = CharType
  | otherwise               = IntType

---- End of Specific Type Analysis Helper Methods ----

---- Actual Evaluation methods ----

eval :: CanInterp c => c Expr Val
eval = proc e -> case e of
  ThisRef -> eval -< (Local "@this") -- TODO: Lookup in current method signature
  ParameterRef n -> eval -< (Local ("@parameter" ++ show n)) -- TODO: Lookup in current method signature
  Local localName -> fetchLocal -< localName
  BinopExpr el op er -> do
    tl <- eval -< el
    tr <- eval -< er
    case op of
      Cmp -> returnA -< ByteType
      Cmpg -> returnA -< ByteType
      Cmpl -> returnA -< ByteType
      Cmpeq -> returnA -< BooleanType
      Cmpne -> returnA -< BooleanType
      Cmpgt -> returnA -< BooleanType
      Cmpge -> returnA -< BooleanType
      Cmplt -> returnA -< BooleanType
      Cmple -> returnA -< BooleanType
      Shl -> returnA -< if isIntegerType tl then IntType else tl
      Shr -> returnA -< tl
      Ushr -> returnA -< tl
      Plus -> returnA -< if isIntegerType tl then IntType else tl
      Minus -> returnA -< if isIntegerType tl then IntType else tl
      Mult -> returnA -< if isIntegerType tl then IntType else tl
      Div -> returnA -< if isIntegerType tl then IntType else tl
      Rem -> returnA -< if isIntegerType tl then IntType else tl
      Mod -> returnA -< if isIntegerType tl then IntType else tl
      And -> evalBinaryOp -< (tl, tr)
      Or -> evalBinaryOp -< (tl, tr)
      Xor -> evalBinaryOp -< (tl, tr)
  UnopExpr op e -> do
    t <- eval -< e
    case op of
      Lengthof -> returnA -< IntType
      Neg -> returnA -< if isIntegerType t
        then case t of
          Integer1Type -> ByteType
          BooleanType -> ByteType
          Integer127Type -> ByteType
          ByteType -> ByteType
          ShortType -> ShortType
          Integer32767Type -> ShortType
          _ -> IntType
        else t
  CaughtExceptionRef -> eval -< (Local "@caughtexception") -- TODO: Check which exceptions it could have catched by examining statements
  ArrayRef localName i -> do
    t <- fetchLocal -< localName
    returnA -< case t of
      ArrayType t' -> t'
      RefType c -> if c `elem` ["java.lang.Object", "java.io.Serializable", "java.lang.Cloneable"]
        then t
        else BottomType
      _ -> BottomType
  NewArrayExpr t _ -> returnA -< ArrayType t
  NewMultiArrayExpr t is -> returnA -< (foldl (\acc _ -> ArrayType acc) t is)
  CastExpr t _ -> returnA -< t
  InstanceOfExpr _ _ -> returnA -< BooleanType
  InvokeExpr invokeExpr -> returnA -< case invokeExpr of
    SpecialInvoke _ (MethodSignature _ returnType _ _) _ -> returnType
    VirtualInvoke _ (MethodSignature _ returnType _ _) _ -> returnType
    InterfaceInvoke _ (MethodSignature _ returnType _ _) _ -> returnType
    StaticInvoke (MethodSignature _ returnType _ _) _ -> returnType
    DynamicInvoke _ _ _ (MethodSignature _ returnType _ _) _ -> returnType
  NewExpr t -> if isBaseType t
    then returnA -< case t of
      TClass c -> RefType c
      _ -> returnA -< t
    else failA -< VString "Expected a nonvoid base type for new"
  FieldRef localName fieldSignature -> do
    (_, VObject _ m) <- fetchObjectWithAddr -< localName
    case Map.lookup fieldSignature m of
      Just x -> returnA -< x
      Nothing -> failA -< VString $ printf "Field %s not defined for object %s" (show fieldSignature) (show localName)
  SignatureRef fieldSignature -> do
    (_, val) <- fetchFieldWithAddr -< fieldSignature
    returnA -< val
  DoubleConstant _ -> returnA -< DoubleType
  FloatConstant _ -> returnA -< FloatType
  IntConstant n -> returnA -< (getIntConstantType n)
  LongConstant _ -> returnA -< NullType
  NullConstant -> returnA -< VNull
  StringConstant _ -> returnA -< RefType "java.lang.String"
  ClassConstant _ -> returnA -< RefType "java.lang.Class"
  MethodHandle _ -> returnA -< RefType "java.lang.invoke.MethodHandle"

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
        VInt x -> do
          label <- matchCases -< (cases, x)
          goto -< (stmts, label)
        _ -> failA -< VString "Expected an integer as argument for switch"
    Lookupswitch immediate cases -> do
      v <- eval -< immediate
      case v of
        VInt x -> do
          label <- matchCases -< (cases, x)
          goto -< (stmts, label)
        _ -> failA -< VString "Expected an integer as argument for switch"
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
        VLocal localName -> do
          addr <- lookup' -< localName
          write -< (addr, v)
          runStatements -< (stmts, i + 1)
        VReference ref -> case ref of
          ArrayRef localName index -> do
            n <- evalIndex -< index
            (addr, VArray xs) <- fetchArrayWithAddr -< localName
            if n >= 0 && n < length xs
            then do
              let xs' = replace n v xs
              write -< (addr, VArray xs')
              runStatements -< (stmts, i + 1)
            else failA -< VString "ArrayIndexOutOfBoundsException"
          FieldRef localName fieldSignature -> do
            (addr, VObject c m) <- fetchObjectWithAddr -< localName
            case m Map.!? fieldSignature of
              Just _ -> do
                let m' = Map.insert fieldSignature v m
                write -< (addr, VObject c m')
                runStatements -< (stmts, i + 1)
              Nothing -> failA -< VString $ printf "FieldSignature %s not defined on object %s: (%s)" (show fieldSignature) (show localName) (show m)
          SignatureRef fieldSignature -> do
            (addr, _) <- fetchFieldWithAddr -< fieldSignature
            write -< (addr, v)
            runStatements -< (stmts, i + 1)
          _ -> failA -< VString $ printf "Can only assign a reference or a local variable"
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
  MEmpty -> returnA -< Nothing
  MFull{declarations=decs,statements=s} -> do
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
