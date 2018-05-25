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

import           Prelude hiding (lookup,read,rem,div,mod)
import qualified Prelude as P

import           Data.Fixed
import           Data.List (elemIndex,find,replicate,repeat)
import           Data.Map (Map)
import qualified Data.Map as Map

import           Data.Concrete.Error
import           Data.Concrete.Environment (Env)
import qualified Data.Concrete.Environment as E
import qualified Data.Concrete.Store as S

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
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Concrete.MaybeEnvironment
import           Control.Arrow.Transformer.Concrete.MaybeStore

import           Text.Printf

import           Syntax
import           Shared

---- Values ----

data Val
  = BottomVal
  | IntVal Int
  | LongVal Int
  | FloatVal Float
  | DoubleVal Float
  | StringVal String
  | ClassVal String
  | BoolVal Bool
  | NullVal
  | RefVal Addr
  | ArrayVal [Val]
  | ObjectVal String (Map FieldSignature Val) deriving (Eq)

instance Show Val where
  show BottomVal = "⊥"
  show (IntVal n) = show n
  show (LongVal l) = show l ++ "l"
  show (FloatVal f) = show f
  show (DoubleVal d) = show d ++ "d"
  show (StringVal s) = s
  show (ClassVal c) = "<" ++ c ++ ">"
  show (BoolVal b) = show b
  show NullVal = "null"
  show (RefVal a) = "@" ++ show a
  show (ArrayVal xs) = show xs
  show (ObjectVal c m) = show c ++ "{" ++ show m ++ "}"

defaultValue :: Type -> Val
defaultValue BooleanType = BoolVal False
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

isNumVal :: Val -> Maybe (Either Int Float)
isNumVal (IntVal n) = Just (Left n)
isNumVal (LongVal l) = Just (Left l)
isNumVal (FloatVal f) = Just (Right f)
isNumVal (DoubleVal d) = Just (Right d)
isNumVal _ = Nothing

---- End of Values ----

---- Interp Type ----

newtype Interp x y = Interp
  (Except (Exception Val)
    (Reader MethodReader
      (MaybeEnvironment String Addr
        (MaybeStoreArrow Addr Val
          (State Addr
            (Const (CompilationUnits, Fields) (->)))))) x y)
  deriving (Category,Arrow,ArrowChoice)

instance ArrowTryCatch (Exception Val) x y Interp where
  tryCatchA (Interp f) (Interp g) = Interp $ tryCatchA f g

deriving instance ArrowConst (CompilationUnits, Fields) Interp
deriving instance ArrowFail (Exception Val) Interp
deriving instance ArrowReader MethodReader Interp
deriving instance ArrowState Addr Interp
deriving instance ArrowMaybeEnv String Addr (PointerEnv Env) Interp
deriving instance ArrowMaybeStore Addr Val Interp

---- End of Interp type ----

---- Program Boilerplate ----

runInterp :: Interp x y -> [CompilationUnit] -> [(String, Addr)] -> [(Addr, Val)] -> MethodReader -> x -> Error (Exception Val) y
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
  (latestAddr + length fields, (S.fromList store', (env, (mainMethod, x))))

---- End of Program Boilerplate ----

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

toInt :: (CanFail Val c) => c Val Int
toInt = proc v -> case v of
  IntVal x -> returnA -< x
  LongVal x -> returnA -< x
  _ -> failA -< StaticException "Expected an integer valued array for toInt"

unbox :: (CanFail Val c, CanUseStore Val c) => c Val Val
unbox = proc val -> case val of
  RefVal addr -> do
    v <- Shared.read' -< addr
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

unboxMaybe :: (CanFail Val c, CanUseMem Env Val c) => c (Maybe Val) (Maybe Val)
unboxMaybe = proc val -> case val of
  Just x -> do
    x' <- unbox -< x
    returnA -< Just x'
  Nothing -> returnA -< Nothing

fetchLocal :: (CanFail Val c, CanUseMem Env Val c) => c String Val
fetchLocal = Shared.lookup' >>> Shared.read'

lookupField :: (CanFail Val c, CanUseConst c) => c FieldSignature Addr
lookupField = proc x -> do
  (_,fields) <- askConst -< ()
  case Map.lookup x fields of
    Just addr -> returnA -< addr
    Nothing -> failA -< StaticException $ printf "Field %s not bound" (show x)

fetchFieldWithAddr :: (CanFail Val c, CanUseStore Val c, CanUseConst c) => c FieldSignature (Addr, Val)
fetchFieldWithAddr = proc x -> do
  addr <- lookupField -< x
  val <- Shared.read' -< addr
  returnA -< (addr, val)

fetchCompilationUnit :: (CanFail Val c, CanUseConst c) => c String CompilationUnit
fetchCompilationUnit = proc n -> do
  (compilationUnits, _) <- askConst -< ()
  case Map.lookup n compilationUnits of
    Just x -> returnA -< x
    Nothing -> failA -< StaticException $ printf "CompilationUnit %s not loaded" (show n)

alloc :: (CanUseState c, CanUseStore Val c) => c Val Addr
alloc = proc val -> do
  addr <- getA -< ()
  write -< (addr, val)
  putA -< succ addr
  returnA -< addr

matchesSignature :: Type -> String -> [Type] -> Member -> Bool
matchesSignature retType n argTypes (MethodMember m) =
  methodName m == n && returnType m == retType && parameters m == argTypes
matchesSignature _ _ _ _ = False

fetchMethod :: (CanFail Val c, CanUseConst c) => c MethodSignature Method
fetchMethod = proc (MethodSignature c retType n argTypes) -> do
  unit <- fetchCompilationUnit -< c
  case find (matchesSignature retType n argTypes) (fileBody unit) of
    Just (MethodMember m) -> returnA -< m
    _ -> failA -< StaticException $ printf "Method %s not defined for class %s" (show n) (show c)

fetchRefValWithAddr :: (CanFail Val c, CanUseMem Env Val c) => c String (Addr, Val)
fetchRefValWithAddr = proc localName -> do
  v <- fetchLocal -< localName
  case v of
    RefVal addr -> do
      v' <- Shared.read' -< addr
      returnA -< (addr, v')
    _ -> failA -< StaticException $ printf "Variable %s is not a reference" (show localName)

fetchArrayWithAddr :: (CanFail Val c, CanUseMem Env Val c) => c String (Addr, Val)
fetchArrayWithAddr = proc localName -> do
  (addr, v) <- fetchRefValWithAddr -< localName
  case v of
    ArrayVal _ -> returnA -< (addr, v)
    _ -> failA -< StaticException $ printf "Variable %s not bound to an array" (show localName)

fetchObjectWithAddr :: (CanFail Val c, CanUseMem Env Val c) => c String (Addr, Val)
fetchObjectWithAddr = proc localName -> do
  (addr, v) <- fetchRefValWithAddr -< localName
  case v of
    ObjectVal _ _ -> returnA -< (addr, v)
    _ -> failA -< StaticException $ printf "Variable %s not bound to an object" (show localName)

getCurrentMethodBody :: (CanFail Val c, CanUseReader c) => c () MethodBody
getCurrentMethodBody = proc () -> do
  m <- askA -< ()
  case methodBody m of
    EmptyBody -> failA -< StaticException $ printf "Empty body for method %s" (show m)
    FullBody{} -> returnA -< methodBody m

throw :: CanInterp Env Val c => c (String, String) Val
throw = proc (clzz, message) -> do
  (RefVal addr) <- newSimple -< RefType clzz
  v <- Shared.read' -< addr
  case v of
    ObjectVal c m -> do
      let m' = Map.insert (FieldSignature clzz (RefType "String") "message") (StringVal message) m
      write -< (addr, ObjectVal c m')
      failA -< DynamicException (RefVal addr)
    _ -> failA -< StaticException $ printf "Undefined exception %s" clzz

---- End of Boilerplate Methods ----

---- Evaluation Helper Methods ----

isSuperClass :: (CanFail Val c, CanUseConst c) => c (String, String) Bool
isSuperClass = proc (c, p) -> if c == p
  then returnA -< True
  else do
    unit <- fetchCompilationUnit -< c
    case extends unit of
      Just c' -> isSuperClass -< (c', p)
      Nothing -> returnA -< False

isInstanceof :: (CanFail Val c, CanUseConst c, CanUseStore Val c) => c (Val, Type) Bool
isInstanceof = proc (v, t) -> do
  Shared.assert -< (isNonvoidType t, "Expected a nonvoid type for instanceof")
  v' <- unbox -< v
  case (v', t) of
    (BoolVal _,     BooleanType)  -> returnA -< True
    (IntVal n,      ByteType)     -> returnA -< n >= -128                 && n < 128                 -- n >= (-2)^7  && n < 2^7
    (IntVal n,      CharType)     -> returnA -< n >= 0                    && n < 65536               -- n >= 0       && n < 2^16
    (IntVal n,      ShortType)    -> returnA -< n >= -32768               && n < 32768               -- n >= (-2)^15 && n < 2^15
    (IntVal n,      IntType)      -> returnA -< n >= -2147483648          && n < 2147483648          -- n >= (-2)^31 && n < 2^31
    (LongVal l,     LongType)     -> returnA -< l >= -9223372036854775808 && l <= 9223372036854775807 -- l >= (-2)^63 && l < 2^63
    (FloatVal _,    FloatType)    -> returnA -< True
    (DoubleVal _,   DoubleType)   -> returnA -< True
    (NullVal,       NullType)     -> returnA -< True
    (ObjectVal c _, RefType p)    -> isSuperClass -< (c, p)
    (ArrayVal xs,   ArrayType t') -> (mapA isInstanceof >>^ all (==True)) -< zip xs (repeat t')
    (_, _) -> returnA -< False

evalIndex :: (CanInterp Env Val c) => c Expr Int
evalIndex = proc i -> do
  v <- Shared.eval -< i
  case v of
    IntVal n -> returnA -< n
    LongVal l -> returnA -< l
    _ -> failA -< StaticException "Expected an integer array index"

evalInvoke :: CanInterp Env Val c => c EInvoke (Maybe Val)
evalInvoke = proc e -> case e of
  StaticInvoke methodSignature args -> do
    method <- fetchMethod -< methodSignature
    Shared.assert -< (Static `elem` methodModifiers method, "Expected a static method for static invoke")
    runMethod -< (method, Nothing, args)
  VirtualInvoke localName methodSignature args -> do
    method <- fetchMethod -< methodSignature
    Shared.assert -< (Static `notElem` methodModifiers method, "Expected a non-static method for static invoke")
    this <- fetchLocal -< localName
    runMethod -< (method, Just this, args)
  SpecialInvoke localName methodSignature args -> do
    method <- fetchMethod -< methodSignature
    Shared.assert -< (Static `notElem` methodModifiers method, "Expected a non-static method for static invoke")
    this <- fetchLocal -< localName
    runMethod -< (method, Just this, args)
  _ -> failA -< StaticException "Not implemented"

statementsFromLabel :: (CanFail Val c, CanUseReader c) => c String [Statement]
statementsFromLabel = proc label -> do
  b <- getCurrentMethodBody -< ()
  case Label label `elemIndex` (statements b) of
    Just i -> returnA -< drop i (statements b)
    Nothing -> failA -< StaticException $ printf "Undefined label: %s" label

matchCases :: (CanFail Val c) => c ([CaseStatement], Int) String
matchCases = proc (cases, v) -> case cases of
  ((ConstantCase n, label): cases') -> if v == n
    then returnA -< label
    else matchCases -< (cases', v)
  ((DefaultCase, label): _) -> returnA -< label
  [] -> failA -< StaticException $ printf "No cases match value %s" (show v)

---- End of Evaluation Helper Methods ----

---- Actual Evaluation methods ----

-- eval :: CanInterp Env Val c => c Expr Val
-- eval = proc e -> case e of
--   NewExpr t -> do
--     Shared.assert -< (isBaseType t, "Expected a base type for new")
--     case t of
--       RefType c -> newSimple -< c
--       _ -> returnA -< (defaultValue t)
--   NewArrayExpr t i -> do
--     Shared.assert -< (isNonvoidType t, "Expected a nonvoid type for newarray")
--     v <- eval -< i
--     n <- toInt -< v
--     Shared.assert -< (n > 0, "Expected a positive integer for newarray size")
--     newArray -< (t, [n])
--   NewMultiArrayExpr t is -> do
--     Shared.assert -< (isBaseType t, "Expected a nonvoid base type for newmultiarray")
--     vs <- mapA eval -< is
--     ns <- mapA toInt -< vs
--     Shared.assert -< (all (>0) ns, "Expected positive integers for newmultiarray sizes")
--     newArray -< (t, ns)
--   CastExpr t i -> do
--     v <- eval -< i
--     b <- isInstanceof -< (v, t)
--     if b
--     then do
--       v' <- unbox -< v
--       case v' of
--         ObjectVal _ _ -> returnA -< v
--         _ -> failA -< StaticException "Casting of primivites and arrays is not yet supported"
--         -- https://docs.oracle.com/javase/specs/jls/se7/html/jls-5.html#jls-5.1.2
--     else throw -< ("java.lang.ClassCastException", printf "Cannot cast %s to type %s" (show v) (show t))
--   InstanceOfExpr i t -> do
--     v <- eval -< i
--     b <- isInstanceof -< (v, t)
--     returnA -< BoolVal b
--   InvokeExpr invokeExpr -> do
--     v <- tryCatchA evalInvoke (pi2 >>> failA) -< invokeExpr
--     case v of
--       Just v' -> returnA -< v'
--       Nothing -> failA -< StaticException "Method returned nothing"
--   ArrayRef localName i -> do
--     n <- evalIndex -< i
--     (_, ArrayVal xs) <- fetchArrayWithAddr -< localName
--     if n >= 0 && n < length xs
--     then returnA -< xs !! n
--     else throw -< ("java.lang.ArrayIndexOutOfBoundsException", printf "Index %d out of bounds" (show n))
--   FieldRef localName fieldSignature -> do
--     (_, ObjectVal _ m) <- fetchObjectWithAddr -< localName
--     case Map.lookup fieldSignature m of
--       Just x -> returnA -< x
--       Nothing -> failA -< StaticException $ printf "Field %s not defined for object %s" (show fieldSignature) (show localName)
--   SignatureRef fieldSignature -> do
--     (_, val) <- fetchFieldWithAddr -< fieldSignature
--     returnA -< val
--   BinopExpr i1 op i2 -> do
--     v1 <- eval -< i1
--     v2 <- eval -< i2
--     case op of
--       Cmpeq -> returnA -< BoolVal (v1 == v2)
--       Cmpne -> returnA -< BoolVal (v1 /= v2)
--       _ -> do
--         let toFloatVal :: Float -> Val
--             toFloatVal x = case (v1, v2) of
--               (DoubleVal _, _) -> DoubleVal x
--               (_, DoubleVal _) -> DoubleVal x
--               (_, _) -> FloatVal x
--         let toIntVal :: Int -> Val
--             toIntVal x = case (v1, v2) of
--               (LongVal _, _) -> LongVal x
--               (_, LongVal _) -> LongVal x
--               (_, _) -> IntVal x
--         case (isNumVal v1, isNumVal v2) of
--           (Nothing, _) -> failA -< StaticException $ printf "Expected two numbers as argument for %s" (show op)
--           (_, Nothing) -> failA -< StaticException $ printf "Expected two numbers as argument for %s" (show op)
--           (Just (Right x1), Just (Right x2)) -> evalBinopFractional -< (toFloatVal, op, x1, x2)
--           (Just (Right x1), Just (Left x2))  -> evalBinopFractional -< (toFloatVal, op, x1, fromIntegral x2)
--           (Just (Left x1),  Just (Right x2)) -> evalBinopFractional -< (toFloatVal, op, fromIntegral x1, x2)
--           (Just (Left x1),  Just (Left x2))  -> evalBinopIntegral   -< (toIntVal,   op, x1, x2)
--   UnopExpr op i -> do
--     v <- eval -< i
--     case op of
--       Lengthof -> case v of
--         RefVal addr -> do
--           v' <- read' -< addr
--           case v' of
--             ArrayVal xs -> returnA -< (IntVal (length xs))
--             _ -> failA -< StaticException "Expected an array as argument for lengthof"
--         _ -> failA -< StaticException "Expected an array as argument for lengthof"
--       Neg -> case v of
--         IntVal n -> returnA -< (IntVal (-n))
--         LongVal l -> returnA -< (LongVal (-l))
--         FloatVal f -> returnA -< (FloatVal (-f))
--         DoubleVal d -> returnA -< (DoubleVal (-d))
--         _ -> failA -< StaticException "Expected a number as argument for -"
--   ThisRef -> eval -< (Local "@this")
--   ParameterRef n -> eval -< (Local ("@parameter" ++ show n))
--   CaughtExceptionRef -> eval -< (Local "@caughtexception")
--   Local localName -> fetchLocal -< localName
--   DoubleConstant f -> returnA -< (DoubleVal f)
--   FloatConstant f -> returnA -< (FloatVal f)
--   IntConstant n -> returnA -< (IntVal n)
--   LongConstant f -> returnA -< (LongVal f)
--   NullConstant -> returnA -< NullVal
--   StringConstant s -> returnA -< (StringVal s)
--   ClassConstant c -> returnA -< (ClassVal c)
--   MethodHandle _ -> failA -< StaticException "Evaluation of method handles is not implemented"

getInitializedFields :: (CanFail Val c, CanUseConst c) => c String [(FieldSignature, Val)]
getInitializedFields = proc c -> do
  unit <- fetchCompilationUnit -< c
  let fieldSignatures = getFieldSignatures unit (\m -> Static `notElem` m)
  let ownFields = map (\s@(FieldSignature _ t' _) -> (s, defaultValue t')) fieldSignatures
  case extends unit of
    Just p -> do
      parentFields <- getInitializedFields -< p
      returnA -< parentFields ++ ownFields
    Nothing -> returnA -< ownFields

instance UseVal Val Interp where
  newSimple = proc t -> do
    case t of
      RefType c -> do
        fields <- getInitializedFields -< c
        addr <- alloc -< (ObjectVal c (Map.fromList fields))
        returnA -< RefVal addr
      _ -> returnA -< defaultValue t
  newArray = proc (t, sizes) -> case sizes of
    (s:sizes') -> do
      s' <- toInt -< s
      vals <- mapA newArray -< replicate s' (t, sizes')
      addr <- alloc -< ArrayVal vals
      returnA -< RefVal addr
    [] -> returnA -< defaultValue t
  -- and :: c (v,v) v
  -- or :: c (v,v) v
  -- xor :: c (v,v) v
  -- rem = proc (v1,v2) -> case (v1,v2) of
  --   (IntVal x1, IntVal x2) -> returnA -< IntVal (x1 `P.rem` x2)
  --   (LongVal x1, LongVal x2) -> returnA -< LongVal (x1 `P.rem` x2)
  --   (FloatVal x1, FloatVal x2) -> returnA -< FloatVal (x1 `rem'` x2)
  --   (DoubleVal x1, DoubleVal x2) -> returnA -< DoubleVal (x1 `rem'` x2)
  --   _ -> failA -< StaticException "Expected integer variables for rem"
  mod = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1, IntVal x2) -> returnA -< IntVal (x1 `P.mod` x2)
    (LongVal x1, LongVal x2) -> returnA -< LongVal (x1 `P.mod` x2)
    (FloatVal x1, FloatVal x2) -> returnA -< FloatVal (x1 `mod'` x2)
    (DoubleVal x1, DoubleVal x2) -> returnA -< DoubleVal (x1 `mod'` x2)
    _ -> failA -< StaticException "Expected integer variables for mod"
  cmp = proc (v1,v2) -> case (v1,v2) of
    (LongVal x1, LongVal x2)
      | x1 < x2 -> returnA -< IntVal (-1)
      | x1 == x2 -> returnA -< IntVal 0
      | x1 > x2 -> returnA -< IntVal 1
    _ -> failA -< StaticException "Expected long variables for cmp"
  cmpg = proc (v1,v2) -> case (v1,v2) of
    (FloatVal x1, FloatVal x2)
      | x1 < x2 -> returnA -< IntVal (-1)
      | x1 == x2 -> returnA -< IntVal 0
      | x1 > x2 -> returnA -< IntVal 1
    (DoubleVal x1, DoubleVal x2)
      | x1 < x2 -> returnA -< IntVal (-1)
      | x1 == x2 -> returnA -< IntVal 0
      | x1 > x2 -> returnA -< IntVal 1
    _ -> failA -< StaticException "Expected floating variables for cmpg"
  cmpl = proc (v1,v2) -> case (v1,v2) of
    (FloatVal x1, FloatVal x2)
      | x1 < x2 -> returnA -< IntVal (-1)
      | x1 == x2 -> returnA -< IntVal 0
      | x1 > x2 -> returnA -< IntVal 1
    (DoubleVal x1, DoubleVal x2)
      | x1 < x2 -> returnA -< IntVal (-1)
      | x1 == x2 -> returnA -< IntVal 0
      | x1 > x2 -> returnA -< IntVal 1
    _ -> failA -< StaticException "Expected floating variables for cmpl"
  eq = proc (v1, v2) -> returnA -< BoolVal (v1 == v2)
  neq = proc (v1, v2) -> returnA -< BoolVal (v1 /= v2)
  gt = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1, IntVal x2) -> returnA -< BoolVal (x1 > x2)
    (LongVal x1, LongVal x2) -> returnA -< BoolVal (x1 > x2)
    (FloatVal x1, FloatVal x2) -> returnA -< BoolVal (x1 > x2)
    (DoubleVal x1, DoubleVal x2) -> returnA -< BoolVal (x1 > x2)
    _ -> failA -< StaticException "Expected numeric variables for gt"
  ge = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1, IntVal x2) -> returnA -< BoolVal (x1 >= x2)
    (LongVal x1, LongVal x2) -> returnA -< BoolVal (x1 >= x2)
    (FloatVal x1, FloatVal x2) -> returnA -< BoolVal (x1 >= x2)
    (DoubleVal x1, DoubleVal x2) -> returnA -< BoolVal (x1 >= x2)
    _ -> failA -< StaticException "Expected numeric variables for ge"
  lt = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1, IntVal x2) -> returnA -< BoolVal (x1 < x2)
    (LongVal x1, LongVal x2) -> returnA -< BoolVal (x1 < x2)
    (FloatVal x1, FloatVal x2) -> returnA -< BoolVal (x1 < x2)
    (DoubleVal x1, DoubleVal x2) -> returnA -< BoolVal (x1 < x2)
    _ -> failA -< StaticException "Expected numeric variables for lt"
  le = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1, IntVal x2) -> returnA -< BoolVal (x1 <= x2)
    (LongVal x1, LongVal x2) -> returnA -< BoolVal (x1 <= x2)
    (FloatVal x1, FloatVal x2) -> returnA -< BoolVal (x1 <= x2)
    (DoubleVal x1, DoubleVal x2) -> returnA -< BoolVal (x1 <= x2)
    _ -> failA -< StaticException "Expected numeric variables for le"
  -- shl = proc (v1,v2) -> case (v1,v2) of
  --   (IntVal x1, IntVal x2) -> returnA -< x1  x2
  --   (LongVal x1, LongVal x2) -> returnA -< x1  x2
  --   (FloatVal x1, FloatVal x2) -> returnA -< x1  x2
  --   (DoubleVal x1, DoubleVal x2) -> returnA -< x1  x2
  -- shr = proc (v1,v2) -> case (v1,v2) of
  --   (IntVal x1, IntVal x2) -> returnA -< x1  x2
  --   (LongVal x1, LongVal x2) -> returnA -< x1  x2
  --   (FloatVal x1, FloatVal x2) -> returnA -< x1  x2
  --   (DoubleVal x1, DoubleVal x2) -> returnA -< x1  x2
  -- ushr = proc (v1,v2) -> case (v1,v2) of
  --   (IntVal x1, IntVal x2) -> returnA -< x1  x2
  --   (LongVal x1, LongVal x2) -> returnA -< x1  x2
  --   (FloatVal x1, FloatVal x2) -> returnA -< x1  x2
  --   (DoubleVal x1, DoubleVal x2) -> returnA -< x1  x2
  plus = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1, IntVal x2) -> returnA -< IntVal (x1 + x2)
    (LongVal x1, LongVal x2) -> returnA -< LongVal (x1 + x2)
    (FloatVal x1, FloatVal x2) -> returnA -< FloatVal (x1 + x2)
    (DoubleVal x1, DoubleVal x2) -> returnA -< DoubleVal (x1 + x2)
    _ -> failA -< StaticException "Expected numeric variables for plus"
  minus = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1, IntVal x2) -> returnA -< IntVal (x1 - x2)
    (LongVal x1, LongVal x2) -> returnA -< LongVal (x1 - x2)
    (FloatVal x1, FloatVal x2) -> returnA -< FloatVal (x1 - x2)
    (DoubleVal x1, DoubleVal x2) -> returnA -< DoubleVal (x1 - x2)
    _ -> failA -< StaticException "Expected numeric variables for minus"
  mult = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1, IntVal x2) -> returnA -< IntVal (x1 * x2)
    (LongVal x1, LongVal x2) -> returnA -< LongVal (x1 * x2)
    (FloatVal x1, FloatVal x2) -> returnA -< FloatVal (x1 * x2)
    (DoubleVal x1, DoubleVal x2) -> returnA -< DoubleVal (x1 * x2)
    _ -> failA -< StaticException "Expected numeric variables for mult"
  div = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1, IntVal x2) -> if x2 == 0
      then throw -< ("java.lang.ArithmeticException", "/ by zero")
      else returnA -< IntVal (x1 `P.div` x2)
    (LongVal x1, LongVal x2) -> if x2 == 0
      then throw -< ("java.lang.ArithmeticException", "/ by zero")
      else returnA -< LongVal (x1 `P.div` x2)
    (FloatVal x1, FloatVal x2) -> returnA -< FloatVal (x1 / x2)
    (DoubleVal x1, DoubleVal x2) -> returnA -< DoubleVal (x1 / x2)
    _ -> failA -< StaticException "Expected numeric variables for div"
  lengthOf = proc v -> do
    v' <- unbox -< v
    case v' of
      ArrayVal xs -> returnA -< (IntVal (length xs))
      _ -> failA -< StaticException "Expected an array as argument for lengthof"
  neg = proc v -> case v of
    IntVal n -> returnA -< (IntVal (-n))
    LongVal l -> returnA -< (LongVal (-l))
    FloatVal f -> returnA -< (FloatVal (-f))
    DoubleVal d -> returnA -< (DoubleVal (-d))
    _ -> failA -< StaticException "Expected a number as argument for -"
  doubleConstant = arr (\x -> DoubleVal x)
  floatConstant = arr (\x -> FloatVal x)
  intConstant = arr (\x -> IntVal x)
  longConstant = arr (\x -> LongVal x)
  nullConstant = arr (\() -> NullVal)
  stringConstant = arr (\x -> StringVal x)
  classConstant = arr (\x -> ClassVal x)

runStatements :: CanInterp Env Val c => c [Statement] (Maybe Val)
runStatements = proc stmts -> case stmts of
  [] -> returnA -< Nothing
  (stmt:rest) -> case stmt of
    Label _ -> tryCatchA ((\(_:r) -> r) ^>> runStatements) (proc ((Label labelName):stmts, exception) -> do
      case exception of
        StaticException _ -> failA -< exception
        DynamicException val -> do
          body <- getCurrentMethodBody -< ()
          val' <- unbox -< val
          case val' of
            ObjectVal c _ -> case find (\clause -> className clause == c && fromLabel clause == labelName && (Label (toLabel clause)) `elem` stmts) (catchClauses body) of
              Just clause -> do
                env <- getEnv -< ()
                addr <- alloc -< val
                env' <- extendEnv -< ("@caughtexception", addr, env)
                localEnv (statementsFromLabel >>> runStatements) -< (env', withLabel clause)
              Nothing -> failA -< exception
            _ -> failA -< exception) -< stmts

    -- Breakpoint
    Tableswitch immediate cases -> do
      v <- Shared.eval -< immediate
      case v of
        IntVal x ->
          matchCases >>> statementsFromLabel >>> runStatements -< (cases, x)
        _ -> failA -< StaticException "Expected an integer as argument for switch"
    Lookupswitch immediate cases -> do
      v <- Shared.eval -< immediate
      case v of
        IntVal x -> do
          matchCases >>> statementsFromLabel >>> runStatements -< (cases, x)
        _ -> failA -< StaticException "Expected an integer as argument for switch"
    Identity localName e _ -> do
      addr <- Shared.eval >>> alloc -< e
      env <- getEnv -< ()
      env' <- extendEnv -< (localName, addr, env)
      localEnv runStatements -< (env', rest)
    IdentityNoType localName e -> do
      addr <- Shared.eval >>> alloc -< e
      env <- getEnv -< ()
      env' <- extendEnv -< (localName, addr, env)
      localEnv runStatements -< (env', rest)
    Assign var e -> do
      v <- Shared.eval -< e
      case var of
        LocalVar localName -> do
          addr <- Shared.lookup' -< localName
          write -< (addr, v)
          runStatements -< rest
        ReferenceVar ref -> case ref of
          ArrayRef localName index -> do
            n <- evalIndex -< index
            (addr, ArrayVal xs) <- fetchArrayWithAddr -< localName
            if n >= 0 && n < length xs
            then do
              let xs' = replace n v xs
              write -< (addr, ArrayVal xs')
              runStatements -< rest
            else failA -< StaticException "ArrayIndexOutOfBoundsException"
          FieldRef localName fieldSignature -> do
            (addr, ObjectVal c m) <- fetchObjectWithAddr -< localName
            case m Map.!? fieldSignature of
              Just _ -> do
                let m' = Map.insert fieldSignature v m
                write -< (addr, ObjectVal c m')
                runStatements -< rest
              Nothing -> failA -< StaticException $ printf "FieldSignature %s not defined on object %s: (%s)" (show fieldSignature) (show localName) (show m)
          SignatureRef fieldSignature -> do
            (addr, _) <- fetchFieldWithAddr -< fieldSignature
            write -< (addr, v)
            runStatements -< rest
          _ -> failA -< StaticException $ printf "Can only assign a reference or a local variable"
    If e label -> do
      v <- Shared.eval -< e
      case v of
        BoolVal True -> (statementsFromLabel >>> runStatements) -< label
        BoolVal False -> runStatements -< rest
        _ -> failA -< StaticException "Expected a boolean expression for if statement"
    Goto label -> (statementsFromLabel >>> runStatements) -< label
    -- Nop
    Ret e -> case e of
      Just immediate -> do
        v <- Shared.eval -< immediate
        returnA -< Just v
      Nothing -> returnA -< Nothing
    Return e -> case e of
      Just immediate -> do
        v <- Shared.eval -< immediate
        returnA -< Just v
      Nothing -> returnA -< Nothing
    Throw immediate -> do
      v <- Shared.eval -< immediate
      failA -< DynamicException v
    Invoke e -> do
      evalInvoke -< e
      runStatements -< rest
    _ -> runStatements -< rest

createMethodEnv :: (CanFail Val c, CanUseMem Env Val c, CanUseState c) => c [(String, Val)] (PointerEnv Env)
createMethodEnv = proc pairs -> do
  foldA (proc (env, (s,v)) -> do
    addr <- alloc -< v
    extendEnv -< (s, addr, env)) -< (pairs, E.empty)

runMethod :: CanInterp Env Val c => c (Method, Maybe Val, [Expr]) (Maybe Val)
runMethod = proc (method,this,args) -> do
  case methodBody method of
    EmptyBody -> returnA -< Nothing
    FullBody{declarations=decs,statements=stmts} -> do
      argVals <- mapA Shared.eval -< args
      let thisPair = case this of
            Just x -> [("@this", x)]
            Nothing -> []
      let paramPairs = zip (map (\i -> "@parameter" ++ show i) [0..]) argVals
      let decPairs = concatMap (\(t, d) -> zip d (repeat (defaultValue t))) decs
      env <- createMethodEnv -< thisPair ++ paramPairs ++ decPairs
      localEnv (localA runStatements) -< (env, (method, stmts))

runProgram :: CanInterp Env Val c => c [Expr] (Maybe Val)
runProgram = proc args -> do
  (units,_) <- askConst -< ()
  mapA (proc unit -> do
    let getMethod :: Member -> [Method]
        getMethod (MethodMember m) = [m]
        getMethod _ = []
        getMethods members = concatMap getMethod members
    case find (\m -> methodName m == "<clinit>") $ getMethods (fileBody unit) of
      Just classInitMethod -> runMethod -< (classInitMethod, Nothing, [])
      Nothing -> returnA -< Nothing) -< Map.elems units
  mainMethod <- askA -< ()
  tryCatchA runMethod (pi2 >>> proc exception -> case exception of
    DynamicException v -> do
      v' <- unbox -< v
      failA -< DynamicException v'
    StaticException _ -> failA -< exception) -< (mainMethod, Nothing, args)

---- End of Actual Evaluation methods ----
