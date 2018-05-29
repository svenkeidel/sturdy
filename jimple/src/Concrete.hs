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

import           Prelude hiding (id)
import qualified Prelude as P

import           Data.Fixed
import           Data.List (replicate,repeat)
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

---- End of Values ----

---- Interp Type ----

type Constants = (CompilationUnits,Fields)

newtype Interp x y = Interp
  (Except (Exception Val)
    (Reader MethodReader
      (MaybeEnvironment String Addr
        (MaybeStoreArrow Addr Val
          (State Addr
            (Const Constants (->)))))) x y)
  deriving (Category,Arrow,ArrowChoice)

instance ArrowTryCatch (Exception Val) x y Interp where
  tryCatchA (Interp f) (Interp g) = Interp $ tryCatchA f g

deriving instance ArrowConst Constants Interp
deriving instance ArrowFail (Exception Val) Interp
deriving instance ArrowReader MethodReader Interp
deriving instance ArrowState Addr Interp
deriving instance ArrowMaybeEnv String Addr (PointerEnv Env) Interp
deriving instance ArrowMaybeStore Addr Val Interp

runInterp :: Interp x y -> [CompilationUnit] -> [(String,Addr)] -> [(Addr,Val)] -> MethodReader -> x -> Error (Exception Val) y
runInterp (Interp f) files env store mainMethod x =
  let compilationUnits = map (\file -> (fileName file,file)) files
      latestAddr = case map snd env ++ map fst store of
        [] -> 0
        addrs -> maximum addrs
      fields = zip (concatMap (\u -> Shared.getFieldSignatures u (\m -> Static `elem` m)) files) [latestAddr..]
  in runConst (Map.fromList compilationUnits,Map.fromList fields)
      (evalState
        (evalMaybeStore
          (runMaybeEnvironment
            (runReader
              (runExcept f)))))
  (latestAddr + length fields,(S.fromList store,(env,(mainMethod,x))))

---- End of Interp type ----

unbox1 :: (CanFail Val c,CanUseStore Val c) => c Val Val
unbox1 = proc val -> case val of
  RefVal addr -> Shared.readVar -< addr
  _ -> returnA -< val

throw :: (UseVal Val c,CanFail Val c,CanUseStore Val c) => c (String,String) Val
throw = proc (clzz,message) -> do
  (RefVal addr) <- newSimple -< RefType clzz
  v <- Shared.readVar -< addr
  case v of
    ObjectVal c m -> do
      let m' = Map.insert (FieldSignature clzz (RefType "String") "message") (StringVal message) m
      write -< (addr,ObjectVal c m')
      failA -< DynamicException (RefVal addr)
    _ -> failA -< StaticException $ printf "Undefined exception %s" clzz

instance UseVal Val Interp where
  newSimple = proc t -> case t of
    RefType c -> do
      fields <- Shared.getInitializedFields -< c
      addr <- alloc -< (ObjectVal c (Map.fromList fields))
      returnA -< RefVal addr
    _ -> defaultValue -< t
  newArray = proc (t,sizes) -> case sizes of
    (s:sizes') -> case s of
      IntVal s' -> do
        vals <- mapA newArray -< replicate s' (t,sizes')
        addr <- alloc -< ArrayVal vals
        returnA -< RefVal addr
      _ -> failA -< StaticException $ printf "Expected an integer array size, got %s" (show s)
    [] -> defaultValue -< t
  -- and :: c (v,v) v
  -- or :: c (v,v) v
  -- xor :: c (v,v) v
  -- rem = proc (v1,v2) -> case (v1,v2) of
  --   (IntVal x1,IntVal x2) -> returnA -< IntVal (x1 `P.rem` x2)
  --   (LongVal x1,LongVal x2) -> returnA -< LongVal (x1 `P.rem` x2)
  --   (FloatVal x1,FloatVal x2) -> returnA -< FloatVal (x1 `rem'` x2)
  --   (DoubleVal x1,DoubleVal x2) -> returnA -< DoubleVal (x1 `rem'` x2)
  --   _ -> failA -< StaticException "Expected integer variables for rem"
  mod = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1,IntVal x2) -> returnA -< IntVal (x1 `P.mod` x2)
    (LongVal x1,LongVal x2) -> returnA -< LongVal (x1 `P.mod` x2)
    (FloatVal x1,FloatVal x2) -> returnA -< FloatVal (x1 `mod'` x2)
    (DoubleVal x1,DoubleVal x2) -> returnA -< DoubleVal (x1 `mod'` x2)
    _ -> failA -< StaticException "Expected integer variables for mod"
  cmp = proc (v1,v2) -> case (v1,v2) of
    (LongVal x1,LongVal x2)
      | x1 < x2 -> returnA -< IntVal (-1)
      | x1 == x2 -> returnA -< IntVal 0
      | x1 > x2 -> returnA -< IntVal 1
    _ -> failA -< StaticException "Expected long variables for cmp"
  cmpg = proc (v1,v2) -> case (v1,v2) of
    (FloatVal x1,FloatVal x2)
      | x1 < x2 -> returnA -< IntVal (-1)
      | x1 == x2 -> returnA -< IntVal 0
      | x1 > x2 -> returnA -< IntVal 1
    (DoubleVal x1,DoubleVal x2)
      | x1 < x2 -> returnA -< IntVal (-1)
      | x1 == x2 -> returnA -< IntVal 0
      | x1 > x2 -> returnA -< IntVal 1
    _ -> failA -< StaticException "Expected floating variables for cmpg"
  cmpl = proc (v1,v2) -> case (v1,v2) of
    (FloatVal x1,FloatVal x2)
      | x1 > x2 -> returnA -< IntVal (-1)
      | x1 == x2 -> returnA -< IntVal 0
      | x1 < x2 -> returnA -< IntVal 1
    (DoubleVal x1,DoubleVal x2)
      | x1 > x2 -> returnA -< IntVal (-1)
      | x1 == x2 -> returnA -< IntVal 0
      | x1 < x2 -> returnA -< IntVal 1
    _ -> failA -< StaticException "Expected floating variables for cmpl"
  eq = proc (v1,v2) -> returnA -< BoolVal (v1 == v2)
  neq = proc (v1,v2) -> returnA -< BoolVal (v1 /= v2)
  gt = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1,IntVal x2) -> returnA -< BoolVal (x1 > x2)
    (LongVal x1,LongVal x2) -> returnA -< BoolVal (x1 > x2)
    (FloatVal x1,FloatVal x2) -> returnA -< BoolVal (x1 > x2)
    (DoubleVal x1,DoubleVal x2) -> returnA -< BoolVal (x1 > x2)
    _ -> failA -< StaticException "Expected numeric variables for gt"
  ge = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1,IntVal x2) -> returnA -< BoolVal (x1 >= x2)
    (LongVal x1,LongVal x2) -> returnA -< BoolVal (x1 >= x2)
    (FloatVal x1,FloatVal x2) -> returnA -< BoolVal (x1 >= x2)
    (DoubleVal x1,DoubleVal x2) -> returnA -< BoolVal (x1 >= x2)
    _ -> failA -< StaticException "Expected numeric variables for ge"
  lt = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1,IntVal x2) -> returnA -< BoolVal (x1 < x2)
    (LongVal x1,LongVal x2) -> returnA -< BoolVal (x1 < x2)
    (FloatVal x1,FloatVal x2) -> returnA -< BoolVal (x1 < x2)
    (DoubleVal x1,DoubleVal x2) -> returnA -< BoolVal (x1 < x2)
    _ -> failA -< StaticException "Expected numeric variables for lt"
  le = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1,IntVal x2) -> returnA -< BoolVal (x1 <= x2)
    (LongVal x1,LongVal x2) -> returnA -< BoolVal (x1 <= x2)
    (FloatVal x1,FloatVal x2) -> returnA -< BoolVal (x1 <= x2)
    (DoubleVal x1,DoubleVal x2) -> returnA -< BoolVal (x1 <= x2)
    _ -> failA -< StaticException "Expected numeric variables for le"
  -- shl = proc (v1,v2) -> case (v1,v2) of
  --   (IntVal x1,IntVal x2) -> returnA -< x1  x2
  --   (LongVal x1,LongVal x2) -> returnA -< x1  x2
  --   (FloatVal x1,FloatVal x2) -> returnA -< x1  x2
  --   (DoubleVal x1,DoubleVal x2) -> returnA -< x1  x2
  -- shr = proc (v1,v2) -> case (v1,v2) of
  --   (IntVal x1,IntVal x2) -> returnA -< x1  x2
  --   (LongVal x1,LongVal x2) -> returnA -< x1  x2
  --   (FloatVal x1,FloatVal x2) -> returnA -< x1  x2
  --   (DoubleVal x1,DoubleVal x2) -> returnA -< x1  x2
  -- ushr = proc (v1,v2) -> case (v1,v2) of
  --   (IntVal x1,IntVal x2) -> returnA -< x1  x2
  --   (LongVal x1,LongVal x2) -> returnA -< x1  x2
  --   (FloatVal x1,FloatVal x2) -> returnA -< x1  x2
  --   (DoubleVal x1,DoubleVal x2) -> returnA -< x1  x2
  plus = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1,IntVal x2) -> returnA -< IntVal (x1 + x2)
    (LongVal x1,LongVal x2) -> returnA -< LongVal (x1 + x2)
    (FloatVal x1,FloatVal x2) -> returnA -< FloatVal (x1 + x2)
    (DoubleVal x1,DoubleVal x2) -> returnA -< DoubleVal (x1 + x2)
    _ -> failA -< StaticException "Expected numeric variables for plus"
  minus = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1,IntVal x2) -> returnA -< IntVal (x1 - x2)
    (LongVal x1,LongVal x2) -> returnA -< LongVal (x1 - x2)
    (FloatVal x1,FloatVal x2) -> returnA -< FloatVal (x1 - x2)
    (DoubleVal x1,DoubleVal x2) -> returnA -< DoubleVal (x1 - x2)
    _ -> failA -< StaticException "Expected numeric variables for minus"
  mult = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1,IntVal x2) -> returnA -< IntVal (x1 * x2)
    (LongVal x1,LongVal x2) -> returnA -< LongVal (x1 * x2)
    (FloatVal x1,FloatVal x2) -> returnA -< FloatVal (x1 * x2)
    (DoubleVal x1,DoubleVal x2) -> returnA -< DoubleVal (x1 * x2)
    _ -> failA -< StaticException "Expected numeric variables for mult"
  div = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1,IntVal x2) -> if x2 == 0
      then throw -< ("java.lang.ArithmeticException","/ by zero")
      else returnA -< IntVal (x1 `P.div` x2)
    (LongVal x1,LongVal x2) -> if x2 == 0
      then throw -< ("java.lang.ArithmeticException","/ by zero")
      else returnA -< LongVal (x1 `P.div` x2)
    (FloatVal x1,FloatVal x2) -> returnA -< FloatVal (x1 / x2)
    (DoubleVal x1,DoubleVal x2) -> returnA -< DoubleVal (x1 / x2)
    _ -> failA -< StaticException "Expected numeric variables for div"
  lengthOf = proc v -> do
    v' <- unbox -< v
    case v' of
      ArrayVal xs -> returnA -< (IntVal (length xs))
      _ -> failA -< StaticException "Expected an array as argument for lengthof"
  neg = proc v -> case v of
    IntVal n -> returnA -< IntVal (-n)
    LongVal l -> returnA -< LongVal (-l)
    FloatVal f -> returnA -< FloatVal (-f)
    DoubleVal d -> returnA -< DoubleVal (-d)
    _ -> failA -< StaticException "Expected a number as argument for -"
  doubleConstant = arr DoubleVal
  floatConstant = arr FloatVal
  intConstant = arr IntVal
  longConstant = arr LongVal
  nullConstant = arr $ const NullVal
  stringConstant = arr StringVal
  classConstant = arr ClassVal
  unbox = proc val -> case val of
    RefVal addr -> do
      v <- Shared.readVar -< addr
      case v of
        ObjectVal c m -> do
          let (keys,vals) = unzip (Map.toList m)
          vals' <- mapA unbox -< vals
          returnA -< ObjectVal c (Map.fromList (zip keys vals'))
        ArrayVal xs -> do
          xs' <- mapA unbox -< xs
          returnA -< ArrayVal xs'
        _ -> returnA -< v
    _ -> returnA -< val
  defaultValue = arr (\t -> case t of
    BooleanType   -> BoolVal False
    ByteType      -> IntVal 0
    CharType      -> IntVal 0
    ShortType     -> IntVal 0
    IntType       -> IntVal 0
    LongType      -> LongVal 0
    FloatType     -> FloatVal 0.0
    DoubleType    -> DoubleVal 0.0
    NullType      -> NullVal
    (RefType _)   -> NullVal
    (ArrayType _) -> NullVal
    _             -> BottomVal)
  instanceOf = first unbox >>> (proc (v,t) -> case (v,t) of
    (BoolVal _,     BooleanType)  -> returnA -< BoolVal True
    (IntVal n,      ByteType)     -> returnA -< BoolVal $ n >= -128   && n < 128   -- n >= (-2)^7  && n < 2^7
    (IntVal n,      CharType)     -> returnA -< BoolVal $ n >= 0      && n < 65536 -- n >= 0       && n < 2^16
    (IntVal n,      ShortType)    -> returnA -< BoolVal $ n >= -32768 && n < 32768 -- n >= (-2)^15 && n < 2^15
    (IntVal _,      IntType)      -> returnA -< BoolVal True
    (LongVal _,     LongType)     -> returnA -< BoolVal True
    (FloatVal _,    FloatType)    -> returnA -< BoolVal True
    (DoubleVal _,   DoubleType)   -> returnA -< BoolVal True
    (NullVal,       NullType)     -> returnA -< BoolVal True
    (ObjectVal c _, RefType p)    -> isSuperClass -< (c,p)
    (ArrayVal xs,   ArrayType t') -> do
      b <- (mapA instanceOf >>^ all (==BoolVal True)) -< zip xs (repeat t')
      returnA -< BoolVal b
    (_,_) -> returnA -< BoolVal False)
    where
      isSuperClass = proc (c,p) -> if c == p
        then returnA -< BoolVal True
        else do
          unit <- Shared.readCompilationUnit -< c
          case extends unit of
            Just c' -> isSuperClass -< (c',p)
            Nothing -> returnA -< BoolVal False
  cast = proc (v,t,b) -> case b of
    BoolVal True -> do
      v' <- unbox -< v
      case v' of
        ObjectVal _ _ -> returnA -< v
        _ -> failA -< StaticException "Casting of primivites and arrays is not yet supported"
        -- https://docs.oracle.com/javase/specs/jls/se7/html/jls-5.html#jls-5.1.2
    BoolVal False -> throw -< ("java.lang.ClassCastException",printf "Cannot cast %s to type %s" (show v) (show t))
    _ -> failA -< StaticException $ printf "Expected boolean for instanceOf, got %s" (show b)
  readIndex = first unbox1 >>> proc (v,i) -> case (v,i) of
    (ArrayVal xs,IntVal n) -> if n >= 0 && n < length xs
      then returnA -< xs !! n
      else throw -< ("java.lang.ArrayIndexOutOfBoundsException",printf "Index %d out of bounds" (show n))
    (ArrayVal _,_) -> failA -< StaticException $ printf "Expected an integer index for array lookup, got %s" (show i)
    _ -> failA -< StaticException $ printf "Expected an array for index lookup, got %s" (show v)
  updateIndex = first (first (id &&& unbox1)) >>> proc (((ref,a),i),v) -> case (ref,a,i) of
    (RefVal addr,ArrayVal xs,IntVal n) -> if n >= 0 && n < length xs
      then write -< (addr,ArrayVal (replace n v xs))
      else voidA throw -< ("java.lang.ArrayIndexOutOfBoundsException",printf "Index %d out of bounds" (show n))
    (RefVal _,ArrayVal _,_) -> failA -< StaticException $ printf "Expected an integer index for array lookup, got %s" (show i)
    _ -> failA -< StaticException $ printf "Expected an array for index lookup, got %s" (show v)
    where
      replace :: Int -> a -> [a] -> [a]
      replace _ _ [] = []
      replace 0 x (_:t) = x:t
      replace n x (h:t) = h:replace (n-1) x t
  readField = first unbox1 >>> proc (v,f) -> case v of
    (ObjectVal _ m) -> case Map.lookup f m of
      Just x -> returnA -< x
      Nothing -> failA -< StaticException $ printf "Field %s not defined for object %s" (show f) (show v)
    _ -> failA -< StaticException $ printf "Expected an object for field lookup, got %s" (show v)
  updateField = first (id &&& unbox1) >>> proc ((ref,o),(f,v)) -> case (ref,o) of
    (RefVal addr,ObjectVal c m) -> case m Map.!? f of
      Just _ -> write -< (addr,ObjectVal c (Map.insert f v m))
      Nothing -> failA -< StaticException $ printf "FieldSignature %s not defined on object %s" (show f) (show o)
    _ -> failA -< StaticException $ printf "Expected an object for field update, got %s" (show o)

instance UseFlow Val Interp where
  if_ f1 f2 = proc (v,(x,y)) -> case v of
    BoolVal True -> f1 -< x
    BoolVal False -> f2 -< y
    _ -> failA -< StaticException "Expected boolean as argument for 'if'"
  case_ f = proc (v,cases) -> case v of
    IntVal x -> matchCases >>> f -< (x,cases)
    _ -> failA -< StaticException "Expected an integer as argument for switch"
    where
      matchCases = proc (x,cases) -> case cases of
        [] -> failA -< StaticException $ printf "No cases match value %s" (show x)
        ((ConstantCase n,label): rest) -> if n == x
          then returnA -< label
          else matchCases -< (x,rest)
        ((DefaultCase,label): _) -> returnA -< label
  catch f = proc (v,clauses) -> case clauses of
    [] -> failA -< DynamicException v
    (clause:rest) -> do
      b <- instanceOf -< (v,RefType (className clause))
      case b of
        BoolVal True -> f -< (v,clause)
        BoolVal False -> catch f -< (v,rest)
        _ -> failA -< StaticException "Expected a boolean from instanceOf"

instance UseMem Env Interp where
  emptyEnv = arr (\() -> E.empty)

instance UseConst Interp where
  askCompilationUnits = askConst >>^ fst
  askFields = askConst >>^ snd

---- End of Actual Evaluation methods ----
