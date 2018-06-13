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

import           Prelude hiding (id,fail)
import qualified Prelude as P

import           Data.Bits
import qualified Data.Bits as B
import           Data.Fixed
import           Data.Order
import           Data.List (replicate,repeat,find,splitAt)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Tuple (swap)

import           Data.Concrete.Error
import           Data.Concrete.Environment (Env)
import qualified Data.Concrete.Environment as E
import qualified Data.Concrete.Store as S

import           Control.Category hiding ((.))
import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Const
import           Control.Arrow.Environment
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Store
import qualified Control.Arrow.Utils as U
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Concrete.Environment
import           Control.Arrow.Transformer.Concrete.Store

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

instance PreOrd Val where
  IntVal n1 ⊑ IntVal n2 = n1 == n2
  LongVal l1 ⊑ LongVal l2 = l1 == l2
  FloatVal f1 ⊑ FloatVal f2 = f1 == f2
  DoubleVal d1 ⊑ DoubleVal d2 = d1 == d2
  StringVal s1 ⊑ StringVal s2 = s1 == s2
  ClassVal c1 ⊑ ClassVal c2 = c1 == c2
  BoolVal b1 ⊑ BoolVal b2 = b1 == b2
  NullVal ⊑ NullVal = True
  ArrayVal xs ⊑ ArrayVal ys = xs == ys
  ObjectVal c1 m1 ⊑ ObjectVal c2 m2 = c1 == c2 && m1 == m2
  RefVal a ⊑ RefVal b = a == b
  BottomVal ⊑ _ = True
  _ ⊑ _ = False

instance LowerBounded Val where
  bottom = BottomVal

---- End of Values ----

---- Interp Type ----

type Addr = Int
type Constants = (CompilationUnits,Fields)

newtype Interp x y = Interp
  (Except (Exception Val)
    (Reader MethodReader
      (Environment String Addr
        (StoreArrow Addr Val
          (State Addr
            (Const Constants (->)))))) x y)
  deriving (Category,Arrow,ArrowChoice)

deriving instance ArrowConst Constants Interp
deriving instance ArrowReader MethodReader Interp
deriving instance ArrowState Addr Interp
deriving instance ArrowEnv String Addr (Env String Addr) Interp
deriving instance ArrowRead Addr Val Addr Val Interp
deriving instance ArrowRead Addr Val (Exception Val) Val Interp
deriving instance ArrowWrite Addr Val Interp
deriving instance ArrowFail (Exception Val) Interp
deriving instance ArrowExcept x Val (Exception Val) Interp
deriving instance ArrowExcept x (Maybe Val) (Exception Val) Interp

runInterp :: Interp x y -> [CompilationUnit] -> [(String,Addr)] -> [(Addr,Val)] -> MethodReader -> x -> Error (Exception Val) y
runInterp (Interp f) files env store mainMethod x =
  let compilationUnits = map (\file -> (fileName file,file)) files
      latestAddr = case map snd env ++ map fst store of
        [] -> 0
        addrs -> maximum addrs
      fields = zip (concatMap (\u -> Shared.getFieldSignatures u (\m -> Static `elem` m)) files) [latestAddr..]
  in runConst (Map.fromList compilationUnits,Map.fromList fields)
      (evalState
        (evalStore
          (runEnvironment'
            (runReader
              (runExcept f)))))
  (latestAddr + length fields,(S.fromList store,(env,(mainMethod,x))))

---- End of Interp type ----

withInt :: (CanFail Val c) => (Int -> Int -> Int) -> c (Val,Val) Val
withInt op = proc (v1,v2) -> case (v1,v2) of
  (IntVal x1,IntVal x2) -> returnA -< IntVal $ op x1 x2
  (LongVal x1,LongVal x2) -> returnA -< LongVal $ op x1 x2
  _ -> fail -< StaticException "Expected integer variables for op"

withFloat :: (CanFail Val c) => (Float -> Float -> Float) -> c (Val,Val) Val
withFloat op = proc (v1,v2) -> case (v1,v2) of
  (FloatVal x1,FloatVal x2) -> returnA -< FloatVal $ op x1 x2
  (DoubleVal x1,DoubleVal x2) -> returnA -< DoubleVal $ op x1 x2
  _ -> fail -< StaticException "Expected floating variables for op"

cmpNum :: (CanFail Val c) => (Bool -> Bool) -> c (Val,Val) Val
cmpNum post = proc (v1,v2) -> case (v1,v2) of
  (IntVal x1,IntVal x2) -> returnA -< BoolVal $ post (x1 < x2)
  (LongVal x1,LongVal x2) -> returnA -< BoolVal $ post (x1 < x2)
  (FloatVal x1,FloatVal x2) -> returnA -< BoolVal $ post (x1 < x2)
  (DoubleVal x1,DoubleVal x2) -> returnA -< BoolVal $ post (x1 < x2)
  _ -> fail -< StaticException "Expected numeric variables for comparison"

order :: (Ord x,Arrow c) => (Int -> Int) -> c (x,x) Val
order post = arr (IntVal . post . (\(x1,x2) -> case compare x1 x2 of
  LT -> -1
  EQ -> 0
  GT -> 1))

createException :: (UseVal Val c,CanFail Val c,CanUseStore Addr Val c) => c (String,String) (Exception Val)
createException = proc (clzz,message) -> do
  RefVal addr <- newSimple -< RefType clzz
  v <- Shared.read_ -< addr
  case v of
    ObjectVal c m -> do
      let m' = Map.insert (FieldSignature clzz (RefType "String") "message") (StringVal message) m
      write -< (addr,ObjectVal c m')
      returnA -< DynamicException (RefVal addr)
    _ -> returnA -< StaticException $ printf "Undefined exception %s" clzz

instance UseVal Val Interp where
  newSimple = proc t -> case t of
    RefType c -> do
      fields <- Shared.getInitializedFields -< c
      alloc >>^ RefVal -< (ObjectVal c (Map.fromList fields))
    _ -> defaultValue -< t
  newArray = proc (t,sizes) -> case sizes of
    (s:sizes') -> case s of
      IntVal s' -> do
        vals <- U.map newArray -< replicate s' (t,sizes')
        alloc >>^ RefVal -< ArrayVal vals
      _ -> fail -< StaticException $ printf "Expected an integer array size, got %s" (show s)
    [] -> defaultValue -< t
  and = withInt (.&.)
  or = withInt (.|.)
  xor = withInt B.xor
  rem = withInt P.mod <+> withFloat mod'
  cmp = proc (v1,v2) -> case (v1,v2) of
    (LongVal x1,LongVal x2) -> order id -< (x1,x2)
    _ -> fail -< StaticException "Expected long variables for 'cmp'"
  cmpg = proc (v1,v2) -> case (v1,v2) of
    (FloatVal x1,FloatVal x2) -> order id -< (x1,x2)
    (DoubleVal x1,DoubleVal x2) -> order id -< (x1,x2)
    _ -> fail -< StaticException "Expected floating variables for 'cmpg'"
  cmpl = proc (v1,v2) -> case (v1,v2) of
    (FloatVal x1,FloatVal x2) -> order (*(-1)) -< (x1,x2)
    (DoubleVal x1,DoubleVal x2) -> order (*(-1)) -< (x1,x2)
    _ -> fail -< StaticException "Expected floating variables for 'cmpl'"
  eq = arr (BoolVal . uncurry (==))
  neq = arr (BoolVal . uncurry (/=))
  gt = swap ^>> cmpNum id
  ge = cmpNum not
  lt = cmpNum id
  le = swap ^>> cmpNum not
  shl = withInt shiftL
  shr = withInt shiftR
  ushr = withInt shiftR
  plus = withInt (+) <+> withFloat (+)
  minus = withInt (-) <+> withFloat (-)
  mult = withInt (*) <+> withFloat (*)
  div = withFloat (/) <+> proc (v1,v2) -> case (v1,v2) of
    (IntVal x1,IntVal x2) -> div_ >>^ IntVal -< (x1,x2)
    (LongVal x1,LongVal x2) -> div_ >>^ LongVal -< (x1,x2)
    _ -> fail -< StaticException "Expected numeric variables for 'div'"
    where
      div_ = proc (x1,x2) -> if x2 == 0
        then createException >>> fail -< ("java.lang.ArithmeticException","/ by zero")
        else returnA -< (x1 `P.div` x2)
  lengthOf = proc v -> case v of
    ArrayVal xs -> returnA -< (IntVal (length xs))
    _ -> fail -< StaticException "Expected an array variable for 'lengthOf'"
  neg = proc v -> case v of
    IntVal n -> returnA -< IntVal (-n)
    LongVal l -> returnA -< LongVal (-l)
    FloatVal f -> returnA -< FloatVal (-f)
    DoubleVal d -> returnA -< DoubleVal (-d)
    _ -> fail -< StaticException "Expected a number as argument for -"
  doubleConstant = arr DoubleVal
  floatConstant = arr FloatVal
  intConstant = arr IntVal
  longConstant = arr LongVal
  nullConstant = arr $ const NullVal
  stringConstant = arr StringVal
  classConstant = arr ClassVal
  deref = proc val -> case val of
    RefVal addr -> Shared.read_ -< addr
    _ -> returnA -< val
  deepDeref = proc val -> case val of
    RefVal addr -> do
      v <- Shared.read_ -< addr
      case v of
        ObjectVal c m -> do
          let (keys,vals) = unzip (Map.toList m)
          vals' <- U.map deepDeref -< vals
          returnA -< ObjectVal c (Map.fromList (zip keys vals'))
        ArrayVal xs -> U.map deepDeref >>^ ArrayVal -< xs
        _ -> returnA -< v
    _ -> returnA -< val
  defaultValue = proc t -> case t of
    BooleanType   -> returnA -< BoolVal False
    ByteType      -> returnA -< IntVal 0
    CharType      -> returnA -< IntVal 0
    ShortType     -> returnA -< IntVal 0
    IntType       -> returnA -< IntVal 0
    LongType      -> returnA -< LongVal 0
    FloatType     -> returnA -< FloatVal 0.0
    DoubleType    -> returnA -< DoubleVal 0.0
    NullType      -> returnA -< NullVal
    (RefType _)   -> returnA -< NullVal
    (ArrayType _) -> returnA -< NullVal
    _             -> fail -< StaticException $ printf "No default value for type %s" (show t)
  instanceOf = first deepDeref >>> (proc (v,t) -> case (v,t) of
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
      b <- (U.map instanceOf >>^ all (==BoolVal True)) -< zip xs (repeat t')
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
  cast = first (first (id &&& deepDeref)) >>> proc (((v,v'),t),b) -> case (b,v') of
    (BoolVal False,_) -> createException >>> fail -< ("java.lang.ClassCastException",printf "Cannot cast %s to type %s" (show v) (show t))
    (BoolVal True,ObjectVal _ _) -> returnA -< v
    (BoolVal _,_) -> fail -< StaticException "Casting of primivites and arrays is not yet supported"
    -- https://docs.oracle.com/javase/specs/jls/se7/html/jls-5.html#jls-5.1.2
    (_,_) -> fail -< StaticException $ printf "Expected boolean for instanceOf, got %s" (show b)
  readIndex = proc (v,i) -> case (v,i) of
    (ArrayVal xs,IntVal n)
      | n >= 0 && n < length xs -> returnA -< xs !! n
      | otherwise -> createException >>> fail -< ("java.lang.ArrayIndexOutOfBoundsException",printf "Index %d out of bounds" (show n))
    (ArrayVal _,_) -> fail -< StaticException $ printf "Expected an integer index for array lookup, got %s" (show i)
    _ -> fail -< StaticException $ printf "Expected an array for index lookup, got %s" (show v)
  updateIndex = first (first (id &&& deref)) >>> proc (((ref,a),i),v) -> case (ref,a,i) of
    (RefVal addr,ArrayVal xs,IntVal n)
      | n >= 0 && n < length xs -> write -< (addr,ArrayVal ((\(h,(_:t)) -> h ++ v:t) $ splitAt n xs))
      | otherwise -> createException >>> fail -< ("java.lang.ArrayIndexOutOfBoundsException",printf "Index %d out of bounds" (show n))
    (RefVal _,ArrayVal _,_) -> fail -< StaticException $ printf "Expected an integer index for array lookup, got %s" (show i)
    _ -> fail -< StaticException $ printf "Expected an array for index lookup, got %s" (show v)
  readField = proc (v,f) -> case v of
    (ObjectVal _ m) -> case Map.lookup f m of
      Just x -> returnA -< x
      Nothing -> fail -< StaticException $ printf "Field %s not defined for object %s" (show f) (show v)
    _ -> fail -< StaticException $ printf "Expected an object for field lookup, got %s" (show v)
  updateField = first (id &&& deref) >>> proc ((ref,o),(f,v)) -> case (ref,o) of
    (RefVal addr,ObjectVal c m) -> case m Map.!? f of
      Just _ -> write -< (addr,ObjectVal c (Map.insert f v m))
      Nothing -> fail -< StaticException $ printf "FieldSignature %s not defined on object %s" (show f) (show o)
    _ -> fail -< StaticException $ printf "Expected an object for field update, got %s" (show o)

instance UseFlow Val Interp where
  if_ f1 f2 = proc (v,(x,y)) -> case v of
    BoolVal True -> f1 -< x
    BoolVal False -> f2 -< y
    _ -> fail -< StaticException "Expected boolean as argument for 'if'"
  case_ f g = proc (v,cases) -> case v of
    IntVal x -> case find (matchCase x) cases of
      Just (_,label) -> f -< label
      Nothing -> g -< v
    _ -> fail -< StaticException "Expected an integer as argument for switch"
    where
      matchCase x (c,_) = case c of
        ConstantCase n -> x == n
        DefaultCase    -> True
  catch f = proc (v,clauses) -> case clauses of
    [] -> fail -< DynamicException v
    (clause:rest) -> do
      b <- instanceOf -< (v,RefType (className clause))
      case b of
        BoolVal True -> f -< (v,clause)
        BoolVal False -> catch f -< (v,rest)
        _ -> fail -< StaticException "Expected a boolean from instanceOf"

instance UseMem Env Addr Interp where
  emptyEnv = arr $ const E.empty
  addrFromInt = arr id

instance UseConst Interp where
  askCompilationUnits = askConst >>^ fst
  askFields = askConst >>^ snd

---- End of Actual Evaluation methods ----
