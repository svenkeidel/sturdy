{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Interval where

import           Prelude hiding (id,lookup,read,Bounded(..),Bool(..),(<),(>=),(==),(/))
import qualified Prelude as P

import           Data.Fixed

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Abstract.Environment (Env)
import qualified Data.Abstract.Environment as E
import qualified Data.Abstract.Store as S
import qualified Data.Abstract.Boolean as B
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import           Data.Abstract.Bounded hiding (lift)
import           Data.Abstract.Ordering
import           Data.Abstract.Equality
import           Data.Abstract.Error

import           Data.Order
import           Data.Numeric
import qualified Data.Boolean as B

import           Control.Category hiding ((.))

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
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.MaybeEnvironment
import           Control.Arrow.Transformer.Abstract.MaybeStore

import           Text.Printf

import           Syntax
import           Shared

---- Values ----
type IV = Interval Int
type Constants = (CompilationUnits, Fields, IV)

type Addr = Int
-- Remove these instances when abstract multi-env is available.
instance LowerBounded Addr where
  bottom = (-2)^29
instance Complete Addr where
  a ⊔ _ = a

data Val
  = BottomVal
  | TopVal
  | IntVal (Bounded IV)
  | LongVal (Bounded IV)
  | FloatVal Float
  | DoubleVal Float
  | StringVal String
  | ClassVal String
  | BoolVal B.Bool
  | NullVal
  | RefVal Addr
  | ArrayVal Val Val
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
  show (ArrayVal x s) = show x ++ "[" ++ show s ++ "]"
  show (ObjectVal c m) = show c ++ "{" ++ show m ++ "}"

instance Equality Val where
  TopVal == TopVal = B.true
  IntVal n1 == IntVal n2 = n1 == n2
  LongVal l1 == LongVal l2 = l1 == l2
  FloatVal f1 == FloatVal f2 = bool $ f1 P.== f2
  DoubleVal d1 == DoubleVal d2 = bool $ d1 P.== d2
  StringVal s1 == StringVal s2 = bool $ s1 P.== s2
  ClassVal c1 == ClassVal c2 = bool $ c1 P.== c2
  BoolVal b1 == BoolVal b2 = b1 == b2
  NullVal == NullVal = B.true
  ArrayVal x1 s1 == ArrayVal x2 s2 = (x1 == x2) `B.and` (s1 == s2)
  ObjectVal c1 m1 == ObjectVal c2 m2 = if c1 P.== c2
    then bool $ all (\(x,y) -> (x == y) P./= B.false) (zip (Map.elems m1) (Map.elems m2))
    else B.false
  _ == _ = B.false

instance PreOrd Val where
  _ ⊑ TopVal = P.True
  IntVal n1 ⊑ IntVal n2 = n1 ⊑ n2
  LongVal l1 ⊑ LongVal l2 = l1 ⊑ l2
  FloatVal f1 ⊑ FloatVal f2 = f1 P.== f2
  DoubleVal d1 ⊑ DoubleVal d2 = d1 P.== d2
  StringVal s1 ⊑ StringVal s2 = s1 P.== s2
  ClassVal c1 ⊑ ClassVal c2 = c1 P.== c2
  BoolVal b1 ⊑ BoolVal b2 = b1 ⊑ b2
  NullVal ⊑ NullVal = P.True
  ArrayVal x1 s1 ⊑ ArrayVal x2 s2 = (x1 ⊑ x2) && (s1 ⊑ s2)
  ObjectVal c1 m1 ⊑ ObjectVal c2 m2 =
    c1 P.== c2 && all (\(x,y) -> x ⊑ y) (zip (Map.elems m1) (Map.elems m2))
  RefVal a ⊑ RefVal b = a P.== b
  _ ⊑ _ = P.False

instance Complete Val where
  IntVal n1 ⊔ IntVal n2 = IntVal $ n1 ⊔ n2
  LongVal l1 ⊔ LongVal l2 = LongVal $ l1 ⊔ l2
  FloatVal f1 ⊔ FloatVal f2 = if f1 P.== f2 then FloatVal f1 else top
  DoubleVal d1 ⊔ DoubleVal d2 = if d1 P.== d2 then DoubleVal d1 else top
  StringVal s1 ⊔ StringVal s2 = if s1 P.== s2 then StringVal s1 else top
  ClassVal c1 ⊔ ClassVal c2 = if c1 P.== c2 then ClassVal c1 else ClassVal "java.lang.Object"
  BoolVal b1 ⊔ BoolVal b2 = BoolVal $ b1 ⊔ b2
  NullVal ⊔ NullVal = NullVal
  ArrayVal x1 s1 ⊔ ArrayVal x2 s2 = ArrayVal (x1 ⊔ x2) (s1 ⊔ s2)
  ObjectVal c1 m1 ⊔ ObjectVal c2 m2 = if c1 P.== c2
    then ObjectVal c1 $ Map.unionWith (⊔) m1 m2
    else top
  RefVal a ⊔ RefVal b = if a P.== b then RefVal a else top
  _ ⊔ _ = top

instance UpperBounded Val where
  top = TopVal

instance LowerBounded Val where
  bottom = BottomVal

bool :: P.Bool -> B.Bool
bool P.True = B.True
bool P.False = B.False

boolVal :: P.Bool -> Val
boolVal b = BoolVal (bool b)

num :: IV -> Int -> Bounded IV
num b x = Bounded b $ I.constant x

range :: IV -> Int -> Int -> Bounded IV
range b x y = Bounded b $ I.Interval x y

---- End of Values ----

---- Interp Type ----

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
deriving instance ArrowMaybeEnv String Addr (Env String Addr) Interp
deriving instance ArrowMaybeStore Addr Val Interp

deriving instance PreOrd y => PreOrd (Interp x y)
deriving instance (Complete y) => Complete (Interp x y)
deriving instance LowerBounded y => LowerBounded (Interp x y)

---- End of Interp type ----

---- Program Boilerplate ----

runInterp :: (?bound :: IV) => Interp x y -> [CompilationUnit] -> [(String, Addr)] -> [(Addr, Val)] -> MethodReader -> x -> Error (Exception Val) y
runInterp (Interp f) files env store mainMethod x =
  let compilationUnits = map (\file -> (fileName file, file)) files
      latestAddr = case map snd env ++ map fst store of
        [] -> 0
        addrs -> maximum addrs
      fields = zip (concatMap (\u -> Shared.getFieldSignatures u (\m -> Static `elem` m)) files) [latestAddr..]
  in runConst (Map.fromList compilationUnits, Map.fromList fields, ?bound)
      (evalState
        (evalMaybeStore
          (runMaybeEnvironment
            (runReader
              (runExcept f)))))
  (latestAddr + length fields, (S.fromList store, (env, (mainMethod, x))))

---- End of Program Boilerplate ----

askBounds :: (CanUseConst Constants c) => c () IV
askBounds = askConst >>^ (\(_,_,x) -> x)

mod_ :: Bounded IV -> Bounded IV -> Bounded IV
mod_ (Bounded b1 (I.Interval _ m1)) (Bounded b2 (I.Interval _ m2)) =
  Bounded (b1 ⊔ b2) (I.Interval 0 (min m1 m2))

boolGLB :: (CanFail Val c) => c [Val] Val
boolGLB = proc xs -> case xs of
  [] -> returnA -< BoolVal B.True
  (x:rest) -> do
    ys <- boolGLB -< rest
    case (x,ys) of
      (BoolVal B.True,y) -> returnA -< y
      (BoolVal B.Top,BoolVal B.True) -> returnA -< BoolVal B.Top
      (BoolVal B.Top,y) -> returnA -< y
      (BoolVal B.False,_) -> returnA -< BoolVal B.False
      (TopVal,_) -> returnA -< TopVal
      (_,_) -> failA -< StaticException "Expected a boolean value from instanceOf"

unbox1 :: (CanFail Val c,CanUseStore Addr Val c) => c Val Val
unbox1 = proc val -> case val of
  RefVal addr -> Shared.readVar -< addr
  _ -> returnA -< val

throw :: (UseVal Val c,CanFail Val c,CanUseStore Addr Val c) => c (String,String) Val
throw = proc (clzz,message) -> do
  RefVal addr <- newSimple -< RefType clzz
  v <- Shared.readVar -< addr
  case v of
    ObjectVal c m -> do
      let m' = Map.insert (FieldSignature clzz (RefType "String") "message") (StringVal message) m
      write -< (addr,ObjectVal c m')
      failA -< DynamicException (RefVal addr)
    _ -> failA -< StaticException $ printf "Undefined exception %s" clzz

instance UseVal Val Interp where
  newSimple = proc t -> do
    case t of
      RefType c -> do
        fields <- getInitializedFields -< c
        addr <- alloc -< (ObjectVal c (Map.fromList fields))
        returnA -< RefVal addr
      _ -> defaultValue -< t
  newArray = proc (t, sizes) -> case sizes of
    (s:rest) -> case s of
      IntVal _ -> do
        val <- newArray -< (t,rest)
        alloc >>^ RefVal -< ArrayVal val s
      TopVal -> returnA -< top
      _ -> failA -< StaticException $ printf "Expected an integer array size, got %s" (show s)
    [] -> defaultValue -< t
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
    (IntVal x1, IntVal x2) -> returnA -< IntVal (x1 `mod_` x2)
    (LongVal x1, LongVal x2) -> returnA -< LongVal (x1 `mod_` x2)
    (FloatVal x1, FloatVal x2) -> returnA -< FloatVal (x1 `mod'` x2)
    (DoubleVal x1, DoubleVal x2) -> returnA -< DoubleVal (x1 `mod'` x2)
    _ -> failA -< StaticException "Expected integer variables for mod"
  cmp = proc (v1,v2) -> case (v1,v2) of
    (LongVal x1, LongVal x2) -> do
      b <- askBounds -< ()
      let nums = map (num b) [-1,0,1]
      let pairs = zip [x1 < x2,x1 == x2,x2 < x1] nums
      let filteredNums = map snd $ filter ((P./=B.False) . fst) pairs
      returnA -< IntVal $ lub filteredNums
    _ -> failA -< StaticException "Expected long variables for cmp"
  cmpg = proc (v1,v2) -> case (v1,v2) of
    (FloatVal x1, FloatVal x2)
      | x1 P.< x2 -> intConstant -< -1
      | x1 P.== x2 -> intConstant -< 0
      | x1 > x2 -> intConstant -< 1
    (DoubleVal x1, DoubleVal x2)
      | x1 P.< x2 -> intConstant -< -1
      | x1 P.== x2 -> intConstant -< 0
      | x1 > x2 -> intConstant -< 1
    _ -> failA -< StaticException "Expected floating variables for cmpg"
  cmpl = proc (v1,v2) -> case (v1,v2) of
    (FloatVal x1, FloatVal x2)
      | x1 > x2 -> intConstant -< -1
      | x1 P.== x2 -> intConstant -< 0
      | x1 P.< x2 -> intConstant -< 1
    (DoubleVal x1, DoubleVal x2)
      | x1 > x2 -> intConstant -< -1
      | x1 P.== x2 -> intConstant -< 0
      | x1 P.< x2 -> intConstant -< 1
    _ -> failA -< StaticException "Expected floating variables for cmpl"
  eq = proc (v1, v2) -> returnA -< BoolVal (v1 == v2)
  neq = proc (v1, v2) -> returnA -< BoolVal $ B.not (v1 == v2)
  gt = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1, IntVal x2) -> returnA -< BoolVal (x2 < x1)
    (LongVal x1, LongVal x2) -> returnA -< BoolVal (x2 < x1)
    (FloatVal x1, FloatVal x2) -> returnA -< boolVal (x2 P.< x1)
    (DoubleVal x1, DoubleVal x2) -> returnA -< boolVal (x2 P.< x1)
    _ -> failA -< StaticException "Expected numeric variables for gt"
  ge = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1, IntVal x2) -> returnA -< BoolVal $ B.not (x1 < x2)
    (LongVal x1, LongVal x2) -> returnA -< BoolVal $ B.not (x1 < x2)
    (FloatVal x1, FloatVal x2) -> returnA -< boolVal $ not (x1 P.< x2)
    (DoubleVal x1, DoubleVal x2) -> returnA -< boolVal $ not (x1 P.< x2)
    _ -> failA -< StaticException "Expected numeric variables for ge"
  lt = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1, IntVal x2) -> returnA -< BoolVal (x1 < x2)
    (LongVal x1, LongVal x2) -> returnA -< BoolVal (x1 < x2)
    (FloatVal x1, FloatVal x2) -> returnA -< boolVal (x1 P.< x2)
    (DoubleVal x1, DoubleVal x2) -> returnA -< boolVal (x1 P.< x2)
    _ -> failA -< StaticException "Expected numeric variables for lt"
  le = proc (v1,v2) -> case (v1,v2) of
    (IntVal x1, IntVal x2) -> returnA -< BoolVal $ B.not (x2 < x1)
    (LongVal x1, LongVal x2) -> returnA -< BoolVal $ B.not (x2 < x1)
    (FloatVal x1, FloatVal x2) -> returnA -< boolVal $ not (x2 P.< x1)
    (DoubleVal x1, DoubleVal x2) -> returnA -< boolVal $ not (x2 P.< x1)
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
    -- (IntVal x1, IntVal x2) -> if x2 == 0
    --   then throw -< ("java.lang.ArithmeticException", "/ by zero")
    --   else returnA -< IntVal (x1 `P.div` x2)
    -- (LongVal x1, LongVal x2) -> if x2 == 0
    --   then throw -< ("java.lang.ArithmeticException", "/ by zero")
    --   else returnA -< LongVal (x1 `P.div` x2)
    (FloatVal x1, FloatVal x2) -> returnA -< FloatVal (x1 P./ x2)
    (DoubleVal x1, DoubleVal x2) -> returnA -< DoubleVal (x1 P./ x2)
    _ -> failA -< StaticException "Expected numeric variables for div"
  lengthOf = proc v -> do
    v' <- unbox -< v
    case v' of
      ArrayVal _ s -> returnA -< s
      _ -> failA -< StaticException "Expected an array as argument for lengthof"
  neg = proc v -> case v of
    IntVal n -> returnA -< IntVal (-n)
    LongVal l -> returnA -< LongVal (-l)
    FloatVal f -> returnA -< FloatVal (-f)
    DoubleVal d -> returnA -< DoubleVal (-d)
    _ -> failA -< StaticException "Expected a number as argument for -"
  doubleConstant = arr (\x -> DoubleVal x)
  floatConstant = arr (\x -> FloatVal x)
  intConstant = proc x -> do
    b <- askBounds -< ()
    returnA -< IntVal $ num b x
  longConstant = proc x -> do
    b <- askBounds -< ()
    returnA -< LongVal $ num b x
  nullConstant = arr (\() -> NullVal)
  stringConstant = arr (\x -> StringVal x)
  classConstant = arr (\x -> ClassVal x)
  unbox = proc val -> case val of
    RefVal addr -> do
      v <- Shared.readVar -< addr
      case v of
        ObjectVal c m -> do
          let (keys, vals) = unzip (Map.toList m)
          vals' <- mapA unbox -< vals
          returnA -< ObjectVal c (Map.fromList (zip keys vals'))
        ArrayVal x s -> do
          x' <- unbox -< x
          returnA -< ArrayVal x' s
        _ -> returnA -< v
    _ -> returnA -< val
  defaultValue = proc t -> do
    b <- askBounds -< ()
    returnA -< case t of
      BooleanType   -> BoolVal B.False
      ByteType      -> IntVal $ num b 0
      CharType      -> IntVal $ num b 0
      ShortType     -> IntVal $ num b 0
      IntType       -> IntVal $ num b 0
      LongType      -> LongVal $ num b 0
      FloatType     -> FloatVal 0.0
      DoubleType    -> DoubleVal 0.0
      NullType      -> NullVal
      (RefType _)   -> NullVal
      (ArrayType _) -> NullVal
      _             -> BottomVal
  instanceOf = (first unbox) >>> (proc (v,t) -> case (v,t) of
    (TopVal, _) -> returnA -< BoolVal B.Top
    (BoolVal _,     BooleanType)  -> returnA -< BoolVal B.True
    (IntVal n,      ByteType)     -> do
      b <- askBounds -< ()
      returnA -< boolVal $ n ⊑ range b (-128) 127     -- n >= (-2)^7  && n < 2^7
    (IntVal n,      CharType)     -> do
      b <- askBounds -< ()
      returnA -< boolVal $ n ⊑ range b 0 65535        -- n >= 0       && n < 2^16
    (IntVal n,      ShortType)    -> do
      b <- askBounds -< ()
      returnA -< boolVal $ n ⊑ range b (-32768) 32767 -- n >= (-2)^15 && n < 2^15
    (IntVal _,      IntType)      -> returnA -< BoolVal B.True
    (LongVal _,     LongType)     -> returnA -< BoolVal B.True
    (FloatVal _,    FloatType)    -> returnA -< BoolVal B.True
    (DoubleVal _,   DoubleType)   -> returnA -< BoolVal B.True
    (NullVal,       NullType)     -> returnA -< BoolVal B.True
    (ObjectVal c _, RefType p)    -> isSuperClass -< (c, p)
    (ArrayVal x _,  ArrayType t') -> instanceOf -< (x,t')
    (_, _) -> returnA -< BoolVal B.False)
    where
      isSuperClass = proc (c,p) -> if c P.== p
        then returnA -< BoolVal B.True
        else do
          unit <- Shared.readCompilationUnit -< c
          case extends unit of
            Just c' -> isSuperClass -< (c',p)
            Nothing -> returnA -< BoolVal B.False
  cast = first (first (id &&& unbox)) >>> proc (((v,v'),t),b) -> case (b,v') of
    (BoolVal B.False,_) -> throw -< ("java.lang.ClassCastException", printf "Cannot cast %s to type %s" (show v) (show t))
    (BoolVal B.Top,ObjectVal _ _) -> joined returnA throw -< (v,("java.lang.ClassCastException",printf "Cannot cast %s to type %s" (show v) (show t)))
    (BoolVal B.True,ObjectVal _ _) -> returnA -< v
    (BoolVal _,_) -> failA -< StaticException "Casting of primivites and arrays is not yet supported"
    -- https://docs.oracle.com/javase/specs/jls/se7/html/jls-5.html#jls-5.1.2
    (TopVal,_) -> returnA -< TopVal
    (_,_) -> failA -< StaticException $ printf "Expected boolean for instanceOf, got %s" (show b)
  readIndex = (first unbox1) >>> proc (v,i) -> case (v,i) of
    (TopVal,_) -> returnA -< TopVal
    (_,TopVal) -> returnA -< TopVal
    (ArrayVal x (IntVal s),IntVal n) -> do
      b <- askBounds -< ()
      if n ⊑ ((num b 0) ⊔ s)
      then returnA -< x
      else throw -< ("java.lang.ArrayIndexOutOfBoundsException", printf "Index %d out of bounds" (show n))
    (ArrayVal _ _,_) -> failA -< StaticException $ printf "Expected an integer index for array lookup, got %s" (show i)
    _ -> failA -< StaticException $ printf "Expected an array for index lookup, got %s" (show v)
  updateIndex = (first (first (id &&& unbox1))) >>> proc (((ref,a),i),v) -> case (ref,a,i) of
    (RefVal addr,ArrayVal x (IntVal s),IntVal n) -> do
      b <- askBounds -< ()
      if n ⊑ ((num b 0) ⊔ s)
      then write -< (addr,ArrayVal (x ⊔ v) (IntVal s))
      else voidA throw -< ("java.lang.ArrayIndexOutOfBoundsException", printf "Index %d out of bounds" (show n))
    (RefVal _,ArrayVal _ _,_) -> failA -< StaticException $ printf "Expected an integer index for array lookup, got %s" (show i)
    (RefVal _,TopVal,_) -> returnA -< () -- TopVal is already written
    _ -> failA -< StaticException $ printf "Expected an array for index lookup, got %s" (show v)
  readField = (first unbox1) >>> proc (v,f) -> case v of
    (ObjectVal _ m) -> case Map.lookup f m of
      Just x -> returnA -< x
      Nothing -> failA -< StaticException $ printf "Field %s not defined for object %s" (show f) (show v)
    _ -> failA -< StaticException $ printf "Expected an object for field lookup, got %s" (show v)
  updateField = (first (id &&& unbox1)) >>> proc ((ref,o),(f,v)) -> case (ref,o) of
    (RefVal addr,ObjectVal c m) -> case m Map.!? f of
      Just _ -> write -< (addr, ObjectVal c (Map.insert f v m))
      Nothing -> failA -< StaticException $ printf "FieldSignature %s not defined on object %s" (show f) (show o)
    _ -> failA -< StaticException $ printf "Expected an object for field update, got %s" (show o)

instance UseFlow Val Interp where
  if_ f1 f2 = proc (v,(x,y)) -> case v of
    BoolVal B.True -> f1 -< x
    BoolVal B.False -> f2 -< y
    BoolVal B.Top -> joined f1 f2 -< (x,y)
    TopVal -> returnA -< Just top
    _ -> failA -< StaticException "Expected boolean as argument for 'if'"
  case_ f = proc (v,cases) -> case v of
    IntVal _ -> matchCases >>> (lubA f) -< (v,cases)
    TopVal -> returnA -< Just top
    _ -> failA -< StaticException "Expected integer argument for 'case'"
    where
      matchCases = proc (v,cases) -> case cases of
        [] -> failA -< StaticException $ printf "No cases match value %s" (show v)
        ((ConstantCase n, label): rest) -> do
          n' <- intConstant -< n
          b <- eq -< (n', v)
          case b of
            BoolVal B.True -> returnA -< [label]
            BoolVal B.False -> matchCases -< (v,rest)
            BoolVal B.Top -> do
              labels <- matchCases -< (v,rest)
              returnA -< label:labels
            TopVal -> do
              labels <- matchCases -< (v,rest)
              returnA -< label:labels
            _ -> failA -< StaticException "Expected boolean from eq"
        ((DefaultCase, label): _) -> returnA -< [label]
  catch f = proc (v,clauses) -> case clauses of
    [] -> failA -< DynamicException v
    (clause:rest) -> do
      b <- instanceOf -< (v, RefType (className clause))
      case b of
        BoolVal B.True -> f -< (v,clause)
        BoolVal B.False -> catch f -< (v,rest)
        BoolVal B.Top -> joined f (catch f) -< ((v,clause),(v,rest))
        TopVal -> returnA -< Just top
        _ -> failA -< StaticException "Expected a boolean from instanceOf"

instance UseMem Env Addr Interp where
  emptyEnv = arr $ const E.empty
  addrFromInt = arr id

instance UseConst Interp where
  askCompilationUnits = askConst >>^ (\(x,_,_) -> x)
  askFields = askConst >>^ (\(_,x,_) -> x)

---- End of Actual Evaluation methods ----
