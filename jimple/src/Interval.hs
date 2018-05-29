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

import           Prelude hiding (id,lookup,read,Bounded(..),Bool(..),(<),(==),(/))
import qualified Prelude as P

import           Data.Fixed
import           Data.List (elemIndex,find,replicate,repeat)

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
-- TODO Use Text over String

type IV = Interval Int
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
  ArrayVal xs == ArrayVal ys = bool $ all (\(x,y) -> (x == y) P./= B.false) (zip xs ys)
  ObjectVal c1 m1 == ObjectVal c2 m2 = if c1 P.== c2
    then bool $ all (\(x,y) -> (x == y) P./= B.false) (zip (Map.elems m1) (Map.elems m2))
    else B.false
  _ == _ = B.false

instance PreOrd Val where
  _ ⊑ TopVal = P.True
  IntVal n1 ⊑ IntVal n2 = n1 P.== n2
  LongVal l1 ⊑ LongVal l2 = l1 P.== l2
  FloatVal f1 ⊑ FloatVal f2 = f1 P.== f2
  DoubleVal d1 ⊑ DoubleVal d2 = d1 P.== d2
  StringVal s1 ⊑ StringVal s2 = s1 P.== s2
  ClassVal c1 ⊑ ClassVal c2 = c1 P.== c2
  BoolVal b1 ⊑ BoolVal b2 = b1 ⊑ b2
  NullVal ⊑ NullVal = P.True
  ArrayVal xs ⊑ ArrayVal ys = all (\(x,y) -> x ⊑ y) (zip xs ys)
  ObjectVal c1 m1 ⊑ ObjectVal c2 m2 =
    c1 P.== c2 && all (\(x,y) -> x ⊑ y) (zip (Map.elems m1) (Map.elems m2))
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
  ArrayVal xs ⊔ ArrayVal ys = ArrayVal $ xs ++ ys
  ObjectVal c1 m1 ⊔ ObjectVal c2 m2 =
    if c1 P.== c2
      then let
        joinedVals = map (\(x,y) -> x ⊔ y) $ zip (Map.elems m1) (Map.elems m2)
        in ObjectVal c1 (Map.fromList (zip (Map.keys m1) joinedVals))
      else top
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
num b x = (Bounded b (I.Interval x x))

---- End of Values ----

---- Interp Type ----

type Constants = (CompilationUnits, Fields, IV)

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

deriving instance PreOrd y => PreOrd (Interp x y)
deriving instance Complete y => Complete (Interp x y)
deriving instance (PreOrd y, LowerBounded y) => LowerBounded (Interp x y)

instance Complete Int where
  a ⊔ b = max a b

instance LowerBounded Int where
  bottom = -2^29

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

getBounds :: (UseConst c, CanUseConst Constants c) => c () IV
getBounds = askConst >>^ (\(_,_,x) -> x)

mod_ :: Bounded IV -> Bounded IV -> Bounded IV
mod_ (Bounded b1 (I.Interval _ m1)) (Bounded b2 (I.Interval _ m2)) =
  Bounded (b1 ⊔ b2) (I.Interval 0 (min m1 m2))

instance UseVal Val Interp where
  newSimple = proc t -> do
    case t of
      RefType c -> do
        fields <- getInitializedFields -< c
        addr <- alloc -< (ObjectVal c (Map.fromList fields))
        returnA -< RefVal addr
      _ -> defaultValue -< t
  -- newArray = proc (t, sizes) -> case sizes of
  --   (s:sizes') -> do
  --     s' <- toInt -< s
  --     vals <- mapA newArray -< replicate s' (t, sizes')
  --     addr <- alloc -< ArrayVal vals
  --     returnA -< RefVal addr
  --   [] -> defaultValue -< t
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
  -- cmp = proc (v1,v2) -> case (v1,v2) of
  --   (LongVal x1, LongVal x2)
  --     | x1 < x2 -> returnA -< IntVal (-1)
  --     | x1 == x2 -> returnA -< IntVal 0
  --     | x1 > x2 -> returnA -< IntVal 1
  --   _ -> failA -< StaticException "Expected long variables for cmp"
  -- cmpg = proc (v1,v2) -> case (v1,v2) of
  --   (FloatVal x1, FloatVal x2)
  --     | x1 < x2 -> returnA -< IntVal (-1)
  --     | x1 == x2 -> returnA -< IntVal 0
  --     | x1 > x2 -> returnA -< IntVal 1
  --   (DoubleVal x1, DoubleVal x2)
  --     | x1 < x2 -> returnA -< IntVal (-1)
  --     | x1 == x2 -> returnA -< IntVal 0
  --     | x1 > x2 -> returnA -< IntVal 1
  --   _ -> failA -< StaticException "Expected floating variables for cmpg"
  -- cmpl = proc (v1,v2) -> case (v1,v2) of
  --   (FloatVal x1, FloatVal x2)
  --     | x1 > x2 -> returnA -< IntVal (-1)
  --     | x1 == x2 -> returnA -< IntVal 0
  --     | x1 < x2 -> returnA -< IntVal 1
  --   (DoubleVal x1, DoubleVal x2)
  --     | x1 > x2 -> returnA -< IntVal (-1)
  --     | x1 == x2 -> returnA -< IntVal 0
  --     | x1 < x2 -> returnA -< IntVal 1
  --   _ -> failA -< StaticException "Expected floating variables for cmpl"
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
      ArrayVal xs -> intConstant -< length xs
      _ -> failA -< StaticException "Expected an array as argument for lengthof"
  neg = proc v -> case v of
    IntVal n -> returnA -< (IntVal (-n))
    LongVal l -> returnA -< (LongVal (-l))
    FloatVal f -> returnA -< (FloatVal (-f))
    DoubleVal d -> returnA -< (DoubleVal (-d))
    _ -> failA -< StaticException "Expected a number as argument for -"
  doubleConstant = arr (\x -> DoubleVal x)
  floatConstant = arr (\x -> FloatVal x)
  intConstant = proc x -> do
    b <- getBounds -< ()
    returnA -< IntVal (Bounded b (I.Interval x x))
  longConstant = proc x -> do
    b <- getBounds -< ()
    returnA -< LongVal (Bounded b (I.Interval x x))
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
        ArrayVal xs -> mapA unbox >>^ ArrayVal -< xs
        _ -> returnA -< v
    _ -> returnA -< val
  -- defaultValue = arr (\t -> case t of
  --   BooleanType   -> BoolVal B.False
  --   ByteType      -> IntVal 0
  --   CharType      -> IntVal 0
  --   ShortType     -> IntVal 0
  --   IntType       -> IntVal 0
  --   LongType      -> LongVal 0
  --   FloatType     -> FloatVal 0.0
  --   DoubleType    -> DoubleVal 0.0
  --   NullType      -> NullVal
  --   (RefType _)   -> NullVal
  --   (ArrayType _) -> NullVal
  --   _             -> BottomVal)
  -- instanceOf = (first unbox) >>> (proc (v,t) -> case (v,t) of
  --   (BoolVal _,     BooleanType)  -> returnA -< BoolVal True
  --   (IntVal n,      ByteType)     -> returnA -< BoolVal $ n >= -128   && n < 128   -- n >= (-2)^7  && n < 2^7
  --   (IntVal n,      CharType)     -> returnA -< BoolVal $ n >= 0      && n < 65536 -- n >= 0       && n < 2^16
  --   (IntVal n,      ShortType)    -> returnA -< BoolVal $ n >= -32768 && n < 32768 -- n >= (-2)^15 && n < 2^15
  --   (IntVal _,      IntType)      -> returnA -< BoolVal True
  --   (LongVal _,     LongType)     -> returnA -< BoolVal True
  --   (FloatVal _,    FloatType)    -> returnA -< BoolVal True
  --   (DoubleVal _,   DoubleType)   -> returnA -< BoolVal True
  --   (NullVal,       NullType)     -> returnA -< BoolVal True
  --   (ObjectVal c _, RefType p)    -> isSuperClass -< (c, p)
  --   (ArrayVal xs,   ArrayType t') -> do
  --     b <- (mapA instanceOf >>^ all (==BoolVal True)) -< zip xs (repeat t')
  --     returnA -< BoolVal b
  --   (_, _) -> returnA -< BoolVal False)
  -- cast = proc (v,t,b) -> case b of
  --   BoolVal True -> do
  --     v' <- unbox -< v
  --     case v' of
  --       ObjectVal _ _ -> returnA -< v
  --       _ -> failA -< StaticException "Casting of primivites and arrays is not yet supported"
  --       -- https://docs.oracle.com/javase/specs/jls/se7/html/jls-5.html#jls-5.1.2
  --   BoolVal False -> throw -< ("java.lang.ClassCastException", printf "Cannot cast %s to type %s" (show v) (show t))
  --   _ -> failA -< StaticException $ printf "Expected boolean for instanceOf, got %s" (show b)
  -- readIndex = (first unbox1) >>> proc (v,i) -> case (v,i) of
  --   (ArrayVal xs,IntVal n) -> if n >= 0 && n < length xs
  --     then returnA -< xs !! n
  --     else throw -< ("java.lang.ArrayIndexOutOfBoundsException", printf "Index %d out of bounds" (show n))
  --   (ArrayVal _,_) -> failA -< StaticException $ printf "Expected an integer index for array lookup, got %s" (show i)
  --   _ -> failA -< StaticException $ printf "Expected an array for index lookup, got %s" (show v)
  -- updateIndex = (first (first (id &&& unbox1))) >>> proc (((ref,a),i),v) -> case (ref,a,i) of
  --   (RefVal addr,ArrayVal xs,IntVal n) -> if n >= 0 && n < length xs
  --     then write -< (addr, ArrayVal (replace n v xs))
  --     else voidA throw -< ("java.lang.ArrayIndexOutOfBoundsException", printf "Index %d out of bounds" (show n))
  --   (RefVal _,ArrayVal _,_) -> failA -< StaticException $ printf "Expected an integer index for array lookup, got %s" (show i)
  --   _ -> failA -< StaticException $ printf "Expected an array for index lookup, got %s" (show v)
  -- readField = (first unbox1) >>> proc (v,f) -> case v of
  --   (ObjectVal _ m) -> case Map.lookup f m of
  --     Just x -> returnA -< x
  --     Nothing -> failA -< StaticException $ printf "Field %s not defined for object %s" (show f) (show v)
  --   _ -> failA -< StaticException $ printf "Expected an object for field lookup, got %s" (show v)
  -- updateField = (first (id &&& unbox1)) >>> proc ((ref,o),(f,v)) -> case (ref,o) of
  --   (RefVal addr,ObjectVal c m) -> case m Map.!? f of
  --     Just _ -> write -< (addr, ObjectVal c (Map.insert f v m))
  --     Nothing -> failA -< StaticException $ printf "FieldSignature %s not defined on object %s" (show f) (show o)
  --   _ -> failA -< StaticException $ printf "Expected an object for field update, got %s" (show o)

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

instance UseMem Env Interp where
  emptyEnv = arr (\() -> E.empty)

instance UseConst Interp where
  askCompilationUnits = askConst >>^ (\(x,_,_) -> x)
  askFields = askConst >>^ (\(_,x,_) -> x)

---- End of Actual Evaluation methods ----
