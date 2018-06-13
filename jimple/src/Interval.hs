{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Interval where

import           Prelude hiding (id,fail,Bounded(..),Bool(..),(<),(==),(/))
import qualified Prelude as P

import           Data.Bits
import qualified Data.Bits as B
import           Data.Tuple (swap)
import           Data.Fixed
import           Data.List (union)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Abstract.Environment (Env)
import qualified Data.Abstract.Environment as E
import qualified Data.Abstract.Store as S
import qualified Data.Abstract.Boolean as B
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import           Data.Abstract.Bounded
import           Data.Abstract.Ordering
import           Data.Abstract.Equality
import           Data.Abstract.HandleError
import qualified Data.Abstract.PropagateError as PE

import qualified Data.Concrete.Powerset as C

import           Data.Order
import           Data.Numeric
import qualified Data.Boolean as B
import           Data.GaloisConnection

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
import           Control.Arrow.Abstract.Join
import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Abstract.HandleExcept
import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Store

import           Text.Printf

import           Syntax
import           Shared
import qualified Concrete

---- Values ----
type IV = Interval Int
type Constants = (CompilationUnits,Fields,IV)

type Addr = Int
-- Remove these instances when abstract multi-env is available.
instance LowerBounded Addr where
  bottom = (-2)^(29 :: Int)
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
  NullVal == NullVal = B.true
  ArrayVal x1 s1 == ArrayVal x2 s2 = (x1 == x2) `B.and` (s1 == s2)
  ObjectVal c1 m1 == ObjectVal c2 m2 = if c1 P.== c2
    then bool $ all (\(x,y) -> (x == y) P./= B.false) (zip (Map.elems m1) (Map.elems m2))
    else B.false
  _ == _ = B.false

instance PreOrd Val where
  BottomVal ⊑ _ = P.True
  _ ⊑ TopVal = P.True
  IntVal n1 ⊑ IntVal n2 = n1 ⊑ n2
  LongVal l1 ⊑ LongVal l2 = l1 ⊑ l2
  FloatVal f1 ⊑ FloatVal f2 = f1 P.== f2
  DoubleVal d1 ⊑ DoubleVal d2 = d1 P.== d2
  StringVal s1 ⊑ StringVal s2 = s1 P.== s2
  ClassVal c1 ⊑ ClassVal c2 = c1 P.== c2
  NullVal ⊑ NullVal = P.True
  ArrayVal x1 s1 ⊑ ArrayVal x2 s2 = (x1 ⊑ x2) && (s1 ⊑ s2)
  ObjectVal c1 m1 ⊑ ObjectVal c2 m2 =
    c1 P.== c2 && all (uncurry (⊑)) (zip (Map.elems m1) (Map.elems m2))
  RefVal a ⊑ RefVal b = a P.== b
  _ ⊑ _ = P.False

instance Complete Val where
  BottomVal ⊔ v = v
  v ⊔ BottomVal = v
  IntVal n1 ⊔ IntVal n2 = IntVal $ n1 ⊔ n2
  LongVal l1 ⊔ LongVal l2 = LongVal $ l1 ⊔ l2
  FloatVal f1 ⊔ FloatVal f2 = if f1 P.== f2 then FloatVal f1 else FloatVal $ max f1 f2 -- top
  DoubleVal d1 ⊔ DoubleVal d2 = if d1 P.== d2 then DoubleVal d1 else top
  StringVal s1 ⊔ StringVal s2 = if s1 P.== s2 then StringVal s1 else top
  ClassVal c1 ⊔ ClassVal c2 = if c1 P.== c2 then ClassVal c1 else ClassVal "java.lang.Object"
  NullVal ⊔ NullVal = NullVal
  ArrayVal x s ⊔ NullVal = ArrayVal x s
  NullVal ⊔ ArrayVal x s = ArrayVal x s
  ObjectVal c m ⊔ NullVal = ObjectVal c m
  NullVal ⊔ ObjectVal c m = ObjectVal c m
  RefVal a ⊔ NullVal = RefVal a
  NullVal ⊔ RefVal a = RefVal a
  ArrayVal x1 s1 ⊔ ArrayVal x2 s2 = ArrayVal (x1 ⊔ x2) (s1 ⊔ s2)
  ObjectVal c1 m1 ⊔ ObjectVal c2 m2 = if c1 P.== c2
    then ObjectVal c1 $ Map.unionWith (⊔) m1 m2
    else top
  RefVal a ⊔ RefVal b = if a P.== b then RefVal a else RefVal $ max a b -- top
  _ ⊔ _ = top

instance UpperBounded Val where
  top = TopVal

instance LowerBounded Val where
  bottom = BottomVal

-- instance Galois (IV -> C.Pow Concrete.Val) (IV -> Val) where
--   alpha x = \b -> let ?bound = b in lifted lift (x b)
--     where lift (Concrete.BoolVal b) = BoolVal (alpha b)
--           lift (Concrete.IntVal n) = IntVal $ bounded (I.Interval n n)
--   gamma x b = case x b of
--     BoolVal y -> Concrete.BoolVal <$> gamma y
--     IntVal (Bounded _ y) -> Concrete.IntVal <$> gamma y
--     TopVal ->
--       let bools = gamma (\(_::IV) -> BoolVal B.Top) b
--           ints = gamma (\(_::IV) -> (IntVal (Bounded b top))) b
--       in bools `union` ints

-- alpha x ⊑ y iff x ⊑ gamma y
--   alpha: concrete -> abstract
--   gamma: abstract -> concrete

-- instance Galois (IV -> Pow Concrete.Val) (IV -> Val) where
--   alpha x = \b -> let ?bound = b in lifted lift (x b)
--     where lift (Concrete.BoolVal b) = BoolVal (alphaSing b)
--           lift (Concrete.NumVal n) = NumVal $ bounded (I.Interval n n)
--   gamma x b = case x b of
--     BoolVal y -> Concrete.BoolVal <$> gamma y
--     NumVal (Bounded _ y) -> Concrete.NumVal <$> gamma y
--     Top -> gamma (\(_::IV) -> BoolVal B.Top) b `union` gamma (\(_::IV) -> (NumVal (Bounded b top))) b


---- End of Values ----

---- Interp Type ----

-- instance ArrowRead Addr Val x y (StoreArrow Addr Val Interp) where
--   read (StoreArrow f) (StoreArrow g) = StoreArrow $ proc (var,x) -> do
--     s <- get -< ()
--     case S.lookup var s of
--       Just val -> f -< (val,x)
--       Nothing  -> g -< x
--
-- instance {-# OVERLAPS #-} ArrowWrite Addr Val (StoreArrow Addr Val Interp) where
--   write = StoreArrow $ modify $ arr $ \((var,val),st) -> S.insert var val st

newtype Interp x y = Interp
  (Except (Exception Val)
    (Reader MethodReader
      (Environment String Addr
        (StoreArrow Addr Val
          (State Addr
            (Const Constants (->)))))) x y)
  deriving (Category,Arrow,ArrowChoice)

deriving instance ArrowJoin Interp
deriving instance ArrowConst Constants Interp
deriving instance ArrowFail (Exception Val) Interp
deriving instance ArrowReader MethodReader Interp
deriving instance ArrowState Addr Interp
deriving instance ArrowEnv String Addr (Env String Addr) Interp
deriving instance ArrowRead Addr Val Addr Val Interp
deriving instance ArrowRead Addr Val (Exception Val) Val Interp
deriving instance ArrowWrite Addr Val Interp
deriving instance ArrowExcept x Val (Exception Val) Interp
deriving instance ArrowExcept x (Maybe Val) (Exception Val) Interp

instance (LowerBounded e, LowerBounded a) => LowerBounded (Error e a) where
  bottom = SuccessOrFail bottom bottom

deriving instance PreOrd y => PreOrd (Interp x y)
deriving instance (Complete y) => Complete (Interp x y)
deriving instance LowerBounded y => LowerBounded (Interp x y)

---- End of Interp type ----

---- Program Boilerplate ----

runInterp :: (?bound :: IV) => Interp x y -> [CompilationUnit] -> [(String,Addr)] -> [(Addr,Val)] -> MethodReader -> x -> Error (Exception Val) y
runInterp (Interp f) files env store mainMethod x =
  let compilationUnits = map (\file -> (fileName file,file)) files
      latestAddr = case map snd env ++ map fst store of
        [] -> 0
        addrs -> maximum addrs
      fields = zip (concatMap (\u -> Shared.getFieldSignatures u (\m -> Static `elem` m)) files) [latestAddr..]
  in runConst (Map.fromList compilationUnits,Map.fromList fields,?bound)
      (evalState
        (evalStore
          (runEnvironment'
            (runReader
              (runExcept f)))))
  (latestAddr + length fields,(S.fromList store,(env,(mainMethod,x))))

---- End of Program Boilerplate ----

bool :: P.Bool -> B.Bool
bool P.True = B.True
bool P.False = B.False

num :: IV -> Int -> Bounded IV
num b x = Bounded b $ I.constant x

range :: IV -> Int -> Int -> Bounded IV
range b x y = Bounded b $ I.Interval x y

askBounds :: (CanUseConst Constants c) => c () IV
askBounds = askConst >>^ (\(_,_,x) -> x)

withInt :: (CanFail Val c) => (Int -> Int -> Int) -> c (Val,Val) Val
withInt op = proc (v1,v2) -> case (v1,v2) of
  (TopVal,_) -> returnA -< top
  (_,TopVal) -> returnA -< top
  (IntVal x1,IntVal x2) -> returnA -< IntVal $ lift2 (I.withBounds2 op) x1 x2
  (LongVal x1,LongVal x2) -> returnA -< LongVal $ lift2 (I.withBounds2 op) x1 x2
  _ -> fail -< StaticException $ printf "Expected integer variables for op, got %s %s" (show v1) (show v2)

withFloat :: (CanFail Val c) => (Float -> Float -> Float) -> c (Val,Val) Val
withFloat op = proc (v1,v2) -> case (v1,v2) of
  (TopVal,_) -> returnA -< top
  (_,TopVal) -> returnA -< top
  (FloatVal x1,FloatVal x2) -> returnA -< FloatVal $ op x1 x2
  (DoubleVal x1,DoubleVal x2) -> returnA -< DoubleVal $ op x1 x2
  _ -> fail -< StaticException $ printf "Expected floating variables for op, got %s %s" (show v1) (show v2)

order :: (Ord x,Arrow c) => (Int -> Int) -> c (IV,x,x) Val
order post = arr (\(b,x1,x2) -> IntVal (num b (post (case compare x1 x2 of
  LT -> -1
  EQ -> 0
  GT -> 1))))

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
      fields <- getInitializedFields -< c
      addr <- alloc -< (ObjectVal c (Map.fromList fields))
      returnA -< RefVal addr
    _ -> defaultValue -< t
  newArray = proc (t,sizes) -> case sizes of
    (s:rest) -> case s of
      TopVal -> returnA -< top
      IntVal _ -> do
        val <- newArray -< (t,rest)
        alloc >>^ RefVal -< ArrayVal val s
      _ -> fail -< StaticException $ printf "Expected an integer array size, got %s" (show s)
    [] -> defaultValue -< t
  and = withInt (.&.)
  or = withInt (.|.)
  xor = withInt B.xor
  rem = withInt P.mod <+> withFloat mod'
  cmp = (id &&& U.const askBounds) >>> proc ((v1,v2),b) -> case (v1,v2) of
    (TopVal,_) -> returnA -< IntVal (range b (-1) 1)
    (_,TopVal) -> returnA -< IntVal (range b (-1) 1)
    (LongVal x1,LongVal x2) -> do
      let nums = map (num b) [-1,0,1]
      let pairs = zip [x1 < x2,x1 == x2,x2 < x1] nums
      let filteredNums = map snd $ filter ((P./=B.False) . fst) pairs
      returnA -< IntVal $ lub filteredNums
    _ -> fail -< StaticException "Expected long variables for 'cmp'"
  cmpg = (id &&& U.const askBounds) >>> proc ((v1,v2),b) -> case (v1,v2) of
    (TopVal,_) -> returnA -< top
    (_,TopVal) -> returnA -< top
    (FloatVal x1,FloatVal x2) -> order id -< (b,x1,x2)
    (DoubleVal x1,DoubleVal x2) -> order id -< (b,x1,x2)
    _ -> fail -< StaticException "Expected floating variables for 'cmpg'"
  cmpl = (id &&& U.const askBounds) >>> proc ((v1,v2),b) -> case (v1,v2) of
    (TopVal,_) -> returnA -< top
    (_,TopVal) -> returnA -< top
    (FloatVal x1,FloatVal x2) -> order (*(-1)) -< (b,x1,x2)
    (DoubleVal x1,DoubleVal x2) -> order (*(-1)) -< (b,x1,x2)
    _ -> fail -< StaticException "Expected floating variables for 'cmpl'"
  shl = withInt shiftL
  shr = withInt shiftR
  ushr = withInt shiftR
  plus = withInt (+) <+> withFloat (+)
  minus = withInt (-) <+> withFloat (-)
  mult = withInt (*) <+> withFloat (*)
  div = withFloat (P./) <+> proc (v1,v2) -> case (v1,v2) of
    (IntVal x1,IntVal x2) -> div_ >>^ IntVal -< (x1,x2)
    (LongVal x1,LongVal x2) -> div_ >>^ LongVal -< (x1,x2)
    _ -> fail -< StaticException "Expected numeric variables for 'div'"
    where
      div_ = proc (x1,x2) -> case x1 / x2 of
        PE.Fail _ -> createException >>> fail >>> fail -< ("java.lang.ArithmeticException","/ by zero")
        PE.Success y -> returnA -< y
  lengthOf = proc v -> case v of
    TopVal -> returnA -< top
    ArrayVal _ s -> returnA -< s
    _ -> fail -< StaticException $ printf "Expected an array as argument for 'lengthof', got %s" (show v)
  neg = proc v -> case v of
    TopVal -> returnA -< top
    IntVal n -> returnA -< IntVal (-n)
    LongVal l -> returnA -< LongVal (-l)
    FloatVal f -> returnA -< FloatVal (-f)
    DoubleVal d -> returnA -< DoubleVal (-d)
    _ -> fail -< StaticException "Expected a number as argument for '-'"
  doubleConstant = arr DoubleVal
  floatConstant = arr FloatVal
  intConstant = (id &&& U.const askBounds) >>> arr (\(x,b) -> IntVal $ num b x)
  longConstant = (id &&& U.const askBounds) >>> arr (\(x,b) -> IntVal $ num b x)
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
        ArrayVal x s -> do
          x' <- deepDeref -< x
          returnA -< ArrayVal x' s
        _ -> returnA -< v
    _ -> returnA -< val
  defaultValue = (id &&& U.const askBounds) >>> arr (\(t,b) -> case t of
    BooleanType   -> IntVal $ num b 0
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
    _             -> BottomVal)
  readIndex = proc (v,i) -> case (v,i) of
    (TopVal,_) -> returnA -< top
    (_,TopVal) -> returnA -< top
    (ArrayVal x (IntVal s),IntVal n) -> do
      b <- askBounds -< ()
      if n ⊑ (num b 0 ⊔ s)
      then returnA -< x
      else createException >>> fail -< ("java.lang.ArrayIndexOutOfBoundsException",printf "Index %d out of bounds" (show n))
    (ArrayVal _ _,_) -> fail -< StaticException $ printf "Expected an integer index for array lookup, got %s" (show i)
    _ -> fail -< StaticException $ printf "Expected an array for index lookup, got %s" (show v)
  updateIndex = first (first (id &&& deref)) >>> proc (((ref,a),i),v) -> case (ref,a,i) of
    (TopVal,_,_) -> returnA -< top
    (RefVal addr,ArrayVal x (IntVal s),IntVal n) -> do
      b <- askBounds -< ()
      if n ⊑ (num b 0 ⊔ s)
      then write -< (addr,ArrayVal (x ⊔ v) (IntVal s))
      else createException >>> fail -< ("java.lang.ArrayIndexOutOfBoundsException",printf "Index %d out of bounds" (show n))
    (RefVal _,ArrayVal _ _,_) -> fail -< StaticException $ printf "Expected an integer index for array lookup, got %s" (show i)
    (RefVal _,TopVal,_) -> returnA -< () -- TopVal is already written
    _ -> fail -< StaticException $ printf "Expected an array for index lookup, got %s" (show v)
  readField = proc (v,f) -> case v of
    TopVal -> returnA -< top
    (ObjectVal _ m) -> case Map.lookup f m of
      Just x -> returnA -< x
      Nothing -> fail -< StaticException $ printf "Field %s not defined for object %s" (show f) (show v)
    _ -> fail -< StaticException $ printf "Expected an object for field lookup, got %s" (show v)
  updateField = first (id &&& deref) >>> proc ((ref,o),(f,v)) -> case (ref,o) of
    (TopVal,_) -> returnA -< top
    (RefVal addr,ObjectVal c m) -> case m Map.!? f of
      Just _ -> write -< (addr,ObjectVal c (Map.insert f v m))
      Nothing -> fail -< StaticException $ printf "FieldSignature %s not defined on object %s" (show f) (show o)
    _ -> fail -< StaticException $ printf "Expected an object for field update, got %s" (show o)
  case_ f g = proc (v,cases) -> case v of
    IntVal _ -> do
      labels <- matchCases -< (v,cases)
      case labels of
        [] -> g -< v
        _ -> lubA f -< labels
    TopVal -> returnA -< Just top
    _ -> fail -< StaticException "Expected integer argument for 'case'"
    where
      matchCases = proc (v,cases) -> case cases of
        [] -> returnA -< []
        ((ConstantCase n,label): rest) -> do
          n' <- intConstant -< n
          b <- eq -< (n',v)
          case b of
            B.True -> returnA -< [label]
            B.False -> matchCases -< (v,rest)
            B.Top -> do
              labels <- matchCases -< (v,rest)
              returnA -< label:labels
        ((DefaultCase,label): _) -> returnA -< [label]
  catch f = proc (v,clauses) -> case clauses of
    [] -> fail -< DynamicException v
    (clause:rest) -> do
      b <- instanceOf -< (v,RefType (className clause))
      case b of
        B.True -> f -< (v,clause)
        B.False -> catch f -< (v,rest)
        B.Top -> joined f (catch f) -< ((v,clause),(v,rest))

cmpNum :: (CanFail Val c) => (B.Bool -> B.Bool) -> c (Val,Val) B.Bool
cmpNum post = proc (v1,v2) -> case (v1,v2) of
  (TopVal,_) -> returnA -< B.Top
  (_,TopVal) -> returnA -< B.Top
  (IntVal x1,IntVal x2) -> returnA -< post (x1 < x2)
  (LongVal x1,LongVal x2) -> returnA -< post (x1 < x2)
  (FloatVal x1,FloatVal x2) -> returnA -< post $ bool (x1 P.< x2)
  (DoubleVal x1,DoubleVal x2) -> returnA -< post $ bool (x1 P.< x2)
  _ -> fail -< StaticException $ printf "Expected numeric variables for comparison, got %s %s" (show v1) (show v2)

instance UseBool B.Bool Val Interp where
  eq = arr $ uncurry (==)
  neq = arr $ B.not . uncurry (==)
  gt = swap ^>> cmpNum id
  ge = cmpNum B.not
  lt = cmpNum id
  le = swap ^>> cmpNum B.not
  instanceOf = first deepDeref >>> (id &&& U.const askBounds) >>> (proc ((v,t),b) -> case (v,t) of
    (TopVal,        _)            -> returnA -< B.Top
    (IntVal n,      BooleanType)  -> returnA -< bool $ n ⊑ range b 0 1
    (IntVal n,      ByteType)     -> returnA -< bool $ n ⊑ range b (-128) 127     -- n >= (-2)^7  && n < 2^7
    (IntVal n,      CharType)     -> returnA -< bool $ n ⊑ range b 0 65535        -- n >= 0       && n < 2^16
    (IntVal n,      ShortType)    -> returnA -< bool $ n ⊑ range b (-32768) 32767 -- n >= (-2)^15 && n < 2^15
    (IntVal _,      IntType)      -> returnA -< B.True
    (LongVal _,     LongType)     -> returnA -< B.True
    (FloatVal _,    FloatType)    -> returnA -< B.True
    (DoubleVal _,   DoubleType)   -> returnA -< B.True
    (NullVal,       NullType)     -> returnA -< B.True
    (ObjectVal c _, RefType p)    -> isSuperClass -< (c,p)
    (ArrayVal x _,  ArrayType t') -> instanceOf -< (x,t')
    (_,             _)            -> returnA -< B.False)
    where
      isSuperClass = proc (c,p) -> if c P.== p
        then returnA -< B.True
        else do
          unit <- Shared.readCompilationUnit -< c
          case extends unit of
            Just c' -> isSuperClass -< (c',p)
            Nothing -> returnA -< B.False
  cast = first (first (id &&& deepDeref)) >>> proc (((v,v'),t),b) -> case (b,v') of
    (B.False,_) -> createException >>> fail -< ("java.lang.ClassCastException",printf "Cannot cast %s to type %s" (show v) (show t))
    (B.Top,ObjectVal _ _) -> joined returnA (createException >>> fail) -< (v,("java.lang.ClassCastException",printf "Cannot cast %s to type %s" (show v) (show t)))
    (B.True,ObjectVal _ _) -> returnA -< v
    (_,_) -> fail -< StaticException "Casting of primivites and arrays is not yet supported"
    -- https://docs.oracle.com/javase/specs/jls/se7/html/jls-5.html#jls-5.1.2
  if_ f1 f2 = proc (v,(x,y)) -> case v of
    B.True -> f1 -< x
    B.False -> f2 -< y
    B.Top -> joined f1 f2 -< (x,y)

instance UseMem Env Addr Interp where
  emptyEnv = arr $ const E.empty
  addrFromInt = arr id

instance UseConst Interp where
  askCompilationUnits = askConst >>^ (\(x,_,_) -> x)
  askFields = askConst >>^ (\(_,x,_) -> x)

---- End of Actual Evaluation methods ----
