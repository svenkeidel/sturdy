{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
module ConcreteInterpreter where

import           Prelude hiding (id,fail,lookup,read)

import           Data.Bits
import           Data.Fixed
import           Data.Hashable (Hashable)
import           Data.List (replicate,repeat,find,splitAt)
import           Data.Int
import           Data.Word
import           Data.Text(Text)
import qualified Data.Text as T
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map

-- import           Data.Concrete.Error
-- import           Data.Concrete.Exception

import           Control.Monad (return,fmap,replicateM)
import           Control.Category hiding ((.))

import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Const
import           Control.Arrow.Environment
import           Control.Arrow.Except
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.State
import           Control.Arrow.Store
import qualified Control.Arrow.Trans as Trans
import qualified Control.Arrow.Utils as U

import           Control.Arrow.Transformer.Const
import           Control.Arrow.Transformer.Reader
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Concrete.Environment
import           Control.Arrow.Transformer.Concrete.Failure
import           Control.Arrow.Transformer.Concrete.Store

import           Syntax
import           GenericInterpreter (Frame,PC,IsVal)
import qualified GenericInterpreter as Generic

import           GHC.Generics(Generic)
import           Data.Profunctor

import           Text.Printf

data Val
  = IntVal Int32
  | LongVal Int64
  | FloatVal Float
  | DoubleVal Double
  | StringVal Text
  -- | ClassVal Text
  | NullVal
  | RefVal Addr
  | ArrayVal [Val]
  | ObjectVal ClassId Object
  deriving stock (Eq,Generic)
  deriving anyclass (Hashable)

type Object = HashMap FieldName Val
type Exception = Val
type Env = Frame Val
type Addr = Word64
data Store = Store
  { dynamicStore :: HashMap Addr Val
  , staticStore :: HashMap ClassId Object
  , nextAddress :: Addr
  }

run :: ClassTable -> ClassId -> MethodSignature -> (Store,Exception)
run classTable mainClass mainMethod = _ $
  Trans.run
   (Generic.run ::
     ValueT Val
     (ConstT ClassTable
      (EnvT Env
       (ExceptT Exception
        (StoreT Store
         (FailureT Text
          (->)))))) PC Val)
    classTable
    (emptyStore,(_frame,0))

instance (ArrowChoice c, Profunctor c) => ArrowEnv Variable Val (EnvT Env c) where
  type Join y (EnvT Env c) = ()
  lookup = _
  extend = _

instance IsVal Val (ValueT Val c) where
  type JoinVal y (ValueT Val c) = ()
  if_ = _
  tableswitch = _
  lookupswitch = _
  lookupMethod = _
  matchException = _
  new = _
  newArray = _
  void = _
  doubleConstant = _
  floatConstant = _
  intConstant = _
  longConstant = _
  nullConstant = _
  stringConstant = _
  and = _
  or = _
  xor = _
  rem = _
  mod = _
  cmp = _
  cmpg = _
  cmpl = _
  shl = _
  shr = _
  ushr = _
  plus = _
  minus = _
  mult = _
  lengthOf = _
  div = _
  neg = _
  cast = _
  instanceOf = _

-- type Constants = ([CompilationUnit],Map FieldSignature Addr)

-- newtype Interp x y = Interp
--   (Fix [Statement] (Maybe Val)
--     (Except (Exception Val)
--       (Reader Context
--         (Environment String Addr
--           (StoreArrow Addr Val
--             (State Addr
--               (Const Constants
--                 (->))))))) x y)
--   deriving (Category,Arrow,ArrowChoice)

-- deriving instance ArrowConst Constants Interp
-- deriving instance ArrowEnv String Addr (Env String Addr) Interp
-- deriving instance ArrowExcept x y (Exception Val) Interp
-- deriving instance ArrowFail (Exception Val) Interp
-- deriving instance ArrowFix [Statement] (Maybe Val) Interp
-- deriving instance ArrowReader Context Interp
-- deriving instance ArrowRead Addr Val x Val Interp
-- deriving instance ArrowState Addr Interp
-- deriving instance ArrowWrite Addr Val Interp

-- runInterp :: Interp x y ->
--              [CompilationUnit] -> [(String,Val)] -> x ->
--              Error (Exception Val) y
-- runInterp (Interp f) compilationUnits mem x =
--   runFixPoint
--     (runConst (compilationUnits,fields)
--       (evalState
--         (evalStore
--           (runEnvironment'
--             (runReader
--               (runExcept f))))))
--   (latestAddr + Map.size fields,(S.fromList store,(env,(([],[]),x))))
--   where
--     (env,store) = (\(nv,st) -> (nv,expand st)) $
--       unzip $ map (\((l,v),a) -> ((l,a),(a,v))) $ reverse $ zip mem [0..]
--     -- TODO Extend this function to dereference nested arrays and objects
--     expand = foldl (\st (a,v) -> case (st,v) of
--       ([],ObjectVal c m) ->       [(a + 1,ObjectVal c m),(a,RefVal (a + 1))]
--       ((a',_):_,ObjectVal c m) -> [(a' + 1,ObjectVal c m),(a,RefVal (a' + 1))] ++ st
--       ([],ArrayVal xs) ->       [(a + 1,ArrayVal xs),(a,RefVal (a + 1))]
--       ((a',_):_,ArrayVal xs) -> [(a' + 1,ArrayVal xs),(a,RefVal (a' + 1))] ++ st
--       (_,_) -> (a,v):st
--       ) []
--     latestAddr = case store of
--       [] -> 0
--       (a,_):_ -> a
--     fields = Map.fromList $ zip
--       (concatMap (getFieldSignatures (\m -> Static `elem` m)) compilationUnits)
--       [latestAddr..]

-- type Out v = Error (Exception Val) v
-- type Mem = [(String,Val)]

-- runProgram' :: [CompilationUnit] -> (MethodSignature,[Immediate]) -> Out (Maybe Val)
-- runProgram' units = runInterp (try' runProgram deepDerefMaybe) units []

-- runStatements' :: [CompilationUnit] -> Mem -> [Statement] -> Out (Maybe Val)
-- runStatements' = runInterp (initStatements (try' runStatements deepDerefMaybe))

-- eval' :: [CompilationUnit] -> Mem -> Expr -> Out Val
-- eval' = runInterp (try' eval deepDeref)

-- evalBool' :: [CompilationUnit] -> Mem -> BoolExpr -> Out Bool
-- evalBool' = runInterp (try' evalBool id)

-- evalImmediate' :: [CompilationUnit] -> Mem -> Immediate -> Out Val
-- evalImmediate' = runInterp (try' evalImmediate deepDeref)

-- ---- Instances -----------------------------------------------------------------

-- instance UseVal Val Interp where
--   newSimple = proc t -> case t of
--     RefType c -> do
--       fields <- getInitializedFields -< c
--       alloc >>^ RefVal -< (ObjectVal c (Map.fromList fields))
--     _ -> defaultValue -< t
--     where
--       getInitializedFields = readCompilationUnit >>> proc unit -> do
--         let fieldSignatures = getFieldSignatures (\m -> Static `notElem` m) unit
--         ownFields <- U.map (second defaultValue) -<
--           map (\s@(FieldSignature _ t' _) -> (s,t')) fieldSignatures
--         case extends unit of
--           Just p -> do
--             parentFields <- getInitializedFields -< p
--             returnA -< parentFields ++ ownFields
--           Nothing -> returnA -< ownFields
--   newArray = proc (t,sizes) -> case sizes of
--     (s:sizes') -> case s of
--       IntVal s' -> do
--         vals <- U.map newArray -< replicate s' (t,sizes')
--         alloc >>^ RefVal -< ArrayVal vals
--       _ -> fail -<
--         StaticException "Expected an integer array size"
--     [] -> defaultValue -< t
--   and = withInt (.&.)
--   or = withInt (.|.)
--   xor = withInt Data.Bits.xor
--   rem = withInt mod <+> withFloat mod'
--   cmp = proc (v1,v2) -> case (v1,v2) of
--     (LongVal x1,LongVal x2) -> order id -< (x1,x2)
--     _ -> failStatic -< "Expected long variables for 'cmp'"
--   cmpg = proc (v1,v2) -> case (v1,v2) of
--     (FloatVal x1,FloatVal x2) -> order id -< (x1,x2)
--     (DoubleVal x1,DoubleVal x2) -> order id -< (x1,x2)
--     _ -> failStatic -< "Expected floating variables for 'cmpg'"
--   cmpl = proc (v1,v2) -> case (v1,v2) of
--     (FloatVal x1,FloatVal x2) -> order (*(-1)) -< (x1,x2)
--     (DoubleVal x1,DoubleVal x2) -> order (*(-1)) -< (x1,x2)
--     _ -> failStatic -< "Expected floating variables for 'cmpl'"
--   shl = withInt shiftL
--   shr = withInt shiftR
--   ushr = withInt shiftR
--   plus = withInt (+) <+> withFloat (+)
--   minus = withInt (-) <+> withFloat (-)
--   mult = withInt (*) <+> withFloat (*)
--   div = withFloat (/) <+> proc (v1,v2) -> case (v1,v2) of
--     (IntVal x1,IntVal x2) -> div_ >>^ IntVal -< (x1,x2)
--     (LongVal x1,LongVal x2) -> div_ >>^ LongVal -< (x1,x2)
--     _ -> failStatic -< "Expected numeric variables for 'div'"
--     where
--       div_ = proc (x1,x2) -> if x2 == 0
--         then createException >>> failDynamic -<
--           ("java.lang.ArithmeticException","/ by zero")
--         else returnA -< (x1 `Prelude.div` x2)
--   lengthOf = deref >>> proc v -> case v of
--     ArrayVal xs -> returnA -< (IntVal (length xs))
--     _ -> failStatic -< "Expected an array variable for 'lengthOf'"
--   neg = proc v -> case v of
--     IntVal n -> returnA -< IntVal (-n)
--     LongVal l -> returnA -< LongVal (-l)
--     FloatVal f -> returnA -< FloatVal (-f)
--     DoubleVal d -> returnA -< DoubleVal (-d)
--     _ -> failStatic -< "Expected a number as argument for -"
--   doubleConstant = arr DoubleVal
--   floatConstant = arr FloatVal
--   intConstant = arr IntVal
--   longConstant = arr LongVal
--   nullConstant = arr $ const NullVal
--   stringConstant = arr StringVal
--   classConstant = arr ClassVal
--   defaultValue = proc t -> case t of
--     BooleanType   -> returnA -< IntVal 0
--     ByteType      -> returnA -< IntVal 0
--     CharType      -> returnA -< IntVal 0
--     ShortType     -> returnA -< IntVal 0
--     IntType       -> returnA -< IntVal 0
--     LongType      -> returnA -< LongVal 0
--     FloatType     -> returnA -< FloatVal 0.0
--     DoubleType    -> returnA -< DoubleVal 0.0
--     NullType      -> returnA -< NullVal
--     (RefType _)   -> returnA -< NullVal
--     (ArrayType _) -> returnA -< NullVal
--     _             ->
--       failStatic -< printf "No default value for type %s" (show t)
--   instanceOf = first deref >>> (proc (v,t) -> case (v,t) of
--     (IntVal 0,      BooleanType)  -> fromBool -< True
--     (IntVal 1,      BooleanType)  -> fromBool -< True
--     (IntVal n,      ByteType)     -> fromBool -< n >= -128   && n < 128
--     (IntVal n,      CharType)     -> fromBool -< n >= 0      && n < 65536
--     (IntVal n,      ShortType)    -> fromBool -< n >= -32768 && n < 32768
--     (IntVal _,      IntType)      -> fromBool -< True
--     (LongVal _,     LongType)     -> fromBool -< True
--     (FloatVal _,    FloatType)    -> fromBool -< True
--     (DoubleVal _,   DoubleType)   -> fromBool -< True
--     (NullVal,       NullType)     -> fromBool -< True
--     (ObjectVal c _, RefType p)    -> isSuperClass -< (c,p)
--     (ArrayVal xs,   ArrayType t') ->
--       U.map instanceOf >>> all (==IntVal 1) ^>> fromBool -< zip xs (repeat t')
--     (_,_) -> fromBool -< False)
--     where
--       fromBool = arr $ \b -> if b then IntVal 1 else IntVal 0
--       isSuperClass = proc (c,p) -> if c == p
--         then fromBool -< True
--         else do
--           unit <- readCompilationUnit -< c
--           case extends unit of
--             Just c' -> isSuperClass -< (c',p)
--             Nothing -> fromBool -< False
--   cast = first (first (id &&& deref)) >>> second toBool >>> proc (((v,v'),t),b) -> case (b,v') of
--     (False,_) -> createException >>> failDynamic -<
--       ("java.lang.ClassCastException",printf "Cannot cast %s to type %s" (show v) (show t))
--     (True,ObjectVal _ _) -> returnA -< v
--     (_,IntVal n) -> returnA -<
--       (if t == FloatType then FloatVal else DoubleVal) $ fromIntegral n
--     (_,LongVal n) -> returnA -<
--       (if t == FloatType then FloatVal else DoubleVal) $ fromIntegral n
--     (_,FloatVal n) | isIntegerType t -> returnA -< IntVal $ round n
--     (_,DoubleVal n) | isIntegerType t -> returnA -< IntVal $ round n
--     (_,FloatVal n) | isIntegerType t -> returnA -< IntVal $ round n
--     (_,DoubleVal n) | isIntegerType t -> returnA -< IntVal $ round n
--     (_,_) -> failStatic -< "Casting of primivites and arrays is not yet supported"
--     -- https://docs.oracle.com/javase/specs/jls/se7/html/jls-5.html#jls-5.1.2
--   declare f = proc ((l,v),x) -> do
--     addr <- alloc -< v
--     extendEnv' f -< (l,addr,x)
--   readVar = lookup_ >>> read_
--   updateVar f = first (first lookup_ >>> write) >>> U.pi2 >>> f
--   readIndex = first deref >>> proc (v,i) -> case (v,i) of
--     (ArrayVal xs,IntVal n)
--       | n >= 0 && n < length xs -> returnA -< xs !! n
--       | otherwise -> createException >>> failDynamic -<
--         ("java.lang.ArrayIndexOutOfBoundsException",printf "Index %d out of bounds" (show n))
--     (ArrayVal _,_) -> failStatic -<
--       printf "Expected an integer index for array lookup, got %s" (show i)
--     _ -> failStatic -<
--       printf "Expected an array for index lookup, got %s" (show v)
--   updateIndex f = first (first (first (id &&& deref)) >>> proc (((ref,a),i),v) -> case (ref,a,i) of
--     (RefVal addr,ArrayVal xs,IntVal n)
--       | n >= 0 && n < length xs -> write -<
--         (addr,ArrayVal ((\(h,_:t) -> h ++ v:t) $ splitAt n xs))
--       | otherwise -> createException >>> failDynamic -<
--         ("java.lang.ArrayIndexOutOfBoundsException",printf "Index %d out of bounds" (show n))
--     (RefVal _,ArrayVal _,_) -> failStatic -<
--       printf "Expected an integer index for array lookup, got %s" (show i)
--     _ -> failStatic -<
--       printf "Expected an array for index lookup, got %s" (show v)) >>> U.pi2 >>> f
--   readField = first deref >>> proc (v,f) -> case v of
--     (ObjectVal _ m) -> justOrFail -<
--       (Map.lookup f m,printf "Field %s not defined for object %s" (show f) (show v))
--     NullVal -> createException >>> failDynamic -< ("java.lang.NullPointerException","")
--     _ -> failStatic -<
--       printf "Expected an object for field lookup, got %s" (show v)
--   updateField g = first (first (id &&& deref) >>> proc ((ref,o),(f,v)) -> case (ref,o) of
--     (RefVal addr,ObjectVal c m) -> case m Map.!? f of
--       Just _ -> write -< (addr,ObjectVal c (Map.insert f v m))
--       Nothing -> failStatic -<
--         printf "FieldSignature %s not defined on object %s" (show f) (show o)
--     (NullVal,_) -> createException >>> failDynamic -< ("java.lang.NullPointerException","")
--     _ -> failStatic -<
--       printf "Expected an object for field update, got %s" (show o)) >>> U.pi2 >>> g
--   readStaticField = lookupStaticField >>> read_
--   updateStaticField = first lookupStaticField >>> write
--   case_ f = proc (v,cases) -> case v of
--     IntVal x -> case find (matchCase x) cases of
--       Just (_,label) -> f -< label
--       Nothing -> failStatic -< "No matching cases"
--     _ -> failStatic -< "Expected an integer as argument for switch"
--     where
--       matchCase x (c,_) = case c of
--         ConstantCase n -> x == n
--         DefaultCase    -> True

-- instance UseException Exception Val Interp where
--   catch f = proc (ex,clauses) -> case ex of
--     StaticException _ -> fail -< ex
--     DynamicException val -> handleException -< (val,clauses)
--     where
--       handleException = proc (v,clauses) -> case clauses of
--         [] -> failDynamic -< v
--         (clause:rest) -> do
--           b <- instanceOf >>> toBool -< (v,RefType (className clause))
--           if b then f -< (v,clause) else handleException -< (v,rest)
--   failDynamic = DynamicException ^>> fail
--   failStatic = StaticException ^>> fail

-- instance UseBool Bool Val Interp where
--   eq = arr (uncurry (==))
--   neq = arr (uncurry (/=))
--   gt = swap ^>> cmpNum id
--   ge = cmpNum not
--   lt = cmpNum id
--   le = swap ^>> cmpNum not
--   if_ f1 f2 = proc ((v,_),(x,y)) -> if v then f1 -< x else f2 -< y

-- instance UseEnv (Env String Addr) Interp where
--   emptyEnv = arr $ const E.empty

-- instance UseConst Interp where
--   askCompilationUnits = askConst >>^ fst

-- ---- Helper Methods ------------------------------------------------------------

-- lookup_ :: Interp String Addr
-- lookup_ = proc x -> lookup U.pi1 fail -<
--   (x,StaticException $ printf "Variable %s not bound" (show x))

-- read_ :: Interp Addr Val
-- read_ = proc addr -> read U.pi1 fail -<
--   (addr,StaticException $ printf "Address %s not bound" (show addr))

-- alloc :: Interp Val Addr
-- alloc = proc val -> do
--   addr <- get -< ()
--   write -< (addr,val)
--   put -< succ addr
--   returnA -< addr

-- withInt :: (CanFail Exception Val c) => (Int -> Int -> Int) -> c (Val,Val) Val
-- withInt op = proc (v1,v2) -> case (v1,v2) of
--   (IntVal x1,IntVal x2) -> returnA -< IntVal $ op x1 x2
--   (LongVal x1,LongVal x2) -> returnA -< LongVal $ op x1 x2
--   _ -> failStatic -< "Expected integer variables for op"

-- withFloat :: (CanFail Exception Val c) => (Float -> Float -> Float) -> c (Val,Val) Val
-- withFloat op = proc (v1,v2) -> case (v1,v2) of
--   (FloatVal x1,FloatVal x2) -> returnA -< FloatVal $ op x1 x2
--   (DoubleVal x1,DoubleVal x2) -> returnA -< DoubleVal $ op x1 x2
--   _ -> failStatic -< "Expected floating variables for op"

-- order :: (Ord x,Arrow c) => (Int -> Int) -> c (x,x) Val
-- order post = arr (IntVal . post . (\(x1,x2) -> case compare x1 x2 of
--   LT -> -1
--   EQ -> 0
--   GT -> 1))

-- toBool :: (CanFail Exception Val c) => c Val Bool
-- toBool = proc b -> case b of
--   IntVal 0 -> returnA -< False
--   IntVal 1 -> returnA -< True
--   _ -> failStatic -< printf "Expected boolean value, got %s" (show b)

-- cmpNum :: (Bool -> Bool) -> Interp (Val,Val) Bool
-- cmpNum post = proc (v1,v2) -> case (v1,v2) of
--   (IntVal x1,IntVal x2) -> returnA -< post (x1 < x2)
--   (LongVal x1,LongVal x2) -> returnA -< post (x1 < x2)
--   (FloatVal x1,FloatVal x2) -> returnA -< post (x1 < x2)
--   (DoubleVal x1,DoubleVal x2) -> returnA -< post (x1 < x2)
--   _ -> failStatic -< "Expected numeric variables for comparison"

-- createException :: Interp (String,String) Val
-- createException = proc (clzz,message) -> do
--   RefVal addr <- newSimple -< RefType clzz
--   v <- read_ -< addr
--   case v of
--     ObjectVal c m -> do
--       let m' = Map.insert
--             (FieldSignature "java.lang.Throwable" (RefType "java.lang.String") "message")
--             (StringVal message) m
--       write -< (addr,ObjectVal c m')
--       returnA -< RefVal addr
--     _ -> failStatic -< printf "Undefined exception %s" clzz

-- lookupStaticField :: Interp FieldSignature Addr
-- lookupStaticField = proc f -> do
--   (_,fields) <- askConst -< ()
--   justOrFail -< (Map.lookup f fields,printf "Field %s not bound" (show f))

-- deref :: Interp Val Val
-- deref = proc val -> case val of
--   RefVal addr -> read_ -< addr
--   _ -> returnA -< val

-- deepDeref :: Interp Val Val
-- deepDeref = proc val -> case val of
--   RefVal addr -> do
--     v <- read_ -< addr
--     case v of
--       ObjectVal c m -> do
--         let (keys,vals) = unzip (Map.toList m)
--         vals' <- U.map deepDeref -< vals
--         returnA -< ObjectVal c (Map.fromList (zip keys vals'))
--       ArrayVal xs -> U.map deepDeref >>^ ArrayVal -< xs
--       _ -> returnA -< v
--   _ -> returnA -< val

-- deepDerefMaybe :: Interp (Maybe Val) (Maybe Val)
-- deepDerefMaybe = liftAMaybe deepDeref

-- deepDerefDynamic :: Interp (Exception Val) (Exception Val)
-- deepDerefDynamic = proc e -> case e of
--   DynamicException v -> deepDeref >>^ DynamicException -< v
--   StaticException _ -> returnA -< e

instance Show Val where
  show (IntVal n) = show n
  show (LongVal l) = show l ++ "l"
  show (FloatVal f) = show f
  show (DoubleVal d) = show d ++ "d"
  show (StringVal s) = T.unpack s
  -- show (ClassVal c) = "<" ++ c ++ ">"
  show NullVal = "null"
  show (RefVal a) = "@" ++ show a
  show (ArrayVal xs) = show xs
  show (ObjectVal c m) = show c ++ "{" ++ show m ++ "}"

emptyStore :: Store
emptyStore = Store { dynamicStore = Map.empty, staticStore = Map.empty, nextAddress = 0 }
