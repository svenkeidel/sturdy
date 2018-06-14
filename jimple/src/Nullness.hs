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
module Nullness where

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
import qualified Data.Abstract.Boolean as Abs
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import           Data.Abstract.Bounded
import           Data.Abstract.Ordering
import           Data.Abstract.Equality
import           Data.Abstract.HandleError

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
type Constants = (CompilationUnits,Fields)

type Addr = Int
-- Remove these instances when abstract multi-env is available.
instance LowerBounded Addr where
  bottom = (-2)^(29 :: Int)
instance Complete Addr where
  a ⊔ _ = a

data Val
  = Bottom
  | Null
  | NonNull
  | Top deriving (Eq)

instance Show Val where
  show Bottom = "⊥"
  show Null = "Null"
  show NonNull = "NonNull"
  show Top = "⊤"

instance Equality Val where
  Top == Top = B.true
  Null == Null = Abs.Top
  NonNull == NonNull = Abs.Top
  Bottom == Bottom = B.true
  _ == _ = B.false

instance PreOrd Val where
  Bottom ⊑ _ = P.True
  _ ⊑ Top = P.True
  Null ⊑ Null = P.True
  NonNull ⊑ NonNull = P.True
  _ ⊑ _ = P.False

instance Complete Val where
  Bottom ⊔ v = v
  v ⊔ Bottom = v
  Null ⊔ Null = Null
  NonNull ⊔ NonNull = NonNull
  _ ⊔ _ = top

instance UpperBounded Val where
  top = Top

instance LowerBounded Val where
  bottom = Bottom

---- End of Values ----

---- Interp Type ----

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

---- End of Program Boilerplate ----

binop :: UseVal Val c => c (Val,Val) Val
binop = proc (v1,v2) -> returnA -< NonNull

unop :: UseVal Val c => c Val Val
unop = proc v -> returnA -< NonNull

instance UseVal Val Interp where
  newSimple = arr $ const NonNull
  newArray = arr $ const NonNull
  and = binop
  or = binop
  xor = binop
  rem = binop
  cmp = binop
  cmpg = binop
  cmpl = binop
  shl = binop
  shr = binop
  ushr = binop
  plus = binop
  minus = binop
  mult = binop
  div = binop
  lengthOf = unop
  neg = unop
  doubleConstant = arr $ const NonNull
  floatConstant = arr $ const NonNull
  intConstant = arr $ const NonNull
  longConstant = arr $ const NonNull
  nullConstant = arr $ const Null
  stringConstant = arr $ const NonNull
  classConstant = arr $ const NonNull
  deref = arr id
  deepDeref = arr id
  defaultValue = arr (\t -> case t of
    (ArrayType _) -> Null
    BooleanType   -> NonNull
    ByteType      -> NonNull
    CharType      -> NonNull
    DoubleType    -> NonNull
    FloatType     -> NonNull
    IntType       -> NonNull
    LongType      -> NonNull
    NullType      -> Null
    (RefType _)   -> Null
    ShortType     -> NonNull
    UnknownType   -> top
    VoidType      -> bottom)
  instanceOf = proc _ -> fail -< StaticException "Not implemented"
    --
    -- first deepDeref >>> (proc (v,t) -> case (v,t) of
    -- (TopVal,        _)            -> returnA -< BoolVal Abs.Top
    -- (BoolVal _,     BooleanType)  -> returnA -< BoolVal Abs.True
    -- (IntVal n,      ByteType)     -> do
    --   b <- askBounds -< ()
    --   returnA -< boolVal $ n ⊑ range b (-128) 127     -- n >= (-2)^7  && n < 2^7
    -- (IntVal n,      CharType)     -> do
    --   b <- askBounds -< ()
    --   returnA -< boolVal $ n ⊑ range b 0 65535        -- n >= 0       && n < 2^16
    -- (IntVal n,      ShortType)    -> do
    --   b <- askBounds -< ()
    --   returnA -< boolVal $ n ⊑ range b (-32768) 32767 -- n >= (-2)^15 && n < 2^15
    -- (IntVal _,      IntType)      -> returnA -< BoolVal Abs.True
    -- (LongVal _,     LongType)     -> returnA -< BoolVal Abs.True
    -- (FloatVal _,    FloatType)    -> returnA -< BoolVal Abs.True
    -- (DoubleVal _,   DoubleType)   -> returnA -< BoolVal Abs.True
    -- (NullVal,       NullType)     -> returnA -< BoolVal Abs.True
    -- (ObjectVal c _, RefType p)    -> isSuperClass -< (c,p)
    -- (ArrayVal x _,  ArrayType t') -> instanceOf -< (x,t')
    -- (_,             _)            -> returnA -< BoolVal Abs.False)
    -- where
    --   isSuperClass = proc (c,p) -> if c P.== p
    --     then returnA -< BoolVal Abs.True
    --     else do
    --       unit <- Shared.readCompilationUnit -< c
    --       case extends unit of
    --         Just c' -> isSuperClass -< (c',p)
    --         Nothing -> returnA -< BoolVal Abs.False
  cast = proc _ -> fail -< StaticException "Not implemented"
  -- first (first (id &&& deepDeref)) >>> proc (((v,v'),t),b) -> case (b,v') of
  --   (BoolVal Abs.False,_) -> createException >>> fail -< ("java.lang.ClassCastException",printf "Cannot cast %s to type %s" (show v) (show t))
  --   (BoolVal Abs.Top,ObjectVal _ _) -> joined returnA (createException >>> fail) -< (v,("java.lang.ClassCastException",printf "Cannot cast %s to type %s" (show v) (show t)))
  --   (BoolVal Abs.True,ObjectVal _ _) -> returnA -< v
  --   (BoolVal _,_) -> fail -< StaticException "Casting of primivites and arrays is not yet supported"
  --   -- https://docs.oracle.com/javase/specs/jls/se7/html/jls-5.html#jls-5.1.2
  --   (TopVal,_) -> returnA -< top
  --   (_,_) -> fail -< StaticException $ printf "Expected boolean for instanceOf, got %s" (show b)
  readIndex = arr fst
  updateIndex = U.void $ arr id
  readField = arr fst
  updateField = U.void $ arr id
  case_ f = U.pi2 >>> map snd ^>> lubA f
  catch f = proc (v,clauses) ->
    joined (lubA f) fail -< (zip (repeat v) clauses, DynamicException v)

cmp_ :: UseBool Abs.Bool Val c => c (Immediate,Immediate) Abs.Bool
cmp_ = proc (v1,v2) -> returnA -< Abs.True

instance UseBool Abs.Bool Val Interp where
  eq = cmp_
  neq = cmp_
  gt = cmp_
  ge = cmp_
  lt = cmp_
  le = cmp_
  if_ f1 f2 = proc (v,(x,y)) -> case v of
    Abs.True -> f1 -< x
    Abs.False -> f2 -< y
    Abs.Top -> joined f1 f2 -< (x,y)

instance UseMem Env Addr Interp where
  emptyEnv = arr $ const E.empty
  addrFromInt = arr id

instance UseConst Interp where
  askCompilationUnits = askConst >>^ fst
  askFields = askConst >>^ snd

---- End of Actual Evaluation methods ----
