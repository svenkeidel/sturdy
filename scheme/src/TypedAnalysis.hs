{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC
  -fsimpl-tick-factor=500
  -fno-warn-orphans
  -fno-warn-partial-type-signatures
#-}

-- | k-CFA analysis for PCF where numbers are approximated by intervals.
module TypedAnalysis where

import           Prelude hiding (not,Bounded,fail,(.),exp,read)

import           Control.Category
import           Control.Arrow hiding ((<+>))
import qualified Control.Arrow.Fix as Fix
import           Control.Arrow.Fail as Fail
import           Control.Arrow.Environment as Env
import qualified Control.Arrow.Fix.Context as Ctx
import           Control.Arrow.Closure (ArrowClosure,IsClosure(..))
import qualified Control.Arrow.Closure as Cls
import           Control.Arrow.Order
import           Control.Arrow.Store
import qualified Control.Arrow.Store as Store
import qualified Control.Arrow.Utils as ArrowUtils
import           Control.Arrow.Fix.Context
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.Fix.Metrics(Metrics)
import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow

import           Control.DeepSeq

import           Data.Hashable
import           Data.Label
import           Data.Order
import           Data.Text (Text)
import           Data.Utils
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import qualified Data.Boolean as B
import           Data.HashSet(HashSet)
import           Data.Identifiable
import           Data.Text.Prettyprint.Doc
import           Data.Profunctor
import qualified Data.Lens as L

import qualified Data.Abstract.Boolean as B
import           Data.Abstract.Terminating(Terminating)
import           Data.Abstract.Closure (Closure)
import           Data.Abstract.DiscretePowerset (Pow)
import           Data.Abstract.CallString(CallString)
import qualified Data.Abstract.Widening as W
import           Data.Abstract.Stable

import           GHC.Exts(IsString(..),toList)
import           GHC.Generics(Generic)

import           Text.Printf

import           Syntax (LExpr,Expr(Apply),Literal(..) ,Op1(..),Op2(..),OpVar(..))
import           GenericInterpreter as Generic

type Cls = Closure Expr (HashSet Env)
type Env = HashMap Text Addr
type Store = HashMap Addr Val
type Errors = HashSet Text
type Ctx = CallString Label
-- -- Input and output type of the fixpoint.

data Addr
  = VarA (Text,Ctx)
  | LabelA (Label,Ctx)
  deriving stock (Eq,Generic)
  deriving anyclass (NFData)
  deriving PreOrd via Discrete Addr

type Symbol = Text

data Val
  = Top
  | NumVal Number
  | StringVal
  | CharVal
  | QuoteVal (Pow Symbol)
  | BoolVal B.Bool
  | ClosureVal Cls
  | ListVal List
  | VoidVal
  | Bottom
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data List
  = Nil
  | Cons (Pow Addr) (Pow Addr)
  | ConsNil (Pow Addr) (Pow Addr)
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data Number
  = IntVal
  | FloatVal
  | NumTop
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

instance (ArrowContext Ctx c) => ArrowAlloc Addr (ValueT Val c) where
  alloc = proc var -> do
    ctx <- Ctx.askContext @Ctx -< ()
    returnA -< VarA (var,ctx)
  {-# INLINE alloc #-}
  {-# SCC alloc #-}

allocLabel :: (ArrowContext Ctx c) => c Label Addr
allocLabel = proc l -> do
  ctx <- Ctx.askContext @Ctx -< ()
  returnA -< LabelA (l,ctx)
{-# INLINE allocLabel #-}
{-# SCC allocLabel #-}

instance (IsString e, ArrowChoice c, ArrowFail e c, ArrowClosure Expr Cls c)
    => ArrowClosure Expr Val (ValueT Val c) where
  type Join y Val (ValueT Val c) = (Cls.Join y Cls c, Fail.Join y c)
  closure = ValueT $ proc e -> do
    cls <- Cls.closure -< e
    returnA -< ClosureVal cls
  apply (ValueT f) = ValueT $ proc (v,x) ->
    case v of
      ClosureVal cls -> Cls.apply f -< (cls,x)
      _ -> failString -< printf "Expected a closure, but got %s" (show v)
  {-# INLINE closure #-}
  {-# INLINE apply #-}
  {-# SCC closure #-}
  {-# SCC apply #-}

instance (ArrowChoice c, ArrowComplete Val c, ArrowContext Ctx c, ArrowFail e c, ArrowStore Addr Val c, ArrowEnv Text Addr c,
          Store.Join Val c, Env.Join Addr c,Store.Join Addr c,Fail.Join Val c,IsString e)
    => IsVal Val (ValueT Val c) where
  type Join y (ValueT Val c) = (ArrowComplete y (ValueT Val c),Fail.Join y c)
  lit = proc x -> case x of
    Int _ -> returnA -< NumVal IntVal
    Float _ -> returnA -< NumVal FloatVal
    Rational _ -> returnA -< Bottom
    Bool True  -> returnA -< BoolVal B.True
    Bool False  -> returnA -< BoolVal B.False
    Char _ -> returnA -< StringVal
    String _ -> returnA -< StringVal
    Quote (Symbol sym) -> returnA -< QuoteVal $ singleton sym
    _ -> returnA -< Bottom
  {-# INLINE lit #-}
  {-# SCC lit #-}

  if_ = if__
  {-# INLINE if_ #-}
  {-# SCC if_ #-}

  nil_ = proc _ -> returnA -< ListVal Nil
  {-# INLINE nil_ #-}
  {-# SCC nil_ #-}

  cons_ = proc ((v1,l1),(v2,l2)) -> do
    a1 <- allocLabel -< l1
    a2 <- allocLabel -< l2
    write -< (a1,v1)
    write -< (a2,v2)
    returnA -< ListVal (Cons (singleton a1) (singleton a2))
  {-# INLINE cons_ #-}
  {-# SCC cons_ #-}

  void = proc _ -> returnA -< VoidVal

  op1_ = proc (op, x) -> case op of
    IsNumber -> returnA -< case x of
      NumVal _ -> BoolVal B.True
      Top -> BoolVal B.Top
      _ -> BoolVal B.False
    IsInteger -> returnA -< case x of
      NumVal IntVal -> BoolVal B.True
      Top -> BoolVal B.Top
      _ -> BoolVal B.False
    IsFloat -> returnA -< case x of
      NumVal FloatVal -> BoolVal B.True
      Top -> BoolVal B.Top
      _ -> BoolVal B.False
    IsRational -> returnA -< case x of
      NumVal IntVal -> BoolVal B.True
      NumVal FloatVal -> BoolVal B.True
      Top -> BoolVal B.Top
      _ -> BoolVal B.False
    IsBoolean -> returnA -< case x of
      BoolVal _ -> BoolVal B.True
      Top -> BoolVal B.Top
      _ -> BoolVal B.False
    IsCons -> returnA -< case x of
      ListVal (Cons _ _) -> BoolVal B.True
      ListVal (ConsNil _ _) -> BoolVal B.Top
      Top -> BoolVal B.Top
      _ -> BoolVal B.False
    IsNull -> returnA -< case x of
      ListVal Nil -> BoolVal B.True
      ListVal (ConsNil _ _) -> BoolVal B.Top
      Top -> BoolVal B.Top
      _ -> BoolVal B.False
    IsZero -> numToBool -< (op,x)
    IsPositive -> numToBool -< (op,x)
    IsNegative -> numToBool -< (op,x)
    IsOdd -> numToBool -< (op,x)
    IsEven -> numToBool -< (op,x)
    Abs -> numToNum -< (op,x)
    Floor -> numToNum' -< (op,x)
    Ceiling -> numToNum' -< (op,x)
    Log -> numToFloat -< (op,x)
    Not -> boolToBool B.not -< (op,x)
    Car -> car' -< x
    Cdr -> cdr' -< x
    Caar -> car' <<< car' -< x
    Cadr -> car' <<< cdr' -< x
    Cddr -> cdr' <<< cdr' -< x
    Caddr -> car' <<< cdr' <<< cdr' -< x
    Cadddr -> car' <<< cdr' <<< cdr' <<< cdr' -< x
    Error -> failString -< printf "error: %s" (show x)
    Random -> intToInt -< (op, x)
    NumberToString -> numToString -< (op, x)
    StringToSymbol -> stringToSym -< (op, x)
    SymbolToString -> symToString -< (op, x)
  {-# INLINABLE op1_ #-}
  {-# SCC op1_ #-}

  op2_ = proc (op, x, y) -> case op of
    Eqv -> returnA -< BoolVal $ eq x y
    Quotient -> intIntToInt -< (op,x,y)
    Remainder -> intIntToInt -< (op,x,y)
    Modulo -> intIntToInt -< (op,x,y)
    StringRef -> stringIntToChar -< (op,x,y)
  {-# INLINEABLE op2_ #-}
  {-# SCC op2_ #-}

  opvar_ = proc (op, xs) -> case op of
    Equal -> numNTo -< (op,1,xs,BoolVal $ if length xs == 1 then B.True else B.Top)
    Smaller -> numNTo -< (op,1,xs,BoolVal $ if length xs == 1 then B.True else B.Top)
    Greater -> numNTo -< (op,1,xs,BoolVal $ if length xs == 1 then B.True else B.Top)
    SmallerEqual -> numNTo -< (op,1,xs,BoolVal $ if length xs == 1 then B.True else B.Top)
    GreaterEqual -> numNTo -< (op,1,xs,BoolVal $ if length xs == 1 then B.True else B.Top)
    Max -> numNTo -< (op,1,xs,foldl1 numLub xs)
    Min -> numNTo -< (op,1,xs,foldl1 numLub xs)
    Add -> numNTo -< (op,0,xs,foldl numLub (NumVal IntVal) xs)
    Mul -> numNTo -< (op,0,xs,foldl numLub (NumVal IntVal) xs)
    Sub -> numNTo -< (op,1,xs,foldl1 numLub xs)
    Div -> do
      numNTo -< (op,1,xs,foldl1 numLubDivision xs)
    Gcd -> numNTo -< (op,0,xs,foldl numLub (NumVal IntVal) xs)
    Lcm -> numNTo -< (op,0,xs,foldl numLub (NumVal IntVal) xs)
    StringAppend -> stringNToString -< xs
  {-# INLINEABLE opvar_ #-}
  {-# SCC opvar_ #-}

if__ :: (ArrowChoice c, ArrowComplete z c) => c x z -> c y z -> c (Val,(x,y)) z
if__ f g = proc (v,(x,y)) -> case v of
    BoolVal B.False -> g -< y
    BoolVal B.Top -> (f -< x) <⊔> (g -< y)
    Top -> (f -< x) <⊔> (g -< y)
    _ -> f -< x
{-# INLINEABLE if__ #-}

numToNum :: (IsString e, Fail.Join Val c, ArrowFail e c, ArrowChoice c, ArrowComplete Val c) => c (Op1,Val) Val
numToNum = proc (op,v) -> case v of
  NumVal IntVal -> returnA -< NumVal IntVal
  NumVal FloatVal -> returnA -< NumVal FloatVal
  Top -> (returnA -< Top) <⊔> (err -< (op,v))
  _ -> err -< (op,v)
  where
    err = proc (op,v) -> failString -< printf "expected a number as argument for %s, but got %s" (show op) (show v)
{-# INLINEABLE numToNum #-}
{-# SCC numToNum #-}

numToNum' :: (IsString e, Fail.Join Val c, ArrowFail e c, ArrowChoice c, ArrowComplete Val c) => c (Op1,Val) Val
numToNum' = proc (op,v) -> case v of
  NumVal _ -> returnA -< NumVal IntVal
  Top -> (returnA -< Top) <⊔> (err -< (op,v))
  _ -> err -< (op,v)
  where
    err = proc (op,v) -> failString -< printf "expected a number as argument for %s, but got %s" (show op) (show v)
{-# INLINEABLE numToNum' #-}
{-# SCC numToNum' #-}

intToInt :: (IsString e, Fail.Join Val c, ArrowFail e c, ArrowChoice c, ArrowComplete Val c) => c (Op1,Val) Val
intToInt = proc (op,v) -> case v of
  NumVal IntVal -> returnA -< NumVal IntVal
  Top -> (returnA -< Top) <⊔> (err -< (op,v))
  _ -> err -< (op,v)
  where
    err = proc (op,v) -> failString -< printf "expected an integer as argument for %s, but got %s" (show op) (show v)
{-# INLINEABLE intToInt #-}
{-# SCC intToInt #-}

numToFloat :: (IsString e, Fail.Join Val c, ArrowFail e c, ArrowChoice c, ArrowComplete Val c) => c (Op1,Val) Val
numToFloat = proc (op,v) -> case v of
  NumVal _ -> returnA -< NumVal FloatVal
  Top -> (returnA -< NumVal FloatVal) <⊔> (err -< (op,v))
  _ -> err -< (op,v)
  where
    err = proc (op,v) -> failString -< printf "expected a number as argument for %s, but got %s" (show op) (show v)
{-# INLINEABLE numToFloat #-}
{-# SCC numToFloat #-}

numToBool :: (IsString e, Fail.Join Val c, ArrowFail e c, ArrowChoice c, ArrowComplete Val c) => c (Op1,Val) Val
numToBool = proc (op,v) -> case v of
  NumVal _ -> returnA -< BoolVal B.Top
  Top -> (returnA -< BoolVal B.Top) <⊔> (err -< (op,v))
  _ -> err -< (op,v)
  where
    err = proc (op,v) -> failString -< printf "expected a number as argument for %s, but got %s" (show op) (show v)
{-# INLINEABLE numToBool #-}
{-# SCC numToBool #-}

boolToBool :: (IsString e, Fail.Join Val c, ArrowFail e c, ArrowChoice c, ArrowComplete Val c) => (B.Bool -> B.Bool) -> c (Op1,Val) Val
boolToBool f = proc (op,v) -> case v of
  BoolVal b -> returnA -< BoolVal (f b)
  Top -> (returnA -< BoolVal B.Top) <⊔> (err -< (op,v))
  _ -> err -< (op,v)
  where
    err = proc (op,v) -> failString -< printf "expected a bool as argument for %s, but got %s" (show op) (show v)
{-# INLINEABLE boolToBool #-}
{-# SCC boolToBool #-}

numToString :: (IsString e, Fail.Join Val c, ArrowFail e c, ArrowChoice c, ArrowComplete Val c) => c (Op1,Val) Val
numToString = proc (op,v) -> case v of
  NumVal _ -> returnA -< StringVal
  Top -> (returnA -< StringVal) <⊔> (err -< (op,v))
  _ -> err -< (op,v)
  where
    err = proc (op,v) -> failString -< printf "expected a number as argument for %s, but got %s" (show op) (show v)
{-# INLINEABLE numToString #-}
{-# SCC numToString #-}

stringToSym :: (IsString e, Fail.Join Val c, ArrowFail e c, ArrowChoice c, ArrowComplete Val c) => c (Op1,Val) Val
stringToSym = proc (op,v) -> case v of
  StringVal -> returnA -< QuoteVal top
  Top -> (returnA -< QuoteVal top) <⊔> (err -< (op,v))
  _ -> err -< (op,v)
  where
    err = proc (op,v) -> failString -< printf "expected a string as argument for %s, but got %s" (show op) (show v)
{-# INLINEABLE stringToSym #-}
{-# SCC stringToSym #-}

symToString :: (IsString e, Fail.Join Val c, ArrowFail e c, ArrowChoice c, ArrowComplete Val c) => c (Op1,Val) Val
symToString = proc (op,v) -> case v of
  QuoteVal _ -> returnA -< StringVal
  Top -> (returnA -< StringVal) <⊔> (err -< (op,v))
  _ -> err -< (op,v)
  where
    err = proc (op,v) -> failString -< printf "expected a quote as argument for %s, but got %s" (show op) (show v)
{-# INLINEABLE symToString #-}
{-# SCC symToString #-}

intIntToInt :: (IsString e, Fail.Join Val c, ArrowChoice c, ArrowFail e c, ArrowComplete Val c) => c (Op2,Val,Val) Val
intIntToInt = proc (op,v1,v2) -> case (v1,v2) of
  (NumVal IntVal,NumVal IntVal) -> returnA -< NumVal IntVal
  (Top,Top) -> (returnA -< NumVal IntVal) <⊔> (err -< (op,v1,v2))
  (Top,NumVal IntVal) -> (returnA -< NumVal IntVal) <⊔> (err -< (op,v1,v2))
  (NumVal IntVal,Top) -> (returnA -< NumVal IntVal) <⊔> (err -< (op,v1,v2))
  _ -> err -< (op,v1,v2)
  where
    err = proc (op,v1,v2) -> failString -< printf "expected a two ints as arguments for %s, but got %s" (show op) (show [v1,v2])
{-# INLINEABLE intIntToInt #-}
{-# SCC intIntToInt #-}

stringIntToChar :: (IsString e, Fail.Join Val c, ArrowChoice c, ArrowFail e c, ArrowComplete Val c) => c (Op2,Val,Val) Val
stringIntToChar = proc (op,v1,v2) -> case (v1,v2) of
  (StringVal, NumVal IntVal) -> returnA -< CharVal
  (Top,Top) -> (returnA -< CharVal) <⊔> (err -< (op,v1,v2))
  (Top,NumVal IntVal) -> (returnA -< CharVal) <⊔> (err -< (op,v1,v2))
  (StringVal,Top) -> (returnA -< CharVal) <⊔> (err -< (op,v1,v2))
  _ -> err -< (op,v1,v2)
  where
    err = proc (op,v1,v2) -> failString -< printf "expected a two string and an int as arguments for %s, but got %s" (show op) (show [v1,v2])
{-# INLINEABLE stringIntToChar #-}
{-# SCC stringIntToChar #-}

car' :: (IsString e, Fail.Join Val c, Store.Join Val c, ArrowChoice c, ArrowFail e c, ArrowStore Addr Val c, ArrowComplete Val c) => c Val Val
car' = proc v -> case v of
  ListVal l -> car -< l
  Top -> (returnA -< Top) <⊔> (err -< v)
  _ -> err -< v
  where
    err = proc v -> failString -< printf "Excpeted list as argument for car, but got %s" (show v)
{-# INLINEABLE car' #-}
{-# SCC car' #-}

car :: (IsString e, Fail.Join Val c, Store.Join Val c, ArrowChoice c, ArrowFail e c, ArrowStore Addr Val c) => c List Val
car = proc v -> case v of
  Cons x _ -> do
    vals <- ArrowUtils.map read' -< toList x
    returnA -< lub vals
  Nil -> failString -< "cannot car an empty list"
  ConsNil x y -> car -< Cons x y
{-# INLINEABLE car #-}
{-# SCC car #-}

cdr' :: (IsString e, Fail.Join Val c, Store.Join Val c, ArrowChoice c, ArrowFail e c, ArrowStore Addr Val c, ArrowComplete Val c) => c Val Val
cdr' = proc v -> case v of
  ListVal l -> cdr -< l
  Top -> (returnA -< Top) <⊔> (err -< v)
  _ -> err -< v
  where
    err = proc v -> failString -< printf "Excpeted list as argument for cdr, but got %s" (show v)
{-# INLINEABLE cdr' #-}
{-# SCC cdr' #-}

cdr :: (IsString e, ArrowChoice c, ArrowFail e c, ArrowStore Addr Val c, Fail.Join Val c, Store.Join Val c) => c List Val
cdr = proc v -> case v of
  Cons _ y -> do
    vals <- ArrowUtils.map read' -< toList y
    returnA -< lub vals
  Nil -> failString -< "cannot cdr an empty list"
  ConsNil x y -> cdr -< Cons x y
{-# INLINEABLE cdr #-}
{-# SCC cdr #-}

eq :: Val -> Val -> B.Bool
eq v1 v2 = case (v1, v2) of
  (Top,_) -> B.Top
  (_,Top) -> B.Top
  (BoolVal b1,BoolVal b2) -> case (b1,b2) of
    (B.Top,_) -> B.Top
    (_,B.Top) -> B.Top
    (B.True,B.True) -> B.True
    (B.False,B.False) -> B.True
    (B.True,B.False) -> B.False
    (B.False,B.True) -> B.False
  (NumVal IntVal,NumVal IntVal) -> B.Top
  (NumVal FloatVal,NumVal FloatVal) -> B.Top
  (NumVal NumTop,NumVal _) -> B.Top
  (NumVal _,NumVal NumTop) -> B.Top
  (StringVal,StringVal) -> B.Top
  (QuoteVal sym1,QuoteVal sym2) -> case (toList sym1, toList sym2) of 
    ([x], [y]) -> if x == y then B.True else B.False
    _ -> B.Top
  (_,_) -> B.False
{-# SCC eq #-}

numNTo :: (IsString e, Fail.Join Val c, ArrowFail e c, ArrowChoice c, ArrowComplete Val c) => c (OpVar,Int,[Val],Val) Val
numNTo = proc (op,minArity,xs,ret) ->
  if minArity <= length xs
  then case lub (map isNum xs) of
    B.True -> returnA -< ret
    B.False -> err -< (op,xs)
    B.Top -> (returnA -< ret) <⊔> (err -< (op,xs))
  else failString -< printf "the operator %s requires at least %d arguments, but got %d" (show op) minArity
  where
    err = proc (op,xs) -> failString -< printf "expected a numbers as argument for %s, but got %s" (show op) (show xs)
{-# INLINEABLE numNTo #-}
{-# SCC numNTo #-}

stringNToString :: (IsString e, Fail.Join Val c, ArrowFail e c, ArrowChoice c, ArrowComplete Val c) => c [Val] Val
stringNToString = proc xs -> case xs of
  (StringVal:ys) -> stringNToString -< ys
  [] -> returnA -< StringVal
  (x:_) -> failString -< printf "Expected a String, but got %s" (show x)

numLub :: Val -> Val -> Val
numLub x y = case (x,y) of
  (NumVal FloatVal,NumVal FloatVal) -> NumVal FloatVal
  (NumVal IntVal,NumVal IntVal) -> NumVal IntVal
  (NumVal IntVal,NumVal _) -> NumVal NumTop
  (NumVal _,NumVal IntVal) -> NumVal NumTop
  (NumVal FloatVal,NumVal _) -> NumVal NumTop
  (NumVal _,NumVal FloatVal) -> NumVal NumTop
  (NumVal NumTop,NumVal NumTop) -> NumVal NumTop
  (Top,_) -> Top
  (_,Top) -> Top
  (_,_) -> Bottom
{-# SCC numLub #-}

-- | Handles the case that the result of a division may be a whole number or a
-- floating point number:
-- @
--   (integer? (/ 2 2)) -> #t
--   (integer? (/ 2 3)) -> #f
-- @
numLubDivision :: Val -> Val -> Val
numLubDivision x y = case (x,y) of
  (NumVal FloatVal,NumVal _) -> NumVal FloatVal
  (NumVal _,NumVal FloatVal) -> NumVal FloatVal
  (NumVal IntVal,NumVal IntVal) -> NumVal NumTop
  (NumVal NumTop,NumVal _) -> NumVal NumTop
  (NumVal _,NumVal NumTop) -> NumVal NumTop
  (Top,_) -> Top
  (_,Top) -> Top
  (_,_) -> Bottom
{-# SCC numLubDivision #-}

isNum :: Val -> B.Bool
isNum v = case v of
  NumVal _ -> B.True
  Top -> B.Top
  _ -> B.False
{-# SCC isNum #-}

instance (ArrowChoice c, IsString e, Fail.Join Val c, ArrowFail e c, ArrowComplete Val c)
    => ArrowComplete Val (ValueT Val c) where
  ValueT f <⊔> ValueT g = ValueT $ proc x -> (f -< x) <⊔> (g -< x)
  {-# INLINEABLE (<⊔>) #-}

instance Hashable Addr
instance Show Addr where show = show . pretty
instance Pretty Addr where
  pretty (VarA (var,ctx)) = pretty var <> viaShow ctx
  pretty (LabelA (l,ctx)) = pretty (labelVal l) <> viaShow ctx

instance Hashable Val
instance Show Val where show = show . pretty
instance Pretty Val where
  pretty (NumVal nv) = pretty nv
  pretty (BoolVal b) = pretty b
  pretty (ClosureVal cls) = pretty cls
  pretty StringVal = "string"
  pretty CharVal = "char"
  pretty (QuoteVal syms) = pretty ["'" <> sym | sym <- toList syms]
  pretty (ListVal l) = pretty l
  pretty VoidVal = "#<void>"
  pretty Top = "Top"
  pretty Bottom = "Bottom"
instance Hashable List
instance Pretty List where
  pretty Nil = "Nil"
  pretty (Cons a1 a2) = "Cons" <> parens (pretty a1 <> "," <> pretty a2)
  pretty (ConsNil a1 a2) = "Cons" <> parens (pretty a1 <> "," <> pretty a2) <> " ⊔ Nil"
instance Hashable Number
instance Show Number where show = show . pretty
instance Pretty Number where
  pretty IntVal = "Int"
  pretty FloatVal = "Float"
  pretty NumTop = "NumTop"

instance IsClosure Val (HashSet Env) where
  mapEnvironment f v = case v of
    ClosureVal c -> ClosureVal (mapEnvironment f c)
    _ -> v
  traverseEnvironment f v = case v of
    ClosureVal c -> ClosureVal <$> traverseEnvironment f c
    _ -> pure v
  {-# SCC mapEnvironment #-}
  {-# SCC traverseEnvironment #-}

storeWidening :: W.Widening Store
storeWidening s1 s2 =
  -- Because the store grows monotonically, we can assume that s1 ⊑ s2. For
  -- stabilization it remains to check that s2 ⊑ s1.
  (if s2 ⊑ s1 then Stable else Unstable, s2)

instance PreOrd Val where
  Bottom ⊑ _ = True
  _ ⊑ Top = True
  NumVal nv1 ⊑ NumVal nv2 = nv1 ⊑ nv2
  StringVal ⊑ StringVal = True
  CharVal ⊑ CharVal = True
  QuoteVal sym1 ⊑ QuoteVal sym2 = sym1 ⊑ sym2
  BoolVal b1 ⊑ BoolVal b2 = b1 ⊑ b2
  ClosureVal c1 ⊑ ClosureVal c2 = c1 ⊑ c2
  ListVal l1 ⊑ ListVal l2 = l1 ⊑ l2
  VoidVal ⊑ VoidVal = True
  _ ⊑ _ = False
  {-# SCC (⊑) #-}

instance Complete Val where
  Bottom ⊔ x = x
  x ⊔ Bottom = x
  NumVal nv1 ⊔ NumVal nv2 = NumVal (nv1 ⊔ nv2)
  StringVal ⊔ StringVal = StringVal
  CharVal ⊔ CharVal = CharVal
  QuoteVal sym1 ⊔ QuoteVal sym2 = QuoteVal (sym1 ⊔ sym2)
  BoolVal b1 ⊔ BoolVal b2 = BoolVal (b1 ⊔ b2)
  ClosureVal c1 ⊔ ClosureVal c2 = ClosureVal (c1 ⊔ c2)
  ListVal l1 ⊔ ListVal l2 = ListVal (l1 ⊔ l2)
  VoidVal ⊔ VoidVal = VoidVal
  _ ⊔ _ = Top
  {-# SCC (⊔) #-}

instance PreOrd List where
  Nil ⊑ Nil = True
  Cons x1 x2 ⊑ Cons y1 y2 = x1 ⊑ y1 && x2 ⊑ y2
  ConsNil x1 x2 ⊑ ConsNil y1 y2 = x1 ⊑ y1 && x2 ⊑ y2

  Nil ⊑ ConsNil _ _ = True
  Cons x1 x2 ⊑ ConsNil y1 y2 = x1 ⊑ y1 && x2 ⊑ y2
  _ ⊑ _ = False

instance PreOrd Number where
  IntVal ⊑ IntVal = True
  FloatVal ⊑ FloatVal = True
  _ ⊑ NumTop = True
  _ ⊑ _ = False

instance Complete List where
  Nil ⊔ Nil = Nil
  Cons x1 x2 ⊔ Cons y1 y2 = Cons (x1 ⊔ y1) (x2 ⊔ y2)
  ConsNil x1 x2 ⊔ ConsNil y1 y2 = ConsNil (x1 ⊔ y1) (x2 ⊔ y2)

  Cons x1 x2 ⊔ Nil = ConsNil x1 x2
  Nil ⊔ Cons x1 x2 = ConsNil x1 x2
  ConsNil x1 x2 ⊔ Cons y1 y2 = ConsNil (x1 ⊔ y1) (x2 ⊔ y2)
  Cons x1 x2 ⊔ ConsNil y1 y2 = ConsNil (x1 ⊔ y1) (x2 ⊔ y2)
  Nil ⊔ ConsNil y1 y2 = ConsNil y1 y2
  ConsNil y1 y2 ⊔ Nil = ConsNil y1 y2

instance Complete Number where
  IntVal ⊔ IntVal = IntVal
  IntVal ⊔ _ = NumTop
  _ ⊔ IntVal = NumTop
  FloatVal ⊔ FloatVal = FloatVal
  FloatVal ⊔ _ = NumTop
  _ ⊔ FloatVal = NumTop
  NumTop ⊔ NumTop = NumTop

instance (Identifiable s, IsString s) => IsString (HashSet s) where
  fromString = singleton . fromString

instance (Identifiable s, Pretty s) => Pretty (HashSet s) where
  pretty m = braces $ hsep (punctuate "," (pretty <$> toList m))

instance (Pretty k, Pretty v) => Pretty (HashMap k v) where
  pretty m = list [ pretty k <+> " -> " <> pretty v | (k,v) <- Map.toList m]

type In = ((Store,Errors),(Env,[Expr]))
type Out = ((Store,Errors), Terminating Val)
type In' = (Store,(Env,(Errors,[Expr])))
type Out' = (Store,(Errors,Terminating Val))
type Eval = (?sensitivity :: Int) => [(Text,Addr)] -> [LExpr] -> (CFG Expr, (Metrics In, Out'))
type Eval' = (?sensitivity :: Int) => [LExpr] -> (CFG Expr, (Metrics In, (Errors,Terminating Val)))

transform :: Profunctor c => Fix.FixpointAlgorithm (c In Out) -> Fix.FixpointAlgorithm (c In' Out')
transform = Fix.transform (L.iso (\(store,(env,(errs,exprs))) -> ((store,errs),(env,exprs)))
                                 (\((store,errs),(env,exprs)) -> (store,(env,(errs,exprs)))))
                          (L.iso (\(store,(errs,val)) -> ((store,errs),val))
                                 (\((store,errs),val) -> (store,(errs,val))))
{-# INLINE transform #-}

isFunctionBody :: In -> Bool
isFunctionBody (_,(_,e)) = case e of
  Apply _ _:_ -> True
  _ -> False
{-# INLINE isFunctionBody #-}

-- Pretty Printing of inputs and outputs

printIn :: In -> Doc ann
printIn ((store,_),(env,expr)) =
  vsep
  [ "EXPR:  " <> showFirst expr
  , "ENV:   " <> align (pretty env)
  , "STORE: " <> align (pretty store)
  ]

printOut :: Out -> Doc ann
printOut ((store,errs),val) =
  vsep
  [ "RET:   " <> pretty val
  , "STORE: " <> align (pretty store)
  , "ERRORS:" <> align (pretty errs)
  ]

printInExpr :: In -> Doc ann
printInExpr (_,(_,expr)) = "EXPR:" <+> showFirst expr

printOutVal :: Out -> Doc ann
printOutVal (_,val) = "RET:" <+> pretty val

showFirst :: Pretty x => [x] -> Doc ann
showFirst (x:_) = pretty x
showFirst [] = "[]"
