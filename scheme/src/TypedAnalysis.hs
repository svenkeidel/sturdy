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

import           Prelude hiding (not,Bounded,fail,(.),exp,read, (**))

import           Control.Category
import           Control.Arrow hiding ((<+>))
import qualified Control.Arrow.Fix as Fix
import           Control.Arrow.Fix (FixpointCombinator)
import           Control.Arrow.Fail as Fail
import qualified Control.Arrow.Fix.Context as Ctx
import           Control.Arrow.Closure (ArrowClosure,IsClosure(..))
import qualified Control.Arrow.Closure as Cls
import           Control.Arrow.Order
import           Control.Arrow.Store
import qualified Control.Arrow.Store as Store
import qualified Control.Arrow.Utils as ArrowUtils
import           Control.Arrow.Fix.Context
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.Fix.Metrics as Metric
import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow
import qualified Control.Arrow.Transformer.Abstract.FiniteEnvStore as M

import           Control.DeepSeq

import           Data.Hashed.Lazy
import           Data.Hashable(Hashable(..))
import           Data.Label
import           Data.Order
import           Data.Text (Text)
import           Data.Utils
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as SM
import qualified Data.Boolean as B
import           Data.HashSet(HashSet)
import qualified Data.HashSet as Set
import           Data.Identifiable
import           Data.Text.Prettyprint.Doc
import           Data.Profunctor
import qualified Data.Lens as L
import           Data.Coerce

import qualified Data.Abstract.MonotoneStore as S
import qualified Data.Abstract.MonotoneErrors as E
import qualified Data.Abstract.Boolean as B
import           Data.Abstract.Closure (Closure, getEnvs)
import qualified Data.Abstract.DiscretePowerset as DP
import           Data.Abstract.CallString(CallString)
import qualified Data.Abstract.Widening as W
import           Data.Abstract.Widening ((**))
import           Data.Abstract.Stable
import           Data.Abstract.Powerset (Pow)
import qualified Data.Abstract.Powerset as Pow

import           GHC.Exts(IsString(..),toList)
import           GHC.Generics(Generic)

import           Text.Printf

import           Syntax (LExpr,Expr(Apply, App),Literal(..) ,Op1(..),Op1List(..),Op2(..),OpVar(..), Expr(Let))
import qualified Syntax as Syn
import           GenericInterpreter as Generic

import           Control.Arrow.Monad
import qualified Debug.Trace as Debug
import           Data.List ((\\))
import qualified Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Fix.Stack (ArrowStack,ArrowStackDepth,ArrowStackElements,widenInput,maxDepth,reuseByMetric)
import Data.Maybe 

type Cls = Closure Expr (HashSet Env)
type Env = M.Env Text Addr
type Store = HashMap Addr (Pow Val)
type Errors = E.Errors Text
type Ctx = CallString Label

data Addr
  = VarA (Text,Label,Ctx)
  | LabelA (Label,Ctx)
  | BottomA
  deriving stock (Eq,Generic)
  deriving anyclass (NFData)
  deriving PreOrd via Discrete Addr

type Symbol = Text

data Val
  = Top
  | NumVal Number
  | StringVal
  | CharVal
  | QuoteVal (DP.Pow Symbol)
  | BoolVal B.Bool
  | ClosureVal Cls
  | ListVal List
  | VoidVal
  | Bottom
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data List
  = Nil
  | Cons (DP.Pow Addr) (DP.Pow Addr)
  | ConsNil (DP.Pow Addr) (DP.Pow Addr)
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data Number
  = IntVal
  | FloatVal
  | NumTop
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

instance (ArrowContext Ctx c) => ArrowAlloc Addr (ValueT Val c) where
  alloc = proc (var,lab) -> do
    ctx <- Ctx.askContext @Ctx -< ()
    returnA -< VarA (var,lab,ctx)
  {-# INLINE alloc #-}
  {-# SCC alloc #-}

instance (ArrowContext Ctx c) => ArrowAlloc Addr (ValueT (Pow Val) c) where
  alloc = proc (var,lab) -> do
    ctx <- Ctx.askContext @Ctx -< ()
    returnA -< VarA (var,lab,ctx)
  {-# INLINE alloc #-}
  {-# SCC alloc #-}

allocLabel :: (ArrowContext Ctx c) => c Label Addr
allocLabel = proc l -> do
  ctx <- Ctx.askContext @Ctx -< ()
  returnA -< LabelA (l,ctx)
{-# INLINE allocLabel #-}
{-# SCC allocLabel #-}

instance (ArrowChoice c, ArrowFail e c, ArrowClosure Expr Cls c)
    => ArrowClosure Expr (Pow Val) (ValueT (Pow Val) c) where
  type Join y (Pow Val) (ValueT (Pow Val) c) = (Cls.Join y Cls c, Fail.Join y c)
  closure = ValueT $ proc e -> do
    cls <- Cls.closure -< e
    returnA -< Pow.singleton $ ClosureVal cls
  apply (ValueT f) = ValueT $ proc (v,x) -> do 
    let clss = getClss $ Pow.toList v
    val <- Cls.apply f -< (lub clss,x) 
    returnA -< val 
  {-# INLINE closure #-}
  {-# INLINE apply #-}
  {-# SCC closure #-}
  {-# SCC apply #-}

instance (IsString e, ArrowChoice c, ArrowFail e c, ArrowClosure Expr Cls c)
    => ArrowClosure Expr Val (ValueT Val c) where
  type Join y Val (ValueT Val c) = (Cls.Join y Cls c, Fail.Join y c)
  closure = ValueT $ proc e -> do
    cls <- Cls.closure -< e
    returnA -< ClosureVal cls
  apply (ValueT f) = ValueT $ proc (v,x) ->
    case v of
      ClosureVal cls -> Cls.apply f -< (cls,x)
      Top -> returnA -< error "Tried to apply a function, but the closure was Top. Continuing at this point would mean that the analysis result is unsound."
      _ -> failString -< printf "Expected a closure, but got %s" (show v)
  {-# INLINE closure #-}
  {-# INLINE apply #-}
  {-# SCC closure #-}
  {-# SCC apply #-}

instance (ArrowChoice c, Profunctor c, Fail.Join Val c, Fail.Join (Pow Val) c, ArrowFail e c,
          IsString e, ArrowComplete Val c, ArrowContext Ctx c) => IsVal (Pow Val) (ValueT (Pow Val) c) where
  type Join y (ValueT (Pow Val) c) = (ArrowComplete y (ValueT (Pow Val) c),Fail.Join y c)
  lit = proc literal -> do 
    val <- liftPow lit -< literal
    returnA -< Pow.singleton val 
  {-# INLINE lit #-}
  {-# SCC lit #-}
  if_ f g = proc (vals, (if_branch, else_branch)) -> do 
    let isTrue = elem (BoolVal B.True) vals
    let isFalse = elem (BoolVal B.False) vals
    let isTop = elem (BoolVal B.Top) vals || elem Top vals || (isTrue && isFalse) -- Top not necessary 
    if__ f g -< if isTop
      then (BoolVal B.Top, (if_branch, else_branch))
      else if isFalse 
        then (BoolVal B.False, (if_branch, else_branch))
        else (BoolVal B.True, (if_branch, else_branch))
  {-# INLINE if_ #-}
  {-# SCC if_ #-}
  void = proc _ -> do 
    v <- liftPow void -< () 
    returnA -< Pow.singleton v 
  {-# INLINE void #-}
  {-# SCC void #-}
  op1_ = proc (op,vals) -> do 
    let input = Pow.crossproduct (Pow.singleton op) vals
    output <- mapA $ liftPow op1_ -< input 
    returnA -< Pow.dedup output 
  {-# INLINABLE op1_ #-}
  {-# SCC op1_ #-}
  op2_ = proc (op,vals1,vals2) -> do
    let input = fmap (\(b,c) -> (op,b,c)) (Pow.dedup $ Pow.crossproduct vals1 vals2) 
    output <- mapA $ liftPow op2_ -< input
    returnA -< Pow.dedup output 
  {-# INLINEABLE op2_ #-}
  {-# SCC op2_ #-}
  opvar_ = proc (op, vals) -> do
    let listVals = map Pow.toList vals 
    let inputVals = foldl (\x y -> [a ++ [b] | a <- x, b <- y]) [[x] | x <- head listVals] (tail listVals) 
    listOutput <- ArrowUtils.map $ liftPow $ opvar_ -< [(x, y) | x <- [op], y <- inputVals]
    let output = Pow.fromList listOutput 
    returnA -< Pow.dedup output
  {-# INLINEABLE opvar_ #-}
  {-# SCC opvar_ #-}

instance (IsString e, ArrowFail e c, ArrowComplete (Pow Val) c, Fail.Join Val c, Fail.Join (Pow Val) c, 
          ArrowChoice c, ArrowContext Ctx c, ArrowStore Addr (Pow Val) c, Store.Join (Pow Val) c ) 
    => IsList_ (Pow Val) (ValueT (Pow Val) c) where
  nil_ = proc _ -> returnA -< Pow.singleton $ ListVal Nil 
  {-# INLINE nil_ #-}
  {-# SCC nil_ #-}
  cons_ = proc ((vals1, l1), (vals2, l2)) -> do
    addr1 <- allocLabel -< l1
    addr2 <- allocLabel -< l2 
    write -< (addr1, vals1)
    write -< (addr2, vals2) 
    write -< (addr1,vals1)
    write -< (addr2,vals2)
    returnA -< Pow.singleton $ ListVal (Cons (singleton addr1) (singleton addr2))
  {-# INLINE cons_ #-}
  {-# SCC cons_ #-}
  op1list_ = proc (op,x) -> case op of 
    Car -> mapJoinA powcar' -< x
    Cdr -> mapJoinA powcdr' -< x 
    Caar -> mapJoinA powcar' <<< mapJoinA powcar' -< x
    Cadr -> mapJoinA powcar' <<< mapJoinA powcdr' -< x
    Cddr -> mapJoinA powcdr' <<< mapJoinA powcdr' -< x
    Caddr -> mapJoinA powcar' <<< mapJoinA powcdr' <<< mapJoinA powcdr' -< x
    Cadddr -> mapJoinA powcar' <<< mapJoinA powcdr' <<< mapJoinA powcdr' <<< mapJoinA powcdr' -< x
  {-# INLINE op1list_ #-}
  {-# SCC op1list_ #-}

instance (ArrowChoice c, ArrowComplete Val c, ArrowContext Ctx c, ArrowFail e c, Fail.Join Val c,IsString e)
    => IsVal (Val) (ValueT Val c) where
  type Join y (ValueT (Val) c) = (ArrowComplete y (ValueT (Val) c),Fail.Join y c)
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
  void = proc _ -> returnA -< VoidVal
  {-# INLINE void #-}
  {-# SCC void #-}
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
     -- Error -> failString -< printf "error: %s" (show x)
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

instance (ArrowChoice c, ArrowComplete Val c, ArrowContext Ctx c, ArrowFail e c, ArrowStore Addr Val c,
          Store.Join Val c, Store.Join Addr c, Fail.Join Val c, IsString e)
    => IsList_ (Val) (ValueT Val c) where
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
  op1list_ = proc (op,x) -> case op of 
    Car -> car' -< x
    Cdr -> cdr' -< x
    Caar -> car' <<< car' -< x
    Cadr -> car' <<< cdr' -< x
    Cddr -> cdr' <<< cdr' -< x
    Caddr -> car' <<< cdr' <<< cdr' -< x
    Cadddr -> car' <<< cdr' <<< cdr' <<< cdr' -< x
  {-# INLINE op1list_ #-}
  {-# SCC op1list_ #-}

if__ :: (ArrowChoice c, ArrowComplete z c) => c x z -> c y z -> c (Val,(x,y)) z
if__ f g = proc (v,(x,y)) -> case v of
  BoolVal B.False -> g -< y
  BoolVal B.Top -> (f -< x) <⊔> (g -< y)
  Top -> (f -< x) <⊔> (g -< y)
  _ -> f -< x
{-# INLINEABLE if__ #-}
{-# SCC if__ #-}

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

car :: (IsString e, Fail.Join v c, Store.Join v c, ArrowChoice c, ArrowFail e c, ArrowStore Addr v c, Complete v) => c List v
car = proc v -> case v of
  Cons x _ -> do
    vals <- ArrowUtils.map read' -< toList x
    returnA -< lub vals
  Nil -> failString -< "cannot car an empty list"
  ConsNil x y -> car -< Cons x y
{-# INLINEABLE car #-}
{-# SCC car #-}

cdr :: (IsString e, ArrowChoice c, ArrowFail e c, ArrowStore Addr v c, Fail.Join v c, Store.Join v c, Complete v) => c List v
cdr = proc v -> case v of
  Cons _ y -> do
    vals <- ArrowUtils.map read' -< toList y
    returnA -< lub vals
  Nil -> failString -< "cannot cdr an empty list"
  ConsNil x y -> cdr -< Cons x y
{-# INLINEABLE cdr #-}
{-# SCC cdr #-}

-- Pow Val: List Operations 
powcar' :: (IsString e, Fail.Join (Pow Val) c, Store.Join (Pow Val) c, ArrowChoice c, ArrowFail e c, 
            ArrowStore Addr (Pow Val) c, ArrowComplete (Pow Val) c) => c Val (Pow Val)
powcar' = proc v -> case v of
  ListVal l -> car -< l
  Top -> (returnA -< Pow.singleton Top) <⊔> (err -< v)
  _ -> err -< v
  where
    err = proc v -> failString -< printf "Expected list as argument for car, but got %s" (show v)
{-# INLINEABLE powcar' #-}
{-# SCC powcar' #-}

powcdr' :: (IsString e, Fail.Join (Pow Val) c, Store.Join (Pow Val) c, ArrowChoice c, ArrowFail e c, 
            ArrowStore Addr (Pow Val) c, ArrowComplete (Pow Val) c) => c Val (Pow Val)
powcdr' = proc v -> case v of
  ListVal l -> cdr -< l
  Top -> (returnA -< Pow.singleton Top) <⊔> (err -< v)
  _ -> err -< v
  where
    err = proc v -> failString -< printf "Expected list as argument for cdr, but got %s" (show v)
{-# INLINEABLE powcdr' #-}
{-# SCC powcdr' #-}

-- Val: List Operations 
car' :: (IsString e, Fail.Join Val c, Store.Join Val c, ArrowChoice c, ArrowFail e c, ArrowStore Addr Val c, ArrowComplete Val c) => c Val Val
car' = proc v -> case v of
  ListVal l -> car -< l
  Top -> (returnA -< Top) <⊔> (err -< v)
  _ -> err -< v
  where
    err = proc v -> failString -< printf "Expected list as argument for car, but got %s" (show v)
{-# INLINEABLE car' #-}
{-# SCC car' #-}

cdr' :: (IsString e, Fail.Join Val c, Store.Join Val c, ArrowChoice c, ArrowFail e c, ArrowStore Addr Val c, ArrowComplete Val c) => c Val Val
cdr' = proc v -> case v of
  ListVal l -> cdr -< l
  Top -> (returnA -< Top) <⊔> (err -< v)
  _ -> err -< v
  where
    err = proc v -> failString -< printf "Expected list as argument for cdr, but got %s" (show v)
{-# INLINEABLE cdr' #-}
{-# SCC cdr' #-}

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
  (QuoteVal sym1,QuoteVal sym2) -> case (sym1, sym2) of
    (DP.Pow xs, DP.Pow ys) | Set.size xs == 1 && Set.size ys == 1 -> if xs == ys then B.True else B.False
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

getClss :: [Val] -> [Cls] 
getClss (ClosureVal cls:rest) = cls:getClss rest 
getClss (_:rest) = getClss rest 
getClss _ = []
{-# SCC getClss #-}

getLists :: [Val] -> [List]
getLists (ListVal list:rest) = list:getLists rest 
getLists (_:rest) = getLists rest 
getLists _ = []

liftPow :: ValueT Val c x y -> ValueT (Pow Val) c x y
liftPow = coerce 
{-# INLINEABLE liftPow #-}
{-# SCC liftPow #-}

instance (ArrowChoice c, Fail.Join Val c, ArrowFail e c, ArrowComplete Val c)
    => ArrowComplete Val (ValueT Val c) where
  ValueT f <⊔> ValueT g = ValueT $ proc x -> (f -< x) <⊔> (g -< x)
  {-# INLINEABLE (<⊔>) #-}

instance (ArrowChoice c, Fail.Join (Pow Val) c, ArrowFail e c, ArrowComplete (Pow Val) c)
    => ArrowComplete (Pow Val) (ValueT (Pow Val) c) where
  ValueT f <⊔> ValueT g = ValueT $ proc x -> do 
    vals <- (f -< x) <⊔> (g -< x)
    returnA -< Pow.dedup vals
  {-# INLINEABLE (<⊔>) #-}  

instance Hashable Addr
instance Show Addr where show = show . pretty
instance Pretty Addr where
  pretty (VarA (var,l,ctx)) = pretty var <> pretty l <> viaShow ctx
  pretty (LabelA (l,ctx)) = pretty (labelVal l) <> viaShow ctx
  pretty (BottomA) = "BottomA"

instance Hashable Val
instance Hashable List
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
instance Pretty (Pow Val) where pretty = viaShow 
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

storeErrWidening :: W.Widening (Store,Errors)
storeErrWidening (s1,e1) (s2,e2) =
  -- Because the store grows monotonically, we can assume that s1 ⊑ s2. For
  -- stabilization it remains to check that s2 ⊑ s1.
  (if (s2,e2) ⊑ (s1,e1) then Stable else Unstable, (s2,e2))

storeErrWidening_nm :: W.Widening (Store,Errors)
storeErrWidening_nm (s1,e1) (s2,e2) = do
  let (w_store,store) = W.finite s1 s2
  let (w_err,err) = (if e2 ⊑ e1 then Stable else Unstable, e2)
  (w_store ⊔ w_err, (store,err))

widening_nm :: W.Widening ((Store, Errors), Pow Val) 
widening_nm ((s1,e1),v1) ((s2,e2),v2) = do 
  let (w_store,store) = W.finite s1 s2
  let (w_err,err) = (if e2 ⊑ e1 then Stable else Unstable, e2)
  let (w_val,val) = W.finite v1 v2
  (w_store ⊔ w_err ⊔ w_val, ((store,err),val))

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
  FloatVal ⊔ FloatVal = FloatVal
  _ ⊔ _ = NumTop

instance LowerBounded Val where 
  bottom = Bottom 

instance LowerBounded Addr where 
  bottom = BottomA 

instance (Identifiable s, IsString s) => IsString (HashSet s) where
  fromString = singleton . fromString

instance (Identifiable s, Pretty s) => Pretty (HashSet s) where
  pretty m = braces $ hsep (punctuate "," (pretty <$> toList m))

instance (Pretty k, Pretty v) => Pretty (HashMap k v) where
  pretty m = list [ pretty k <+> " -> " <> pretty v | (k,v) <- SM.toList m]

type In = ((Store,Errors), (Env, [Expr]))
type Out = ((Store,Errors), (Pow Val))
type In' = (Store,(Env,(Errors,  [Expr])))
type Out' = (Store, (Errors, (Pow Val)))
type Eval = (?sensitivity :: Int) => [(Text,Addr)] -> [LExpr] -> (CFG Expr, (Metric.Monotone In, Out'))
type Eval' = (?sensitivity :: Int) => [LExpr] -> (CFG Expr, (Metric.Monotone In, (Errors, (Pow Val))))
type Eval_nm = (?sensitivity :: Int) => [(Text,Addr)] -> [LExpr] -> (CFG Expr, (Metric.Metrics In, Out'))
type Eval_nm' = (?sensitivity :: Int) => [LExpr] -> (CFG Expr, (Metric.Metrics In, (Errors, (Pow Val))))


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

gcEntry :: In -> Bool
gcEntry (_,(_,e)) = case e of
  -- App _ _ _:_ -> True
  -- Apply _ _:_ -> False
  -- Cons _ _ _:_ -> True 
  _ -> True 
{-# INLINE gcEntry #-}

getAddrIn :: In -> HashSet Addr
getAddrIn (_, (env, _)) = Set.fromList $ SM.elems $ unhashed env 
{-# INLINE getAddrIn #-}

getAddrOut :: Out -> HashSet Addr 
getAddrOut (_,val) = do 
  let vals = Pow.toList val
  Set.fromList $ (getAddrList vals) ++ (getAddrCls vals)
{-# INLINE getAddrOut #-}

getReachable :: HashSet Addr -> Out -> HashSet Addr 
getReachable addrs ((store,errs),vals) = do 
  let addrs_ = toList addrs
  let reachable_vals = catMaybes $ map (\(addr,st) -> SM.lookup addr st) (zip addrs_ (repeat store)) --not nice: repeat
  let reachable_addrs = Set.toList $ Set.fromList $ (getAddrCls $ concat $ map Pow.toList reachable_vals) ++ (getAddrList $ concat $ map Pow.toList reachable_vals) 
  if (reachable_addrs \\ addrs_ == [])
    then addrs
    else getReachable (Set.fromList $ addrs_ ++ reachable_addrs) ((store,errs),vals)
{-# INLINE getReachable #-}

getReachableIn :: HashSet Addr -> In -> HashSet Addr 
getReachableIn addrs ((store,errs),(env,exprs)) = do 
  let addrs_ = toList addrs
  let reachable_vals = catMaybes $ map (\(addr,st) -> SM.lookup addr st) (zip addrs_ (repeat store)) --not nice: repeat
  let reachable_addrs = Set.toList $ Set.fromList $ (getAddrCls $ concat $ map Pow.toList reachable_vals) ++ (getAddrList $ concat $ map Pow.toList reachable_vals) 
  if (reachable_addrs \\ addrs_ == [])
    then addrs
    else getReachableIn (Set.fromList $ addrs_ ++ reachable_addrs) ((store,errs), (env,exprs))
{-# INLINE getReachableIn #-}

removeFromStore :: Out -> HashSet Addr -> Out
removeFromStore ((store,errs),vals) addrs = do 
  let store_updated = delete' (filter (\x -> case x of LabelA _ -> False; _ -> True) (SM.keys store \\ (toList addrs))) store 
  let store_updated = delete' (SM.keys store \\ (toList addrs)) store 
  ((store_updated,errs),vals) 
{-# INLINE removeFromStore #-}

removeFromStoreIn :: In -> HashSet Addr -> In
removeFromStoreIn ((store,errs),(env,exprs)) addrs = do 
  let store_updated = delete' (SM.keys store \\ (toList addrs)) store 
  ((store_updated,errs),(env,exprs))
{-# INLINE removeFromStoreIn #-}

getAddrCls :: [Val] -> [Addr]
getAddrCls vals = do 
  let envs = concat $ map (\x -> getEnvs x) (getClss vals) 
  let envs_ = concat $ map Set.toList envs 
  let addrs = concat $ map (\env -> SM.elems $ unhashed env) envs_
  addrs 
{-# INLINE getAddrCls #-}

getAddrList :: [Val] -> [Addr]
getAddrList vals = concat $ map (\x -> case x of Nil -> []
                                                 Cons a1 a2 -> (toList a1) ++ (toList a2)
                                                 ConsNil a1 a2 -> (toList a1) ++ (toList a2)) (getLists vals )
{-# INLINE getAddrList #-}

lookup' :: (Eq a, Hashable a) => [a] -> HashMap a b -> [Maybe b]
lookup' [] store = []
lookup' (addr:addrs) store = [SM.lookup addr store] ++ lookup' addrs store
{-# INLINE lookup' #-}

delete' :: (Eq a, Hashable a) => [a] -> HashMap a b -> HashMap a b 
delete' [] store = store 
delete' (addr:addrs) store = delete' addrs (SM.delete addr store)
{-# INLINE delete' #-}

printIn :: In -> Doc ann
printIn ((store),(env,expr)) =
  vsep
  ["EXPR:  " <> prettyList expr
  , "ENV:   " <> align (pretty (unhashed env))
  , "STORE: " <> align (pretty store)
  ]
{-# INLINE printIn #-}

printOut :: Out -> Doc ann
printOut ((store,errs),val) =
  vsep
  [ "RET:   " <> pretty val
  , "STORE: " <> align (pretty store)
  , "ERRORS:" <> align (pretty errs)
  ]
{-# INLINE printOut #-}

printAddr :: HashSet Addr -> Doc ann 
printAddr addrs = prettyList $ toList addrs 
{-# INLINE printAddr #-}