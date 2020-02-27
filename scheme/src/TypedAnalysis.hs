{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
  -fspecialise-aggressively
  -flate-specialise
  -fsimpl-tick-factor=500
  -fno-warn-orphans
  -fno-warn-partial-type-signatures
#-}

-- | k-CFA analysis for PCF where numbers are approximated by intervals.
module TypedAnalysis where

import           Prelude hiding (not,Bounded,fail,(.),exp,read)

import           Control.Category
import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Fail as Fail
import           Control.Arrow.Environment as Env
import           Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Chaotic(innermost,outermost)
import           Control.Arrow.Fix.Parallel
import qualified Control.Arrow.Fix.Context as Ctx
import           Control.Arrow.Trans
import           Control.Arrow.Closure (ArrowClosure,IsClosure(..))
import qualified Control.Arrow.Closure as Cls
import           Control.Arrow.Order
import           Control.Arrow.Store
import qualified Control.Arrow.Store as Store
import qualified Control.Arrow.Utils as ArrowUtils
import           Control.Arrow.Fix.Context
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.FiniteEnvStore
import           Control.Arrow.Transformer.Abstract.LogError
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Component
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable(CacheT,Monotone)
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Fix.Metrics

import           Control.Monad.State hiding (lift,fail)
import           Control.DeepSeq

import           Data.Hashable
import           Data.Label
import           Data.Order
import           Data.Text (Text, unpack)
import           Data.Utils
import           Data.HashMap.Lazy (HashMap)
import qualified Data.Boolean as B
import qualified Data.HashMap.Lazy as Map
import           Data.HashSet(HashSet)
import           Data.Identifiable
import           Data.Text.Prettyprint.Doc

import qualified Data.Abstract.Boolean as B
import qualified Data.Abstract.Widening as W
import           Data.Abstract.Terminating(Terminating)
import           Data.Abstract.Closure (Closure)
import           Data.Abstract.DiscretePowerset (Pow)
import           Data.Abstract.CallString(CallString)

import           GHC.Exts(IsString(..),toList)
import           GHC.Generics(Generic)

import           Text.Printf

import           Syntax (Expr(App),Literal(..) ,Op1_(..),Op2_(..),OpVar_(..), apply)
import           GenericInterpreter as Generic

type Cls = Closure Expr (HashSet (HashMap Text Addr))
type Env = HashMap Text Addr
type Store = HashMap Addr Val
type Ctx = CallString Label
-- -- Input and output type of the fixpoint.

data Addr
  = VarA (Text,Ctx)
  | LabelA (Label,Ctx)
  deriving stock (Eq,Generic)
  deriving anyclass (NFData)
  deriving PreOrd via Discrete Addr

data Val
  = Top
  | IntVal
  | FloatVal
  | StringVal
  | QuoteVal
  | BoolVal B.Bool
  | ClosureVal Cls
  | ListVal List
  | Bottom
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data List
  = Nil
  | Cons (Pow Addr) (Pow Addr)
  | ConsNil (Pow Addr) (Pow Addr)
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

instance (ArrowContext Ctx c) => ArrowAlloc Addr (ValueT Val c) where
  alloc = proc var -> do
    ctx <- Ctx.askContext @Ctx -< ()
    returnA -< VarA (var,ctx)

allocLabel :: (ArrowContext Ctx c) => c Label Addr
allocLabel = proc l -> do
  ctx <- Ctx.askContext @Ctx -< ()
  returnA -< LabelA (l,ctx)

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

instance (ArrowChoice c, ArrowComplete Val c, ArrowContext Ctx c, ArrowFail e c, ArrowStore Addr Val c, ArrowEnv Text Addr c,
          Store.Join Val c, Env.Join Addr c,Store.Join Addr c,Fail.Join Val c,IsString e)
    => IsVal Val (ValueT Val c) where
  type Join y (ValueT Val c) = (ArrowComplete y (ValueT Val c),Fail.Join y c)
  lit = proc x -> case x of
    Number _ -> returnA -< IntVal
    Float _ -> returnA -< FloatVal
    Ratio _ -> returnA -< Bottom
    Bool True  -> returnA -< BoolVal B.True
    Bool False  -> returnA -< BoolVal B.False
    Char _ -> returnA -< StringVal
    String _ -> returnA -< StringVal
    Quote _ -> returnA -< QuoteVal
    _ -> returnA -< Bottom

  if_ f g = proc (v,(x,y)) -> case v of
    BoolVal B.False -> g -< y
    BoolVal B.Top -> (f -< x) <⊔> (g -< y)
    Top -> (f -< x) <⊔> (g -< y)
    _ -> f -< x

  nil_ = proc _ -> returnA -< ListVal Nil
  cons_ = proc ((v1,l1),(v2,l2)) -> do
    a1 <- allocLabel -< l1
    a2 <- allocLabel -< l2
    write -< (a1,v1)
    write -< (a2,v2)
    returnA -< ListVal (Cons (singleton a1) (singleton a2))

  op1_ = proc (op, x) -> case op of
    Number_ -> returnA -< case x of
      IntVal -> BoolVal B.True
      FloatVal -> BoolVal B.True
      Top -> BoolVal B.Top
      _ -> BoolVal B.False
    Integer_ -> returnA -< case x of
      IntVal -> BoolVal B.True
      Top -> BoolVal B.Top
      _ -> BoolVal B.False
    Float_ -> returnA -< case x of
      FloatVal -> BoolVal B.True
      Top -> BoolVal B.Top
      _ -> BoolVal B.False
    Ratio_ -> returnA -< case x of
      IntVal -> BoolVal B.Top
      FloatVal -> BoolVal B.Top
      Top -> BoolVal B.Top
      _ -> BoolVal B.False
    Boolean -> returnA -< case x of
      BoolVal _ -> BoolVal B.True
      Top -> BoolVal B.Top
      _ -> BoolVal B.False
    ListS -> returnA -< case x of
      ListVal _ -> BoolVal B.True
      Top -> BoolVal B.Top
      _ -> BoolVal B.False
    ConsS -> returnA -< case x of
      ListVal (Cons _ _) -> BoolVal B.True
      ListVal (ConsNil _ _) -> BoolVal B.Top
      Top -> BoolVal B.Top
      _ -> BoolVal B.False
    Null -> returnA -< case x of
      ListVal Nil -> BoolVal B.True
      ListVal (ConsNil _ _) -> BoolVal B.Top
      Top -> BoolVal B.Top
      _ -> BoolVal B.False
    Zero -> numToBool -< (op,x)
    Positive -> numToBool -< (op,x)
    Negative -> numToBool -< (op,x)
    Odd -> numToBool -< (op,x)
    Even -> numToBool -< (op,x)
    Abs -> numToNum -< (op,x)
    Floor -> numToNum -< (op,x)
    Ceiling -> numToNum -< (op,x)
    Log -> case x of
      IntVal -> returnA -< FloatVal
      FloatVal -> returnA -< FloatVal
      _ -> failString -< printf "expected number as argument of log, but got %s" (show x)
    Not -> case x of
      BoolVal b -> returnA -< BoolVal $ B.not b
      _ -> failString -< printf "expected boolean as argument of not, but got %s" (show x)
    Car -> car' -< x
    Cdr -> cdr' -< x
    Caar -> car' <<< car' -< x
    Cadr -> cdr' <<< car' -< x
    Cddr -> cdr' <<< cdr' -< x
    Caddr -> cdr' <<< cdr' <<< car' -< x
    Error -> failString -< printf "error: " (show x)

  op2_ = proc (op, x, y) -> case op of
    Eqv -> returnA -< BoolVal $ eq x y
    Quotient -> intIntToInt -< (op,x,y)
    Remainder -> intIntToInt -< (op,x,y)
    Modulo -> intIntToInt -< (op,x,y)

  opvar_ =  proc (op, xs) -> case op of
    EqualS -> numNTo -< (op,1,xs,BoolVal $ if length xs == 1 then B.True else B.Top)
    SmallerS -> numNTo -< (op,1,xs,BoolVal $ if length xs == 1 then B.True else B.Top)
    GreaterS -> numNTo -< (op,1,xs,BoolVal $ if length xs == 1 then B.True else B.Top)
    SmallerEqualS -> numNTo -< (op,1,xs,BoolVal $ if length xs == 1 then B.True else B.Top)
    GreaterEqualS -> numNTo -< (op,1,xs,BoolVal $ if length xs == 1 then B.True else B.Top)
    Max -> numNTo -< (op,1,xs,foldl1 numLub xs)
    Min -> numNTo -< (op,1,xs,foldl1 numLub xs)
    Add -> numNTo -< (op,0,xs,foldl numLub IntVal xs)
    Mul -> numNTo -< (op,0,xs,foldl numLub IntVal xs)
    Sub -> numNTo -< (op,1,xs,foldl1 numLub xs)
    Div -> do
      -- (integer? (/ 2 2)) -> #t
      -- (integer? (/ 2 3)) -> #f
      let x = foldr1 numLub xs
      let ret = case x of IntVal -> Top; y -> y
      numNTo -< (op,1,xs,ret)
    Gcd -> numNTo -< (op,0,xs,foldl numLub IntVal xs)
    Lcm -> numNTo -< (op,0,xs,foldl numLub IntVal xs)

numToNum :: (IsString e, Fail.Join Val c, ArrowFail e c, ArrowChoice c, ArrowComplete Val c) => c (Op1_,Val) Val
numToNum = proc (op,v) -> case v of
  IntVal -> returnA -< IntVal
  FloatVal -> returnA -< FloatVal
  Top -> (returnA -< Top) <⊔> (err -< (op,v))
  _ -> err -< (op,v)
  where
    err = proc (op,v) -> failString -< printf "expected a number as argument for %s, but got %s" (show op) (show v)

numToBool :: (IsString e, Fail.Join Val c, ArrowFail e c, ArrowChoice c, ArrowComplete Val c) => c (Op1_,Val) Val
numToBool = proc (op,v) -> case v of
  IntVal -> returnA -< BoolVal B.Top
  FloatVal -> returnA -< BoolVal B.Top
  Top -> (returnA -< BoolVal B.Top) <⊔> (err -< (op,v))
  _ -> err -< (op,v)
  where
    err = proc (op,v) -> failString -< printf "expected a number as argument for %s, but got %s" (show op) (show v)

intIntToInt :: (IsString e, Fail.Join Val c, ArrowChoice c, ArrowFail e c, ArrowComplete Val c) => c (Op2_,Val,Val) Val
intIntToInt = proc (op,v1,v2) -> case (v1,v2) of
  (IntVal,IntVal) -> returnA -< IntVal
  (Top,Top) -> (returnA -< IntVal) <⊔> (err -< (op,v1,v2))
  (Top,IntVal) -> (returnA -< IntVal) <⊔> (err -< (op,v1,v2))
  (IntVal,Top) -> (returnA -< IntVal) <⊔> (err -< (op,v1,v2))
  _ -> err -< (op,v1,v2)
  where
    err = proc (op,v1,v2) -> failString -< printf "expected a two ints as arguments for %s, but got %s" (show op) (show [v1,v2])

car' :: (IsString e, Fail.Join Val c, Store.Join Val c, ArrowChoice c, ArrowFail e c, ArrowStore Addr Val c, ArrowComplete Val c) => c Val Val
car' = proc v -> case v of
  ListVal l -> car -< l
  Top -> (returnA -< Top) <⊔> (err -< v)
  _ -> err -< v
  where
    err = proc v -> failString -< printf "Excpeted list as argument for car, but got %s" (show v)

car :: (IsString e, Fail.Join Val c, Store.Join Val c, ArrowChoice c, ArrowFail e c, ArrowStore Addr Val c) => c List Val
car = proc v -> case v of
  Cons x _ -> do
    vals <- ArrowUtils.map read' -< toList x
    returnA -< lub vals
  Nil -> failString -< "cannot car an empty list"
  ConsNil x y -> car -< Cons x y

cdr' :: (IsString e, Fail.Join Val c, Store.Join Val c, ArrowChoice c, ArrowFail e c, ArrowStore Addr Val c, ArrowComplete Val c) => c Val Val
cdr' = proc v -> case v of
  ListVal l -> cdr -< l
  Top -> (returnA -< Top) <⊔> (err -< v)
  _ -> err -< v
  where
    err = proc v -> failString -< printf "Excpeted list as argument for cdr, but got %s" (show v)

cdr :: (IsString e, ArrowChoice c, ArrowFail e c, ArrowStore Addr Val c, Fail.Join Val c, Store.Join Val c) => c List Val
cdr = proc v -> case v of
  Cons _ y -> do
    vals <- ArrowUtils.map read' -< toList y
    returnA -< lub vals
  Nil -> failString -< "cannot cdr an empty list"
  ConsNil x y -> car -< Cons x y

eq :: Val -> Val -> B.Bool
eq v1 v2 = case (v1,v2) of
  (Top,_) -> B.Top
  (_,Top) -> B.Top
  (BoolVal b1,BoolVal b2) -> case (b1,b2) of
    (B.Top,_) -> B.Top
    (_,B.Top) -> B.Top
    (B.True,B.True) -> B.True
    (B.False,B.False) -> B.True
    (B.True,B.False) -> B.False
    (B.False,B.True) -> B.False
  (IntVal,IntVal) -> B.Top
  (FloatVal,FloatVal) -> B.Top
  (StringVal,StringVal) -> B.Top
  (QuoteVal,QuoteVal) -> B.Top
  (_,_) -> B.False

numNTo :: (IsString e, Fail.Join Val c, ArrowFail e c, ArrowChoice c, ArrowComplete Val c) => c (OpVar_,Int,[Val],Val) Val
numNTo = proc (op,minArity,xs,ret) ->
  if minArity <= length xs
  then case lub (map isNum xs) of
    B.True -> returnA -< ret
    B.False -> err -< (op,xs)
    B.Top -> (returnA -< ret) <⊔> (err -< (op,xs))
  else failString -< printf "the operator %s requires at least %d arguments, but got %d" (show op) minArity
  where
    err = proc (op,xs) -> failString -< printf "expected a numbers as argument for %s, but got %s" (show op) (show xs)

numLub :: Val -> Val -> Val
numLub x y = case (x,y) of
  (FloatVal,FloatVal) -> FloatVal
  (IntVal,FloatVal) -> FloatVal
  (FloatVal,IntVal) -> FloatVal
  (IntVal,IntVal) -> IntVal
  (Top,_) -> Top
  (_,Top) -> Top
  (_,_) -> Bottom

isNum :: Val -> B.Bool
isNum v = case v of
  IntVal -> B.True
  FloatVal -> B.True
  Top -> B.Top
  _ -> B.False

instance (ArrowChoice c, IsString e, Fail.Join Val c, ArrowFail e c, ArrowComplete Val c)
    => ArrowComplete Val (ValueT Val c) where
  ValueT f <⊔> ValueT g = ValueT $ proc x -> (f -< x) <⊔> (g -< x)

instance Hashable Addr
instance Show Addr where show = show . pretty
instance Pretty Addr where
  pretty (VarA (var,ctx)) = pretty var <> viaShow ctx
  pretty (LabelA (l,ctx)) = pretty (labelVal l) <> viaShow ctx

instance Hashable Val
instance Show Val where show = show . pretty
instance Pretty Val where
  pretty IntVal = "Int"
  pretty FloatVal = "Real"
  pretty (BoolVal b) = viaShow b
  pretty (ClosureVal cls) = viaShow cls
  pretty StringVal = "String"
  pretty QuoteVal = "Quote"
  pretty (ListVal l) = pretty l
  pretty Top = "Top"
  pretty Bottom = "Bottom"
instance Hashable List
instance Pretty List where
  pretty Nil = "Nil"
  pretty (Cons a1 a2) = "Cons" <> parens (pretty a1 <> "," <> pretty a2)
  pretty (ConsNil a1 a2) = "Cons" <> parens (pretty a1 <> "," <> pretty a2) <> " ⊔ Nil"

instance IsClosure Val (HashSet Env) where
  mapEnvironment f v = case v of
    ClosureVal c -> ClosureVal (mapEnvironment f c)
    _ -> v
  traverseEnvironment f v = case v of
    ClosureVal c -> ClosureVal <$> traverseEnvironment f c
    _ -> pure v

instance PreOrd Val where
  Bottom ⊑ _ = True
  _ ⊑ Top = True
  IntVal ⊑ IntVal = True
  FloatVal ⊑ FloatVal = True
  StringVal ⊑ StringVal = True
  QuoteVal ⊑ QuoteVal = True
  BoolVal b1 ⊑ BoolVal b2 = b1 ⊑ b2
  ClosureVal c1 ⊑ ClosureVal c2 = c1 ⊑ c2
  ListVal l1 ⊑ ListVal l2 = l1 ⊑ l2
  _ ⊑ _ = False

instance Complete Val where
  Bottom ⊔ x = x
  x ⊔ Bottom = x
  IntVal ⊔ IntVal = IntVal
  FloatVal ⊔ FloatVal = FloatVal
  BoolVal b1 ⊔ BoolVal b2 = BoolVal (b1 ⊔ b2)
  ClosureVal c1 ⊔ ClosureVal c2 = ClosureVal (c1 ⊔ c2)
  StringVal ⊔ StringVal = StringVal
  QuoteVal ⊔ QuoteVal = QuoteVal
  ListVal l1 ⊔ ListVal l2 = ListVal (l1 ⊔ l2)
  _ ⊔ _ = Top

instance PreOrd List where
  Nil ⊑ Nil = True
  Cons x1 x2 ⊑ Cons y1 y2 = x1 ⊑ y1 && x2 ⊑ y2
  ConsNil x1 x2 ⊑ ConsNil y1 y2 = x1 ⊑ y1 && x2 ⊑ y2

  Nil ⊑ ConsNil _ _ = True
  Cons x1 x2 ⊑ ConsNil y1 y2 = x1 ⊑ y1 && x2 ⊑ y2
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

instance (Identifiable s, IsString s) => IsString (HashSet s) where
  fromString = singleton . fromString

instance (Identifiable s, Pretty s) => Pretty (HashSet s) where
  pretty m = braces $ hsep (punctuate "," (pretty <$> toList m))

instance (Pretty k, Pretty v) => Pretty (HashMap k v) where
  pretty m = list [ pretty k <+> " -> " <> pretty v | (k,v) <- Map.toList m]

-- OPERATION HELPER ------------------------------------------------------------
type Interp x y =
  Fix
    (ValueT Val
      (TerminatingT
        (LogErrorT (HashSet Text)
          (EnvStoreT Text Addr Val
            (FixT () ()
              (MetricsT In
                (ComponentT In
                  (StackT Stack In
                    (CacheT Monotone In Out
                      (ContextT Ctx
                        (->))))))))))) [Expr] Val x y

type In = (Store,(([Expr],Label),Env))
type Out = (Store, (HashSet Text, Terminating Val))
type Out' = (Monotone In Out, (Metrics In, Out))

{-# SPECIALIZE Generic.run_ :: Interp [Expr] Val #-}
{-# SPECIALIZE Generic.eval :: Interp [Expr] Val -> Interp Expr Val #-}

evalIntervalChaoticInner :: (?sensitivity :: Int) => [(Text,Addr)] -> [State Label Expr] -> Out'
evalIntervalChaoticInner env0 e = run (extend' (Generic.run_ :: Interp [Expr] Val))
    W.finite
    iterationStrategy
    (W.finite, W.finite)
    (Map.empty,(Map.empty,(env0,e0)))
  where
    e0 = generate (sequence e)
    iterationStrategy =
      Fix.trace printIn printOut .
      Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of [App _ _ l] -> Just l; _ -> Nothing) .
      Fix.filter apply innermost

printIn :: (Store,(Env,[Expr])) -> String
printIn (store,(env,expr)) =
  show $
  vsep
  [ "EXPR:  " <> align (showFirst expr)
  , "ENV:   " <> align (pretty env)
  , "STORE: " <> align (pretty store)
  ]
  where showFirst (x:_) = pretty x; showFirst [] = "[]"

printOut :: (Store,(HashSet Text,Terminating Val)) -> String
printOut (store,(errs,val)) =
  show $
  vsep
  [ "RET:   " <> align (pretty val)
  , "STORE: " <> align (pretty store)
  , "ERRORS:" <> align (pretty errs)
  ]

evalIntervalChaoticOuter :: (?sensitivity :: Int) => [(Text,Addr)] -> [State Label Expr] -> Out'
evalIntervalChaoticOuter env0 e = run (extend' (Generic.run_ :: Interp [Expr] Val))
    W.finite
    iterationStrategy
    (W.finite, W.finite)
    (Map.empty,(Map.empty,(env0,e0)))
  where
    e0 = generate (sequence e)
    iterationStrategy =
      -- Fix.traceShow .
      Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of [App _ _ l] -> Just l; _ -> Nothing) .
      Fix.filter apply outermost

evalIntervalParallel :: (?sensitivity :: Int) => [(Text,Addr)] -> [State Label Expr] -> Out'
evalIntervalParallel env0 e = run (extend' (Generic.run_ :: Interp [Expr] Val))
    W.finite
    iterationStrategy
    (W.finite, W.finite)
    (Map.empty,(Map.empty,(env0,e0)))
  where
    e0 = generate (sequence e)
    iterationStrategy =
      -- Fix.traceShow .
      Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of [App _ _ l] -> Just l; _ -> Nothing) .
      Fix.filter apply parallel

evalIntervalParallelADI :: (?sensitivity :: Int) => [(Text,Addr)] -> [State Label Expr] -> Out'
evalIntervalParallelADI env0 e = run (extend' (Generic.run_ :: Interp [Expr] Val))
    W.finite
    iterationStrategy
    (W.finite, W.finite)
    (Map.empty,(Map.empty,(env0,e0)))
  where
    e0 = generate (sequence e)
    iterationStrategy =
      -- Fix.traceShow .
      Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of [App _ _ l] -> Just l; _ -> Nothing) .
      Fix.filter apply parallelADI

evalIntervalChaoticInner' :: (?sensitivity::Int) => [State Label Expr] -> (Metrics (Store, (([Expr], Label), Env)), (HashSet Text, Terminating Val))
evalIntervalChaoticInner' exprs = let (_,(metrics,res)) = evalIntervalChaoticInner [] exprs in (metrics,snd res)

evalIntervalChaoticOuter':: (?sensitivity :: Int) => [State Label Expr] -> (Metrics (Store, (([Expr], Label), Env)), (HashSet Text, Terminating Val))
evalIntervalChaoticOuter' exprs = let (_,(metrics,res)) = evalIntervalChaoticOuter [] exprs in (metrics,snd res)

evalIntervalParallel':: (?sensitivity :: Int) => [State Label Expr] -> (Metrics (Store, (([Expr], Label), Env)), (HashSet Text, Terminating Val))
evalIntervalParallel' exprs = let (_,(metrics,res)) = evalIntervalParallel [] exprs in (metrics,snd res)

evalIntervalParallelADI':: (?sensitivity :: Int) => [State Label Expr] -> (Metrics (Store, (([Expr], Label), Env)), (HashSet Text, Terminating Val))
evalIntervalParallelADI' exprs = let (_,(metrics,res)) = evalIntervalParallelADI [] exprs in (metrics,snd res)

evalInterval' :: (?sensitivity :: Int) => [(Text,Addr)] -> [State Label Expr] -> (HashSet Text, Terminating Val)
evalInterval' env exprs = snd $ snd $ snd $ evalIntervalChaoticInner env exprs
