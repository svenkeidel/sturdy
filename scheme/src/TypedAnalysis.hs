{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
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
import           Control.Arrow
import           Control.Arrow.Fail
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
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Component
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable(CacheT,Monotone)
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Fix.Metrics

import           Control.Monad.State hiding (lift,fail)

import           Data.Hashable
import           Data.Label
import           Data.Order
import           Data.Text (Text, unpack)
import           Data.Utils
import           Data.HashMap.Lazy (HashMap)
import qualified Data.Boolean as B
import qualified Data.HashMap.Lazy as Map
import           Data.HashSet(HashSet)

import qualified Data.Abstract.Boolean as B
import           Data.Abstract.Error (Error)
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
type In = (Store,(([Expr],Label),Env))
type Out = (Store, Terminating (Error (Pow String) Val))
type Out' = (Monotone (Store, (([Expr], Label), Env))
                      (Store, Terminating (Error (Pow String) Val)),
                      (Metrics (Store, (([Expr], Label), Env)),
                               (HashMap Addr Val,
                                Terminating (Error (Pow String)
                                            Val))))

data Addr
  = VarA (Text,Ctx)
  | LabelA (Label,Ctx)
  deriving (Eq,Generic)

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
  deriving (Eq, Generic)

data List
  = Nil
  | Cons (Pow Addr) (Pow Addr)
  | ConsNil (Pow Addr) (Pow Addr)
  deriving (Eq, Generic)

type Interp x y =
  Fix
    (ValueT Val
      (ErrorT (Pow String)
        (TerminatingT
          (EnvStoreT Text Addr Val
            (FixT () ()
              (MetricsT In
                (ComponentT In
                  (StackT Stack In
                    (CacheT Monotone In Out
                      (ContextT Ctx
                          (->))))))))))) [Expr] Val x y

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
      Fix.traceShow .
      Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of [App _ _ l] -> Just l; _ -> Nothing) .
      Fix.filter apply innermost

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
  type Join y Val (ValueT Val c) = Cls.Join y Cls c
  closure = ValueT $ proc e -> do
    cls <- Cls.closure -< e
    returnA -< ClosureVal cls
  apply (ValueT f) = ValueT $ proc (v,x) ->
    case v of
      ClosureVal cls -> Cls.apply f -< (cls,x)
      _ -> failString -< printf "Expected a closure, but got %s" (show v)

instance (ArrowChoice c, ArrowComplete Val c, ArrowContext Ctx c, ArrowFail e c, ArrowStore Addr Val c, ArrowEnv Text Addr c,
          Store.Join Val c, Env.Join Addr c,Store.Join Addr c, IsString e)
    => IsVal Val (ValueT Val c) where
  type Join y (ValueT Val c) = ArrowComplete y (ValueT Val c)
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
    BoolVal B.True -> f -< x
    BoolVal B.False -> g -< y
    BoolVal B.Top -> (f -< x) <⊔> (g -< y)
    _ -> failString -< printf "Expected a bool as condition, but got %s" (show v)

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
    Null -> returnA -< case x of
      ListVal Nil -> BoolVal B.True
      ListVal (ConsNil _ _) -> BoolVal B.Top
      Top -> BoolVal B.Top
      _ -> BoolVal B.False
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
    EqualS -> numNTo -< (op,xs,BoolVal B.Top)
    SmallerS -> numNTo -< (op,xs,BoolVal B.Top)
    GreaterS -> numNTo -< (op,xs,BoolVal B.Top)
    SmallerEqualS -> numNTo -< (op,xs,BoolVal B.Top)
    GreaterEqualS -> numNTo -< (op,xs,BoolVal B.Top)
    Max -> numNTo -< (op,xs,foldr1 numLub xs)
    Min -> numNTo -< (op,xs,foldr1 numLub xs)
    Add -> numNTo -< (op,xs,foldr1 numLub xs)
    Mul -> numNTo -< (op,xs,foldr1 numLub xs)
    Sub -> numNTo -< (op,xs,foldr1 numLub xs)
    Div -> do
      -- (integer? (/ 2 2)) -> #t
      -- (integer? (/ 2 3)) -> #f
      let x = foldr1 numLub xs
      let ret = case x of IntVal -> Top; y -> y
      numNTo -< (op,xs,ret)
    Gcd -> numNTo -< (op,xs,foldr1 numLub xs)
    Lcm -> numNTo -< (op,xs,foldr1 numLub xs)
    -- and / or not needed as it is desugared to if statements
    -- for internal use only
    And -> boolNToBool B.True (foldr1 B.and) -< (op,xs)
    Or -> boolNToBool B.False (foldr1 B.or) -< (op,xs)

numToNum :: (IsString e, ArrowFail e c, ArrowChoice c) => c (Op1_,Val) Val
numToNum = proc (op,v) -> case v of
  IntVal -> returnA -< IntVal
  FloatVal -> returnA -< FloatVal
  _ -> failString -< printf "expected a number as argument for %s, but got %s" (show op) (show v)

numToBool :: (IsString e, ArrowFail e c, ArrowChoice c) => c (Op1_,Val) Val
numToBool = proc (op,v) -> case v of
  IntVal -> returnA -< BoolVal B.Top
  FloatVal -> returnA -< BoolVal B.Top
  _ -> failString -< printf "expected a number as argument for %s, but got %s" (show op) (show v)

intIntToInt :: (IsString e, ArrowFail e c, ArrowChoice c) => c (Op2_,Val,Val) Val
intIntToInt = proc (op,v1,v2) -> case (v1,v2) of
  (IntVal,IntVal) -> returnA -< IntVal
  _ -> failString -< printf "expected a two ints as arguments for %s, but got %s" (show op) (show v1) (show v2)

car' :: (IsString e, ArrowChoice c, ArrowFail e c, ArrowStore Addr Val c, Store.Join Val c) => c Val Val
car' = proc v -> case v of
  ListVal l -> car -< l
  Top -> returnA -< Top
  _ -> failString -< printf "Excpeted list as argument for car, but got %s" (show v)

car :: (IsString e, ArrowChoice c, ArrowFail e c, ArrowStore Addr Val c, Store.Join Val c) => c List Val
car = proc v -> case v of
  Cons x _ -> do
    vals <- ArrowUtils.map read' -< toList x
    returnA -< lub vals
  Nil -> failString -< "cannot car an empty list"
  ConsNil x y -> car -< Cons x y

cdr' :: (IsString e, ArrowChoice c, ArrowFail e c, ArrowStore Addr Val c, Store.Join Val c) => c Val Val
cdr' = proc v -> case v of
  ListVal l -> cdr -< l
  Top -> returnA -< Top
  _ -> failString -< printf "Excpeted list as argument for car, but got %s" (show v)

cdr :: (IsString e, ArrowChoice c, ArrowFail e c, ArrowStore Addr Val c, Store.Join Val c) => c List Val
cdr = proc v -> case v of
  Cons _ y -> do
    vals <- ArrowUtils.map read' -< toList y
    returnA -< lub vals
  Nil -> returnA -< Bottom
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

-- equal :: (IsString e, ArrowChoice c, ArrowFail e c, ArrowStore Addr Val c, Store.Join Val c) => c (Val,Val) B.Bool
-- equal = proc (v1,v2) -> case (v1,v2) of
--   (ListVal l1,ListVal l2) -> case (l1,l2) of
--     (Nil,Nil) -> returnA -< B.True
--     (Cons _ _,Cons _ _) -> do
--       x1 <- car -< l1
--       y1 <- car -< l2
--       case eq x1 y1 of
--         B.True -> do
--           x2 <- cdr -< l1
--           y2 <- cdr -< l2
--           equal -< (x2,y2)
--         B.False -> returnA -< B.False
--         B.Top -> returnA -< B.Top
--     (ConsNil _ _,_) -> returnA -< B.Top
--     (_,ConsNil _ _) -> returnA -< B.Top
--     (_,_) -> returnA -< B.False
--   (_,_) -> returnA -< eq v1 v2
-- {-# INLINABLE equal #-}

numNTo :: (IsString e, ArrowFail e c, ArrowChoice c) => c (OpVar_,[Val],Val) Val
numNTo = proc (op,xs,ret) ->
  if all isNum xs
  then returnA -< ret
  else failString -< printf "expected a numbers as argument for %s, but got %s" (show op) (show xs)

numLub :: Val -> Val -> Val
numLub x y = case (x,y) of
  (FloatVal,FloatVal) -> FloatVal
  (IntVal,FloatVal) -> FloatVal
  (FloatVal,IntVal) -> FloatVal
  (IntVal,IntVal) -> IntVal
  (_,_) -> Top

isNum :: Val -> Bool
isNum v = case v of
  IntVal -> True
  FloatVal -> True
  _ -> False

isBool :: Val -> Maybe B.Bool
isBool v = case v of
  BoolVal b -> Just b
  _ -> Nothing

boolNToBool :: (IsString e, ArrowFail e c, ArrowChoice c) => B.Bool -> ([B.Bool] -> B.Bool) -> c (OpVar_,[Val]) Val
boolNToBool z f = proc (op,xs) -> case traverse isBool xs of
  Just [] -> returnA -< BoolVal z
  Just x -> returnA -< BoolVal (f x)
  Nothing -> failString -< printf "expected booleans as arguments for %s, but got %s" (show op) (show xs)

instance (ArrowChoice c, IsString e, ArrowFail e c, ArrowComplete Val c)
    => ArrowComplete Val (ValueT Val c) where
  ValueT f <⊔> ValueT g = ValueT $ proc x -> do
    v <- (f -< x) <⊔> (g -< x)
    case v of
      Top -> fail -< fromString "encountered top value after joining."
      _ -> returnA -< v

instance Hashable Addr
instance Show Addr where
  show (VarA (var,ctx)) = unpack var ++ show ctx
  show (LabelA (l,ctx)) = show l ++ show ctx

instance Hashable Val
instance Show Val where
  show IntVal = "Int"
  show FloatVal = "Real"
  show (BoolVal b) = show b
  show (ClosureVal cls) = show cls
  show StringVal = "String"
  show QuoteVal = "Quote"
  show (ListVal l) = show l
  show Top = "Top"
  show Bottom = "Bottom"
instance Hashable List
instance Show List where
  show Nil = "Nil"
  show (Cons a1 a2) = "Cons(" ++ show a1 ++ "," ++ show a2 ++ ")"
  show (ConsNil a1 a2) = "Cons(" ++ show a1 ++ "," ++ show a2 ++ ") ⊔ Nil"

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



-- OPERATION HELPER ------------------------------------------------------------
-- EVALUATION
evalIntervalChaoticInner':: (?sensitivity :: Int) => [State Label Expr] -> (Metrics (Store, (([Expr], Label), Env)), (Terminating (Error (Pow String) Val)))
evalIntervalChaoticInner' exprs = let (_,(metrics,res)) = evalIntervalChaoticInner [] exprs in (metrics,snd $ res)
-- {-# INLINE evalIntervalChaoticInner' #-}

evalIntervalChaoticOuter':: (?sensitivity :: Int) => [State Label Expr] -> (Metrics (Store, (([Expr], Label), Env)), (Terminating (Error (Pow String) Val)))
evalIntervalChaoticOuter' exprs = let (_,(metrics,res)) = evalIntervalChaoticOuter [] exprs in (metrics,snd $ res)
-- {-# INLINE evalIntervalChaoticOuter' #-}

evalIntervalParallel':: (?sensitivity :: Int) => [State Label Expr] -> (Metrics (Store, (([Expr], Label), Env)), (Terminating (Error (Pow String) Val)))
evalIntervalParallel' exprs = let (_,(metrics,res)) = evalIntervalParallel [] exprs in (metrics,snd $ res)
-- {-# INLINE evalIntervalParallel' #-}

evalIntervalParallelADI':: (?sensitivity :: Int) => [State Label Expr] -> (Metrics (Store, (([Expr], Label), Env)), (Terminating (Error (Pow String) Val)))
evalIntervalParallelADI' exprs = let (_,(metrics,res)) = evalIntervalParallelADI [] exprs in (metrics,snd $ res)

evalInterval' :: (?sensitivity :: Int) => [(Text,Addr)] -> [State Label Expr] -> Terminating (Error (Pow String) Val)
evalInterval' env exprs = snd $ snd $ snd $ evalIntervalChaoticInner env exprs
-- {-# INLINE evalInterval' #-}
