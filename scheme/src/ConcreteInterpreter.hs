{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Concrete semantics of Scheme.
module ConcreteInterpreter where

import           Prelude hiding (fail,(.))

import           Control.Arrow
import           Control.Arrow.Fail as Fail
import           Control.Arrow.State as State
import           Control.Arrow.Store as Store
import           Control.Arrow.Closure (ArrowClosure,IsClosure(..))
import qualified Control.Arrow.Closure as Cls
import           Control.Arrow.Trans as Trans
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Concrete.FiniteEnvStore
import           Control.Arrow.Transformer.Concrete.Failure
import           Control.Arrow.Transformer.State

import           Control.Monad.State hiding (fail, StateT, get, put)

import           Data.Concrete.Error

import           Data.Concrete.Closure
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text,pack)
import qualified Data.Text as T
import           Data.Profunctor
import           Data.Label
import           Data.Either
import qualified Data.Function as Function

import           GHC.Generics (Generic)

import           Syntax (Expr,Literal(..) ,Op1(..),Op2(..),OpVar(..),Op1List(..))
import           GenericInterpreter
import qualified GenericInterpreter as Generic

type Env = HashMap Text Addr
type Store = HashMap Addr Val
type Addr = Int
type Cls = Closure Expr Env
data Val
  = IntVal Int
  | FloatVal Double
  | RatioVal Rational
  | BoolVal Bool
  | CharVal Char
  | StringVal Text
  | SymVal Text
  | QuoteVal Val
  | ListVal List
  | ClosureVal Cls
  | VoidVal
  deriving (Generic, Eq)

data List = Cons Addr Addr | Nil
  deriving (Show, Generic, Eq)

evalConcrete' :: [State Label Expr] -> (Addr, (Store, Error String Val))
evalConcrete' es =
  let ?fixpointAlgorithm = Function.fix in
  Trans.run
    (Generic.runFixed ::
       ValueT Val
         (FailureT String
           (EnvStoreT Text Addr Val
             (StateT Addr
               (->)))) [Expr] Val)
         (0, (M.empty, (M.empty, generate <$> es)))

instance (ArrowChoice c, ArrowState Int c) => ArrowAlloc Addr (ValueT Val c) where
  alloc = proc _ -> do
      nextAddr <- get -< ()
      put -< nextAddr + 1
      returnA -< nextAddr

evalConcrete'' :: [State Label Expr] -> Either (Store, String) Val
evalConcrete'' exprs = case evalConcrete' exprs of
  (_, (store, err)) -> case err of
    Success val -> Right val
    Fail str -> Left (store, str)

-- | Concrete instance of the interface for value operations.
instance (ArrowChoice c, ArrowState Int c, ArrowStore Addr Val c, ArrowFail String c, Store.Join Val c, Fail.Join Val c)
  => IsVal Val (ValueT Val c) where
  type Join y (ValueT Val c) = ()

  lit = proc x -> case x of
    Int n -> returnA -< IntVal n
    Float n -> returnA -< FloatVal n
    Rational n -> returnA -< RatioVal n
    Bool n -> returnA -< BoolVal n
    Char n -> returnA -< CharVal n
    String n -> returnA -< StringVal n
    Quote n -> returnA -< evalQuote n
    -- List ns -> returnA -< ListVal (map litsToVals ns)
    -- DottedList ns n -> returnA -< DottedListVal (map litsToVals ns) (litsToVals n)
    _ -> fail -< "(lit): Expected type didn't match with given type" ++ show x

  if_ f g = proc (v1, (x, y)) -> case v1 of
    BoolVal False -> g -< y
    _ -> f -< x

  void = proc () -> returnA -< VoidVal

  op1_ = proc (op, x) -> case op of
    IsNumber -> case x of
      IntVal _ -> returnA -< BoolVal True
      FloatVal _ -> returnA -< BoolVal True
      RatioVal _ -> returnA -< BoolVal True
      _ -> returnA -< BoolVal False
    IsInteger -> case x of
      (IntVal _) -> returnA -< BoolVal True
      _ -> returnA -< BoolVal False
    IsFloat -> case x of
      IntVal _ -> returnA -< BoolVal True
      FloatVal _ -> returnA -< BoolVal True
      _ -> returnA -< BoolVal False
    IsRational -> case x of
      IntVal _ -> returnA -< BoolVal True
      FloatVal _ -> returnA -< BoolVal True
      RatioVal _ -> returnA -< BoolVal True
      _ -> returnA -< BoolVal False
    IsZero -> case x of
      IntVal n -> returnA -< BoolVal (n == 0)
      FloatVal n -> returnA -< BoolVal (n == 0)
      RatioVal n -> returnA -< BoolVal (n == 0)
      _ -> fail -< "(zero?): Contract violation, expecte element of type number"
    IsPositive -> case x of
      IntVal n -> returnA -< BoolVal (n > 0)
      FloatVal n -> returnA -< BoolVal (n > 0)
      RatioVal n -> returnA -< BoolVal (n > 0)
      _ -> fail -< "(positive?): Contract violation, expecte element of type number"
    IsNegative -> case x of
      IntVal n -> returnA -< BoolVal (n < 0)
      FloatVal n -> returnA -< BoolVal (n < 0)
      RatioVal n -> returnA -< BoolVal (n < 0)
      _ -> fail -< "(negative?): Contract violation, expecte element of type number"
    IsOdd -> case x of
      IntVal n -> returnA -< BoolVal (n `mod` 2 == 1)
      _ -> fail -< "(odd?): Contract violation, expecte element of type int: " ++ show x
    IsEven -> case x of
      IntVal n -> returnA -< BoolVal (n `mod` 2 == 0)
      _ -> fail -< "(even?): Contract violation, expecte element of type int"
    IsBoolean -> case x of
      BoolVal _ -> returnA -< BoolVal True
      _ -> returnA -< BoolVal False
    IsNull -> case x of
      ListVal Nil -> returnA -< BoolVal True
      _ -> returnA -< BoolVal False
    IsCons -> case x of
      ListVal (Cons _ _) -> returnA -< BoolVal True
      _ -> returnA -< BoolVal False
    Abs -> case withNum1 abs x of
      Left a -> fail -< a ++ " *"
      Right a -> returnA -< a
    Floor -> case x of
      IntVal n -> returnA -< IntVal n
      FloatVal n -> returnA -< IntVal (floor n)
      RatioVal n -> returnA -< IntVal (floor n)
      _ -> fail -< "(floor): Contract violation, epxected elements of type number"
    Ceiling -> case x of
      IntVal n -> returnA -< IntVal n
      FloatVal n -> returnA -< IntVal (ceiling n)
      RatioVal n -> returnA -< IntVal (ceiling n)
      _ -> fail -< "(ceiling): Contract violation, epxected elements of type number"
    Log -> case x of
      IntVal n -> returnA -< FloatVal $ log (fromIntegral n)
      FloatVal n -> returnA -< FloatVal $ log n
      RatioVal n -> returnA -< FloatVal $ log (fromRational n)
      _ -> fail -< "(log): Contract violation, epxected element of type number"
    Not -> case x of
      BoolVal n -> returnA -< BoolVal (not n)
      _ -> returnA -< BoolVal False
    NumberToString -> case x of
      IntVal n -> returnA -< StringVal (pack (show n))
      FloatVal n -> returnA -< StringVal (pack (show n))
      RatioVal n -> returnA -< StringVal (pack (show n))
      _ -> fail -< "(number->string): Contract violation, expected a number, but got: " ++ show x
    StringToSymbol -> case x of
      StringVal s -> returnA -< QuoteVal (SymVal s)
      _ -> fail -< "(string->symbol): Contract violation, expected a string, but got: " ++ show x
    SymbolToString -> case x of
      QuoteVal (SymVal s) -> returnA -< StringVal (pack (show s))
      _ -> fail -< "(symbol->string): Contract violation, expected a symbol, but got: " ++ show x
    -- Error -> case x of
    --   StringVal s -> fail -< unpack s
    --   _ -> fail -< "(fail): contract violation expected string as error msg"
    Random -> fail -< "random is not implemented"

  op2_ = proc (op, x, y) -> case op of
    Eqv -> case (x, y) of
      (BoolVal n, BoolVal m) -> returnA -< BoolVal (n == m)
      (IntVal n, IntVal m) -> returnA -< BoolVal (n == m)
      (FloatVal n, FloatVal m) -> returnA -< BoolVal (n == m)
      (RatioVal n, RatioVal m) -> returnA -< BoolVal (n == m)
      (CharVal n, CharVal m) -> returnA -< BoolVal (n == m)
      (StringVal n, StringVal m) -> returnA -< BoolVal (n == m)
      (QuoteVal (SymVal n), QuoteVal (SymVal m)) -> returnA -< BoolVal (n == m)
      _ -> returnA -< BoolVal False
    Quotient -> case (x, y) of
      (IntVal n, IntVal m) -> returnA -< IntVal (n `quot` m)
      _ -> fail -< "(quotient): Contract violation, epxected elements of type int"
    Remainder -> case (x, y) of
      (IntVal n, IntVal m) -> returnA -< IntVal (n `rem` m)
      _ -> fail -< "(remainder): Contract violation, epxected elements of type int"
    Modulo -> case (x, y) of
      (IntVal n, IntVal m) -> returnA -< IntVal (n `mod` m)
      _ -> fail -< "(modulo): Contract violation, epxected elements of type int"
    StringRef -> case (x, y) of
      (StringVal s, IntVal n)
        | n < T.length s -> returnA -< CharVal (T.index s n)
        | otherwise      -> fail -< "string-ref: index out of range"
      _ -> fail -< "(string-ref): Contract violation, epxected elements of type string and int"
    -- Cons -> do
    --   case (x, y) of
    --     (n, ListVal []) -> returnA -< ListVal [n]
    --     (n, ListVal ns) -> returnA -< ListVal (n:ns)
    --     (n, DottedListVal ns nlast) -> returnA -< DottedListVal (n:ns) nlast
    --     (n, m) -> returnA -< DottedListVal [n] m

  opvar_ =  proc (op, xs) -> case op of
    Equal -> case (withOrdEqHelp (==) xs) of
      Left a -> fail -< "(=): Contract violation, " ++ a
      Right a -> returnA -< a
    Smaller -> case (withOrdEqHelp (<) xs) of
      Left a -> fail -< "(<): Contract violation, " ++ a
      Right a -> returnA -< a
    Greater -> case (withOrdEqHelp (>) xs) of
      Left a -> fail -< "(>): Contract violation, " ++ a
      Right a -> returnA -< a
    SmallerEqual -> case (withOrdEqHelp (<=) xs) of
      Left a -> fail -< "(<=): Contract violation, " ++ a
      Right a -> returnA -< a
    GreaterEqual -> case (withOrdEqHelp (>=) xs) of
      Left a -> fail -< "(>=): Contract violation, " ++ a
      Right a -> returnA -< a
    Max -> case xs of
      [] -> fail -< "(max): Arity missmatch, expected at least one argument"
      _ -> case foldl (withNum2Fold max) (Right $ head xs) (tail xs) of
        Left a -> fail -< "(max): Contract violation, " ++ a
        Right a -> returnA -< a
    Min -> case xs of
      [] -> fail -< "(min): Arity missmatch, expected at least one argument"
      _ -> case foldl (withNum2Fold min) (Right $ head xs) (tail xs) of
        Left a -> fail -< "(min): Contract violation, " ++ a
        Right a -> returnA -< a
    Add -> case xs of
      [] -> returnA -< IntVal 0
      _ -> case foldl (withNum2Fold (+)) (Right $ head xs) (tail xs) of
        Left a -> fail -< "(+): Contract violation, " ++ a
        Right a -> returnA -< a
    Mul -> case xs of
      [] -> returnA -< IntVal 1
      _ -> case foldl (withNum2Fold (*)) (Right $ head xs) (tail xs) of
        Left a -> fail -< "(*): Contract violation, " ++ a
        Right a -> returnA -< a
    Sub -> case xs of
      [] -> fail -< "(-): Arity missmatch, expected at least one argument"
      [IntVal x] -> returnA -< IntVal (negate x)
      [FloatVal x] -> returnA -< FloatVal (negate x)
      [RatioVal x] -> returnA -< RatioVal (negate x)
      _ -> case foldl (withNum2Fold (-)) (Right $ head xs) (tail xs) of
        Left a -> fail -< "(-): Contract violation, " ++ a
        Right a -> returnA -< a
    Div -> case xs of
      [] -> fail -< "(/): Arity missmatch, expected at least one argument"
      [IntVal 0] -> fail -< "(/): Divided by zero: " ++ show xs
      [FloatVal 0] -> fail -< "(/): Divided by zero: " ++ show xs
      [RatioVal 0] -> fail -< "(/): Divided by zero: " ++ show xs
      [IntVal n] -> returnA -< RatioVal (1 / toRational n)
      [FloatVal n] -> returnA -< RatioVal (1 / toRational n)
      [RatioVal n] -> returnA -< RatioVal (1 / n)
      _ -> if foldl (||) (map checkZero xs !! 1) (tail (map checkZero xs))
           then fail -< "(/): Divided by zero: " ++ show xs
           else case foldl divHelpFold (Right $ head xs) (tail xs) of
             Left a -> fail -< "(/): Contract violation, " ++ a
             Right a -> returnA -< a
    Gcd -> case traverse matchInt xs of
      Left a -> fail -< "(gcd): Contract violation, " ++ show a
      Right as -> returnA -< IntVal (foldl gcd 0 as)
    Lcm -> case traverse matchInt xs of
      Left a -> fail -< "(lcm): Contract violation, " ++ show a
      Right as -> returnA -< IntVal (foldl lcm 1 as)
    StringAppend -> case traverse matchString xs of
      Left a -> fail -< "(string-append): Contract violation, " ++ show a
      Right as -> returnA -< StringVal (T.concat as)

instance (ArrowChoice c, ArrowState Int c, ArrowStore Addr Val c, ArrowFail String c, Store.Join Val c, Fail.Join Val c)
  => IsList_ Val (ValueT Val c) where

  nil_ = proc _ ->
    returnA -< ListVal Nil

  cons_ = proc ((v1,l1),(v2,l2)) -> do
    a1 <- alloc -< ("",l1)
    a2 <- alloc -< ("",l2)
    write -< (a1,v1)
    write -< (a2,v2)
    returnA -< ListVal (Cons a1 a2)
  
  op1list_ = proc (op,x) -> case op of 
    Car -> car -< x
    Cdr -> cdr -< x
    Caar -> car <<< car -< x
    Cadr -> car <<< cdr -< x
    Cddr -> cdr <<< cdr -< x
    Caddr -> car <<< cdr <<< cdr -< x
    Cadddr -> car <<< cdr <<< cdr <<< cdr -< x
    where
      car = proc x -> case x of
        ListVal (Cons a1 _)  -> do
          v <- read' -< a1
          returnA -< v
        _ -> fail -< "(car): Bad form" ++ show x
      cdr = proc x -> case x of
        ListVal (Cons _ a2) -> do
          v <- read' -< a2
          returnA -< v
        _ -> fail -< "(cdr): Bad form: " ++ show x

matchInt :: Val -> Either Val Int
matchInt v = case v of
  IntVal n -> Right n
  _ -> Left v

matchString :: Val -> Either Val Text
matchString v = case v of
  StringVal x -> Right x
  _ -> Left v

-- | Concrete instance of the interface for closure operations.
instance (ArrowChoice c, ArrowFail String c, ArrowClosure Expr Cls c)
    => ArrowClosure Expr Val (ValueT Val c) where
  type Join y Val (ValueT Val c) = (Cls.Join y Cls c, Fail.Join y c)
  closure = ValueT $ rmap ClosureVal Cls.closure
  apply (ValueT f) = ValueT $ proc (v,x) -> case v of
    ClosureVal cls -> Cls.apply f -< (cls,x)
    _ -> fail -< "Expected a closure, but got: " ++ show v
  {-# INLINE closure #-}
  {-# INLINE apply #-}

instance IsClosure Val Env where
  traverseEnvironment f (ClosureVal cl) = ClosureVal <$> traverse f cl
  traverseEnvironment _ v = pure v

  mapEnvironment f (ClosureVal (Closure expr env)) = ClosureVal (Closure expr (f env))
  mapEnvironment _ v = v

instance Show Val where
  show (IntVal n) = show n
  show (FloatVal n) = show n
  show (RatioVal n) = show n
  show (BoolVal n) = show n
  show (CharVal n) = show n
  show (StringVal n) = show n
  show (SymVal n) = show n
  show (QuoteVal n) = "'" ++ show n
  show (ListVal list) = show list
  show (ClosureVal (Closure expr _)) = "Closure " ++ show expr
  show VoidVal = "#<void>"


-- FOLD HELPER -----------------------------------------------------------------
withIntFold :: (forall n. Integral n => n -> n -> n) -> Either String Val -> Val -> Either String Val
withIntFold op v1 v2 = case (v1, v2) of
  (Right x, y) -> withInt op x y
  _ -> Left "Expected elements of type num for operation"

withBoolFold :: (Bool -> Bool -> Bool) -> Either String Val -> Val -> Either String Val
withBoolFold op v1 v2 = case (v1, v2) of
  (Right x, y) -> withBool op x y
  _ -> Left "Expected elements of type bool for operation"

withNum2Fold :: (forall n. (Num n, Ord n) => n -> n -> n) -> Either String Val -> Val -> Either String Val
withNum2Fold op v1 v2 = case (v1, v2) of
  (Right x, y) -> withNum2 op x y
  _ -> Left "Expected elements of type num for operation"

divHelpFold :: Either String Val -> Val -> Either String Val
divHelpFold v1 v2 = case (v1, v2) of
  (Right x, y) -> divHelp x y
  _ -> Left "Expected elements of type num for operation"

withOrdEqHelp :: (forall a. (Ord a, Eq a) => a -> a -> Bool) -> [Val] -> Either String Val
withOrdEqHelp _ [] = Right $ BoolVal True
withOrdEqHelp _ [_] = Right $ BoolVal True
withOrdEqHelp op xs = do
  let xs' = zip xs (tail xs)
  let res = map (withOrdEq op) xs'
  let res' = rights res
  if length res' == length xs - 1
    then Right $ BoolVal $ foldl (&&) (head res') (tail res')
    else Left "Expected elements of type ord for operation"

-- OPERATION HELPER ------------------------------------------------------------
withInt :: (forall n. Integral n => n -> n -> n) -> Val -> Val -> Either String Val
withInt op v1 v2 = case (v1, v2) of
  (IntVal x, IntVal y) -> Right $ IntVal $ op x y
  _ -> Left "Expected elements of type num for operation"

withBool :: (Bool -> Bool -> Bool) -> Val -> Val -> Either String Val
withBool op v1 v2 = case (v1, v2) of
  (BoolVal x, BoolVal y) -> Right $ BoolVal $ op x y
  _ -> Left "Expected elements of type bool for operation"

withNum1 :: (forall n. Num n => n -> n) -> Val -> Either String Val
withNum1 op v = case v of
  (IntVal x) -> Right $ IntVal $ op x
  (FloatVal x) -> Right $ FloatVal $ op x
  (RatioVal x) -> Right $ RatioVal $ op x
  _ -> Left "Expected elements of type num for operation"

withNum2 :: (forall n. (Num n, Ord n)=> n -> n -> n) -> Val -> Val -> Either String Val
withNum2 op v1 v2 = case (v1, v2) of
  (IntVal x, IntVal y) -> Right $ IntVal $ op x y
  (IntVal x, FloatVal y) -> Right $ FloatVal $ op (fromIntegral x) y
  (IntVal x, RatioVal y) -> Right $ RatioVal $ op (fromIntegral x) y
  (FloatVal x, IntVal y) -> Right $ FloatVal $ op x (fromIntegral y)
  (FloatVal x, FloatVal y) -> Right $ FloatVal $ op x y
  (FloatVal x, RatioVal y) -> Right $ RatioVal $ op (toRational x) y
  (RatioVal x, IntVal y) -> Right $ RatioVal $ op x (fromIntegral y)
  (RatioVal x, FloatVal y) -> Right $ RatioVal $ op x (toRational y)
  (RatioVal x, RatioVal y) -> Right $ RatioVal $ op x y
  _ -> Left $ "Expected elements of type num for operation"

divHelp :: Val -> Val -> Either String Val
divHelp v1 v2 = case (v1, v2) of
  (IntVal x, IntVal y) | x `mod` y == 0 -> Right $ IntVal $ div x y
                       | otherwise -> Right $ RatioVal $ (toRational x) / (toRational y)
  (IntVal x, FloatVal y) -> Right $ FloatVal $ fromIntegral x / y
  (IntVal x, RatioVal y) -> Right $ RatioVal $ fromIntegral x / y
  (FloatVal x, IntVal y) -> Right $ FloatVal $ x /fromIntegral y
  (FloatVal x, FloatVal y) -> Right $ FloatVal $ x / y
  (FloatVal x, RatioVal y) -> Right $ RatioVal $ toRational x / y
  (RatioVal x, IntVal y) -> Right $ RatioVal $ x / fromIntegral y
  (RatioVal x, FloatVal y) -> Right $ RatioVal $ x / toRational y
  (RatioVal x, RatioVal y) -> Right $ RatioVal $ x / y
  _ -> Left "Expected elements of type num for operation"

withOrdEq :: (forall a. (Ord a, Eq a) => a -> a -> Bool) -> (Val, Val)-> Either String Bool
withOrdEq op (v1, v2) = case (v1, v2) of
  (IntVal x, IntVal y) -> Right $ op x y
  (IntVal x, FloatVal y) -> Right $ op (fromIntegral x) y

  (FloatVal x, IntVal y) -> Right $ op x (fromIntegral y)
  (FloatVal x, FloatVal y) -> Right $ op x y
  (FloatVal x, RatioVal y) -> Right $ op (toRational x) y
  (RatioVal x, IntVal y) -> Right $ op x (fromIntegral y)
  (RatioVal x, FloatVal y) -> Right $ op x (toRational y)
  (RatioVal x, RatioVal y) -> Right $ op x y
  _ -> Left "Expected elements of type ord for operation"

-- self-evaluation for Num,Bool,String,Char,Quote
-- quote-evaluation for Symbols
-- for list apply function to all elements
evalQuote :: Literal -> Val
evalQuote (Int n) = IntVal n
evalQuote (Float n) = FloatVal n
evalQuote (Rational n) = RatioVal n
evalQuote (Bool n) = BoolVal n
evalQuote (Char n) = CharVal n
evalQuote (String n) = StringVal n
evalQuote (Symbol n) = QuoteVal (SymVal n)
evalQuote (Quote n) = QuoteVal $ litsToVals n
-- evalQuote (List ns) = ListVal $ map evalQuote ns
-- evalQuote (DottedList ns n) = DottedListVal (map evalQuote ns) (evalQuote n)

litsToVals :: Literal -> Val
litsToVals (Int n) = IntVal n
litsToVals (Float n) = FloatVal n
litsToVals (Rational n) = RatioVal n
litsToVals (Bool n) = BoolVal n
litsToVals (Char n) = CharVal n
litsToVals (String n) = StringVal n
litsToVals (Symbol n) = SymVal n
litsToVals (Quote n) = QuoteVal $ litsToVals n
-- litsToVals (List ns) = ListVal $ map litsToVals ns
-- litsToVals (DottedList ns n) = DottedListVal (map litsToVals ns) (litsToVals n)

checkZero :: Val -> Bool
checkZero x = case x of
  (IntVal 0) -> True
  (FloatVal 0) -> True
  (RatioVal 0) -> True
  _ -> False
