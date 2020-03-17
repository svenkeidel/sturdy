{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Concrete semantics of Scheme.
module ConcreteInterpreter where

import Prelude hiding (fail,(.))

import Control.Arrow
import Control.Arrow.Fail as Fail
import Control.Arrow.State as State
import Control.Arrow.Store as Store
import Control.Arrow.Store (ArrowStore, read')
import           Control.Arrow.Closure (ArrowClosure,IsClosure(..))
import qualified Control.Arrow.Closure as Cls
import Control.Arrow.Trans as Trans
import Control.Arrow.Transformer.Value
import Control.Arrow.Transformer.Concrete.FiniteEnvStore
import Control.Arrow.Transformer.Concrete.Failure
import Control.Arrow.Transformer.State

import Control.Monad.State hiding (fail, StateT, get, put)

import Data.Concrete.Error

import Data.Concrete.Closure
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import Data.Text (Text,unpack)
import Data.Profunctor
import Data.Label
import Data.Either
import qualified Data.Function as Function

import GHC.Generics(Generic)

import Syntax (Literal(..), Expr(..), Op1_(..), Op2_(..), OpVar_(..))
import GenericInterpreter
import qualified GenericInterpreter as Generic

type Env = HashMap Text Addr
type Store = HashMap Addr Val
type Addr = Int
type Cls = Closure Expr Env
data Val
  = NumVal Int
  | FloatVal Double
  | RatioVal Rational
  | BoolVal Bool
  | CharVal Char
  | StringVal Text
  | SymVal Text
  | QuoteVal Val
  | ListVal Addr Addr
  | EmptyList
  | ClosureVal Cls
  deriving (Generic, Eq)

evalConcrete' :: [State Label Expr] -> (Addr, (Store, Error String Val))
evalConcrete' es =
  Trans.run
    (Generic.runFixed Function.fix ::
       ValueT Val
         (FailureT String
           (EnvStoreT Text Addr Val
             (StateT Addr
               (->)))) [Expr] Val)
         (0, (M.empty, (M.empty, generate <$> es)))

instance (ArrowChoice c, Profunctor c, ArrowState Int c) => ArrowAlloc Addr (ValueT Val c) where
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
    Number n -> returnA -< NumVal n
    Float n -> returnA -< FloatVal n
    Ratio n -> returnA -< RatioVal n
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

  nil_ = proc _ ->
    returnA -< EmptyList

  cons_ = proc ((v1,_),(v2,_)) -> do
    a1 <- alloc -< ""
    a2 <- alloc -< ""
    write -< (a1,v1)
    write -< (a2,v2)
    returnA -< ListVal a1 a2

  op1_ = proc (op, x) -> case op of
    Number_ -> case x of
      NumVal _ -> returnA -< BoolVal True
      FloatVal _ -> returnA -< BoolVal True
      RatioVal _ -> returnA -< BoolVal True
      _ -> returnA -< BoolVal False
    Integer_ -> case x of
      (NumVal _) -> returnA -< BoolVal True
      _ -> returnA -< BoolVal False
    Float_ -> case x of
      NumVal _ -> returnA -< BoolVal True
      FloatVal _ -> returnA -< BoolVal True
      _ -> returnA -< BoolVal False
    Ratio_ -> case x of
      NumVal _ -> returnA -< BoolVal True
      FloatVal _ -> returnA -< BoolVal True
      RatioVal _ -> returnA -< BoolVal True
      _ -> returnA -< BoolVal False
    Zero -> case x of
      NumVal n -> returnA -< BoolVal (n == 0)
      FloatVal n -> returnA -< BoolVal (n == 0)
      RatioVal n -> returnA -< BoolVal (n == 0)
      _ -> fail -< "(zero?): Contract violation, expecte element of type number"
    Positive -> case x of
      NumVal n -> returnA -< BoolVal (n > 0)
      FloatVal n -> returnA -< BoolVal (n > 0)
      RatioVal n -> returnA -< BoolVal (n > 0)
      _ -> fail -< "(positive?): Contract violation, expecte element of type number"
    Negative -> case x of
      NumVal n -> returnA -< BoolVal (n < 0)
      FloatVal n -> returnA -< BoolVal (n < 0)
      RatioVal n -> returnA -< BoolVal (n < 0)
      _ -> fail -< "(negative?): Contract violation, expecte element of type number"
    Odd -> case x of
      NumVal n -> returnA -< BoolVal (n `mod` 2 == 1)
      _ -> fail -< "(odd?): Contract violation, expecte element of type int: " ++ show x
    Even -> case x of
      NumVal n -> returnA -< BoolVal (n `mod` 2 == 0)
      _ -> fail -< "(even?): Contract violation, expecte element of type int"
    Abs -> case withNum1 abs x of
      Left a -> fail -< a ++ " *"
      Right a -> returnA -< a
    Floor -> case x of
      NumVal n -> returnA -< NumVal n
      FloatVal n -> returnA -< NumVal (floor n)
      RatioVal n -> returnA -< NumVal (floor n)
      _ -> fail -< "(floor): Contract violation, epxected elements of type number"
    Ceiling -> case x of
      NumVal n -> returnA -< NumVal n
      FloatVal n -> returnA -< NumVal (ceiling n)
      RatioVal n -> returnA -< NumVal (ceiling n)
      _ -> fail -< "(ceiling): Contract violation, epxected elements of type number"
    Log -> case x of
      NumVal n -> returnA -< FloatVal $ log (fromIntegral n)
      FloatVal n -> returnA -< FloatVal $ log n
      RatioVal n -> returnA -< FloatVal $ log (fromRational n)
      _ -> fail -< "(log): Contract violation, epxected element of type number"
    Boolean -> case x of
      BoolVal _ -> returnA -< BoolVal True
      _ -> returnA -< BoolVal False
    Not -> case x of
      BoolVal n -> returnA -< BoolVal (not n)
      _ -> returnA -< BoolVal False
    Null -> case x of
      EmptyList -> returnA -< BoolVal True
      _ -> returnA -< BoolVal False
    ListS -> case x of
      ListVal _ _ -> returnA -< BoolVal True
      EmptyList -> returnA -< BoolVal True
      _ -> returnA -< BoolVal False
    ConsS -> case x of
      ListVal _ _ -> returnA -< BoolVal True
      _ -> returnA -< BoolVal False
    Car -> car -< x
    Cdr -> cdr -< x
    Caar -> car <<< car -< x
    Cadr -> car <<< cdr -< x
    Cddr -> cdr <<< cdr -< x
    Caddr -> car <<< cdr <<< cdr -< x
    Cadddr -> car <<< cdr <<< cdr <<< cdr -< x
    Error -> case x of
      StringVal s -> fail -< unpack s
      _ -> fail -< "(fail): contract violation expected string as error msg"
    Random -> fail -< "random is not implemented"
    where
      car = proc x -> case x of
        ListVal a1 _  -> do
          v <- read' -< a1
          returnA -< v
        _ -> fail -< "(car): Bad form" ++ show x
      cdr = proc x -> case x of
        ListVal _ a2 -> do
          v <- read' -< a2
          returnA -< v
        _ -> fail -< "(cdr): Bad form: " ++ show x

  op2_ = proc (op, x, y) -> case op of
    Eqv -> case (x, y) of
      (BoolVal n, BoolVal m) -> returnA -< BoolVal (n == m)
      (NumVal n, NumVal m) -> returnA -< BoolVal (n == m)
      (FloatVal n, FloatVal m) -> returnA -< BoolVal (n == m)
      (RatioVal n, RatioVal m) -> returnA -< BoolVal (n == m)
      (CharVal n, CharVal m) -> returnA -< BoolVal (n == m)
      (StringVal n, StringVal m) -> returnA -< BoolVal (n == m)
      (QuoteVal (SymVal n), QuoteVal (SymVal m)) -> returnA -< BoolVal (n == m)
      _ -> returnA -< BoolVal False
    Quotient -> case (x, y) of
      (NumVal n, NumVal m) -> returnA -< NumVal (n `quot` m)
      _ -> fail -< "(remainder): Contract violation, epxected elements of type int"
    Remainder -> case (x, y) of
      (NumVal n, NumVal m) -> returnA -< NumVal (n `rem` m)
      _ -> fail -< "(remainder): Contract violation, epxected elements of type int"
    Modulo -> case (x, y) of
      (NumVal n, NumVal m) -> returnA -< NumVal (n `mod` m)
      _ -> fail -< "(modulo): Contract violation, epxected elements of type int"
    -- Cons -> do
    --   case (x, y) of
    --     (n, ListVal []) -> returnA -< ListVal [n]
    --     (n, ListVal ns) -> returnA -< ListVal (n:ns)
    --     (n, DottedListVal ns nlast) -> returnA -< DottedListVal (n:ns) nlast
    --     (n, m) -> returnA -< DottedListVal [n] m

  opvar_ =  proc (op, xs) -> case op of
    EqualS -> case (withOrdEqHelp (==) xs) of
      Left a -> fail -< "(=): Contract violation, " ++ a
      Right a -> returnA -< a
    SmallerS -> case (withOrdEqHelp (<) xs) of
      Left a -> fail -< "(<): Contract violation, " ++ a
      Right a -> returnA -< a
    GreaterS -> case (withOrdEqHelp (>) xs) of
      Left a -> fail -< "(>): Contract violation, " ++ a
      Right a -> returnA -< a
    SmallerEqualS -> case (withOrdEqHelp (<=) xs) of
      Left a -> fail -< "(<=): Contract violation, " ++ a
      Right a -> returnA -< a
    GreaterEqualS -> case (withOrdEqHelp (>=) xs) of
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
      [] -> returnA -< NumVal 0
      _ -> case foldl (withNum2Fold (+)) (Right $ head xs) (tail xs) of
        Left a -> fail -< "(+): Contract violation, " ++ a
        Right a -> returnA -< a
    Mul -> case xs of
      [] -> returnA -< NumVal 1
      _ -> case foldl (withNum2Fold (*)) (Right $ head xs) (tail xs) of
        Left a -> fail -< "(*): Contract violation, " ++ a
        Right a -> returnA -< a
    Sub -> case xs of
      [] -> fail -< "(-): Arity missmatch, expected at least one argument"
      [NumVal x] -> returnA -< NumVal (negate x)
      [FloatVal x] -> returnA -< FloatVal (negate x)
      [RatioVal x] -> returnA -< RatioVal (negate x)
      _ -> case foldl (withNum2Fold (-)) (Right $ head xs) (tail xs) of
        Left a -> fail -< "(-): Contract violation, " ++ a
        Right a -> returnA -< a
    Div -> case xs of
      [] -> fail -< "(/): Arity missmatch, expected at least one argument"
      [NumVal 0] -> fail -< "(/): Divided by zero: " ++ show xs
      [FloatVal 0] -> fail -< "(/): Divided by zero: " ++ show xs
      [RatioVal 0] -> fail -< "(/): Divided by zero: " ++ show xs
      [NumVal n] -> returnA -< RatioVal (1 / toRational n)
      [FloatVal n] -> returnA -< RatioVal (1 / toRational n)
      [RatioVal n] -> returnA -< RatioVal (1 / toRational n)
      _ -> if foldl (||) (map checkZero xs !! 1) (tail (map checkZero xs))
           then fail -< "(/): Divided by zero: " ++ show xs
           else case foldl divHelpFold (Right $ head xs) (tail xs) of
             Left a -> fail -< "(/): Contract violation, " ++ a
             Right a -> returnA -< a
    Gcd -> case foldl (withIntFold gcd) (Right $ head xs) (tail xs) of
      Left a -> fail -< "(gcd): Contract violation, " ++ a
      Right a -> returnA -< a
    Lcm -> case foldl (withIntFold lcm) (Right $ head xs) (tail xs) of
      Left a -> fail -< "(lcm): Contract violation, " ++ a
      Right a -> returnA -< a

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
  traverseEnvironment _ (NumVal n) = pure $ NumVal n
  traverseEnvironment _ (FloatVal n) = pure $ FloatVal n
  traverseEnvironment _ (RatioVal n) = pure $ RatioVal n
  traverseEnvironment _ (BoolVal n) = pure $ BoolVal n
  traverseEnvironment _ (CharVal n) = pure $ CharVal n
  traverseEnvironment _ (StringVal n) = pure $ StringVal n
  traverseEnvironment _ (SymVal n) = pure $ SymVal n
  traverseEnvironment _ (QuoteVal n) = pure $ QuoteVal n
  traverseEnvironment _ (ListVal a1 a2) = pure $ ListVal a1 a2
  traverseEnvironment _ EmptyList = pure EmptyList
  traverseEnvironment f (ClosureVal cl) = ClosureVal <$> traverse f cl

  mapEnvironment _ (NumVal n) = NumVal n
  mapEnvironment _ (FloatVal n) = FloatVal n
  mapEnvironment _ (RatioVal n) = RatioVal n
  mapEnvironment _ (BoolVal n) = BoolVal n
  mapEnvironment _ (CharVal n) = CharVal n
  mapEnvironment _ (StringVal n) = StringVal n
  mapEnvironment _ (SymVal n) = SymVal n
  mapEnvironment _ (QuoteVal n) = QuoteVal n
  mapEnvironment _ (ListVal a1 a2) = ListVal a1 a2
  mapEnvironment _ EmptyList = EmptyList
  mapEnvironment f (ClosureVal (Closure expr env)) = ClosureVal (Closure expr (f env))


instance Show Val where
  show (NumVal n) = show n
  show (FloatVal n) = show n
  show (RatioVal n) = show n
  show (BoolVal n) = show n
  show (CharVal n) = show n
  show (StringVal n) = show n
  show (SymVal n) = show n
  show (QuoteVal n) = "'" ++ show n
  show (ListVal a1 a2) = "List" ++ show a1 ++ "," ++ show a2
  show EmptyList = "'()"
  show (ClosureVal n) = show n


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
withOrdEqHelp _ (_:[]) = Right $ BoolVal True
withOrdEqHelp op xs = do
  let xs' = zip xs (tail xs)
  let res = map (withOrdEq op) xs'
  let res' = rights res
  if (length res' == length xs - 1)
    then Right $ BoolVal $ foldl (&&) (head res') (tail res')
    else Left "Expected elements of type ord for operation"

-- OPERATION HELPER ------------------------------------------------------------
withInt :: (forall n. Integral n => n -> n -> n) -> Val -> Val -> Either String Val
withInt op v1 v2 = case (v1, v2) of
  (NumVal x, NumVal y) -> Right $ NumVal $ op x y
  _ -> Left "Expected elements of type num for operation"

withBool :: (Bool -> Bool -> Bool) -> Val -> Val -> Either String Val
withBool op v1 v2 = case (v1, v2) of
  (BoolVal x, BoolVal y) -> Right $ BoolVal $ op x y
  _ -> Left "Expected elements of type bool for operation"

withNum1 :: (forall n. Num n => n -> n) -> Val -> Either String Val
withNum1 op v = case v of
  (NumVal x) -> Right $ NumVal $ op x
  (FloatVal x) -> Right $ FloatVal $ op x
  (RatioVal x) -> Right $ RatioVal $ op x
  _ -> Left "Expected elements of type num for operation"

withNum2 :: (forall n. (Num n, Ord n)=> n -> n -> n) -> Val -> Val -> Either String Val
withNum2 op v1 v2 = case (v1, v2) of
  (NumVal x, NumVal y) -> Right $ NumVal $ op x y
  (NumVal x, FloatVal y) -> Right $ FloatVal $ op (fromIntegral x) y
  (NumVal x, RatioVal y) -> Right $ RatioVal $ op (fromIntegral x) y
  (FloatVal x, NumVal y) -> Right $ FloatVal $ op x (fromIntegral y)
  (FloatVal x, FloatVal y) -> Right $ FloatVal $ op x y
  (FloatVal x, RatioVal y) -> Right $ RatioVal $ op (toRational x) y
  (RatioVal x, NumVal y) -> Right $ RatioVal $ op x (fromIntegral y)
  (RatioVal x, FloatVal y) -> Right $ RatioVal $ op x (toRational y)
  (RatioVal x, RatioVal y) -> Right $ RatioVal $ op x y
  _ -> Left $ "Expected elements of type num for operation"

divHelp :: Val -> Val -> Either String Val
divHelp v1 v2 = case (v1, v2) of
  (NumVal x, NumVal y) | x `mod` y == 0 -> Right $ NumVal $ div x y
                       | otherwise -> Right $ RatioVal $ (toRational x) / (toRational y)
  (NumVal x, FloatVal y) -> Right $ FloatVal $ (fromIntegral x) / y
  (NumVal x, RatioVal y) -> Right $ RatioVal $ (fromIntegral x) / y
  (FloatVal x, NumVal y) -> Right $ FloatVal $ x /(fromIntegral y)
  (FloatVal x, FloatVal y) -> Right $ FloatVal $ x / y
  (FloatVal x, RatioVal y) -> Right $ RatioVal $ (toRational x) / y
  (RatioVal x, NumVal y) -> Right $ RatioVal $ x / (fromIntegral y)
  (RatioVal x, FloatVal y) -> Right $ RatioVal $ x / (toRational y)
  (RatioVal x, RatioVal y) -> Right $ RatioVal $ x / y
  _ -> Left "Expected elements of type num for operation"

withOrdEq :: (forall a. (Ord a, Eq a) => a -> a -> Bool) -> (Val, Val)-> Either String Bool
withOrdEq op (v1, v2) = case (v1, v2) of
  (NumVal x, NumVal y) -> Right $ op x y
  (NumVal x, FloatVal y) -> Right $ op (fromIntegral x) y

  (FloatVal x, NumVal y) -> Right $ op x (fromIntegral y)
  (FloatVal x, FloatVal y) -> Right $ op x y
  (FloatVal x, RatioVal y) -> Right $ op (toRational x) y
  (RatioVal x, NumVal y) -> Right $ op x (fromIntegral y)
  (RatioVal x, FloatVal y) -> Right $ op x (toRational y)
  (RatioVal x, RatioVal y) -> Right $ op x y
  _ -> Left $ "Expected elements of type ord for operation"

-- self-evaluation for Num,Bool,String,Char,Quote
-- quote-evaluation for Symbols
-- for list apply function to all elements
evalQuote :: Literal -> Val
evalQuote (Number n) = NumVal n
evalQuote (Float n) = FloatVal n
evalQuote (Ratio n) = RatioVal n
evalQuote (Bool n) = BoolVal n
evalQuote (Char n) = CharVal n
evalQuote (String n) = StringVal n
evalQuote (Symbol n) = QuoteVal (SymVal n)
evalQuote (Quote n) = QuoteVal $ litsToVals n
-- evalQuote (List ns) = ListVal $ map evalQuote ns
-- evalQuote (DottedList ns n) = DottedListVal (map evalQuote ns) (evalQuote n)

litsToVals :: Literal -> Val
litsToVals (Number n) = NumVal n
litsToVals (Float n) = FloatVal n
litsToVals (Ratio n) = RatioVal n
litsToVals (Bool n) = BoolVal n
litsToVals (Char n) = CharVal n
litsToVals (String n) = StringVal n
litsToVals (Symbol n) = SymVal n
litsToVals (Quote n) = QuoteVal $ litsToVals n
-- litsToVals (List ns) = ListVal $ map litsToVals ns
-- litsToVals (DottedList ns n) = DottedListVal (map litsToVals ns) (litsToVals n)

checkZero :: Val -> Bool
checkZero x = case x of
  (NumVal 0) -> True
  (FloatVal 0) -> True
  (RatioVal 0) -> True
  _ -> False
