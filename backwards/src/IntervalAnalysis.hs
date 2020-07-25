{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module IntervalAnalysis where

import           Control.DeepSeq

import           Data.Text (Text)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Text.Prettyprint.Doc

import           Data.Order
import           Data.Abstract.Error (Error(..))
import           Data.Abstract.Interval (Interval(..))
import           Data.Abstract.DiscretePowerset (Pow)
import qualified Data.Abstract.Boolean as Abs

import           GHC.Generics(Generic)
import           Text.Printf

import           Syntax (Expr(..))

type Env = HashMap Text Val

-- TODO: Add analysis for 'Sign' domain.
data Val = Bottom | NumVal IV | BoolVal Abs.Bool | Top
  deriving (Eq, Generic, NFData)

-- | 'eval' is an interpreter that approximates the forward collecting semantics:
--
-- FW :: Expr -> P(Env) -> P(Val)
-- FW e X = { eval e env | env ∈ X }
eval :: Expr -> Env -> Error (Pow String) Val
eval e env = case e of
  Var x -> case M.lookup x env of
    Just v  -> return v
    Nothing -> fail $ printf "variable %s not bound" (show x)
  NumLit n -> return $ NumVal $ Interval n n
  Add e1 e2 -> do
    v1 <- eval e1 env
    v2 <- eval e2 env
    case (v1,v2) of
      (NumVal n, NumVal m) -> return $ NumVal (add n m)
      (Bottom, _)          -> return Bottom
      (_, Bottom)          -> return Bottom
      (_, _)               ->
        fail $ printf "expected two numbers as arguments for addition, but got %s, %s" (show v1) (show v2)
  BoolLit True  -> return $ BoolVal Abs.True
  BoolLit False -> return $ BoolVal Abs.False
  If e1 e2 e3 -> do
    v <- eval e1 env
    case v of
      BoolVal Abs.True  -> eval e2 env
      BoolVal Abs.False -> eval e3 env
      BoolVal Abs.Top   -> eval e2 env ⊔ eval e3 env
      Bottom            -> bottom
      _                 ->
        fail $ printf "expected a boolean as condition of an if expression, but got %s" (show v)
  where
    add (Interval i1 i2) (Interval j1 j2) =
      Interval (i1 + j1) (i2 + j2)

-- | 'evalᴮ' is an interpreter that approximates the backward collecting semantics:
--
-- BW :: Expr -> P(Env) -> P(Val) -> P(Env)
-- BW e X Y = { env ∈ X | eval e env ∈ Y }
evalᴮ :: Expr -> Env -> Val -> Env
evalᴮ e env res = case e of
  Var x -> M.insert x res env
  NumLit n
    | n ∈ res   -> env
    | otherwise -> bottom
  Add e1 e2 ->
    case (eval e1 env, eval e2 env, res) of
      (Success (NumVal n), Success (NumVal m), NumVal r) -> do
        let (n',m') = addᴮ n m r
        evalᴮ e1 env n' ⊓ evalᴮ e2 env m'
      _ -> bottom
  BoolLit n
    | n ∈ res   -> env
    | otherwise -> bottom
  If e1 e2 e3 ->
    case (eval e1 env) of
      (Success b@(BoolVal Abs.True)) -> do
        let env2 = evalᴮ e2 env res
        evalᴮ e1 env2 b
      (Success b@(BoolVal Abs.False)) -> do
        let env3 = evalᴮ e3 env res
        evalᴮ e1 env3 b
      (Success b@(BoolVal Abs.Top)) -> do
        let env2 = evalᴮ e2 env res
            env3 = evalᴮ e3 env res
        evalᴮ e1 (env2 ⊓ env3) b
      _ -> bottom
  where
    addᴮ i j x =
      ( NumVal i ⊓ NumVal (x - j)
      , NumVal j ⊓ NumVal (x - i)
      )

class ElementOf x xs where
  (∈) :: x -> xs -> Bool

instance ElementOf Int Val where
  _ ∈ Top                   = True
  n ∈ NumVal (Interval i j) = i <= n && n <= j
  _ ∈ _                     = False

instance ElementOf Bool Val where
  _     ∈ Top               = True
  _     ∈ BoolVal Abs.Top   = True
  True  ∈ BoolVal Abs.True  = True
  False ∈ BoolVal Abs.False = True
  _     ∈ _                 = False

instance PreOrd Val where
  Bottom    ⊑ _         = True
  _         ⊑ Top       = True
  NumVal x  ⊑ NumVal y  = x ⊑ y
  BoolVal x ⊑ BoolVal y = x ⊑ y
  _         ⊑ _         = False

instance LowerBounded Val where
  bottom = Bottom

instance UpperBounded Val where
  top = Top

instance Complete Val where
  Bottom   ⊔ y        = y
  x        ⊔ Bottom   = x
  Top      ⊔ _        = Top
  _        ⊔ Top      = Top
  NumVal x ⊔ NumVal y = NumVal (x ⊔ y)
  BoolVal x ⊔ BoolVal y = BoolVal (x ⊔ y)
  _ ⊔ _ = Top

instance CoComplete Val where
  Top      ⊓ y      = y
  x        ⊓ Top    = x
  Bottom   ⊓ _      = Bottom
  _        ⊓ Bottom = Bottom
  NumVal (Interval i1 i2) ⊓ NumVal (Interval j1 j2)
    | x <= y    = NumVal (Interval x y)
    | otherwise = Bottom
    where x = max i1 j1
          y = min i2 j2
  BoolVal Abs.Top ⊓ BoolVal y       = BoolVal y
  BoolVal x       ⊓ BoolVal Abs.Top = BoolVal x
  BoolVal x       ⊓ BoolVal y
    | x == y    = BoolVal x
    | otherwise = Bottom
  _ ⊓ _ = Bottom

instance Show Val where
  show Bottom      = "⊥"
  show (NumVal x)  = show x
  show (BoolVal x) = show x
  show Top         = "⊤"

instance Pretty Val where
  pretty = viaShow

type IV = Interval Int

iv :: Int -> Int -> Val
iv i j = NumVal (Interval i j)
