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
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-type-signatures #-}
module IntervalAnalysis where

import           Prelude

import           Data.Text (Text)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Text.Prettyprint.Doc

import           Data.Order
import           Data.Abstract.Error (Error(..))
import           Data.Abstract.Interval (Interval(..))

import           GHC.Generics(Generic)
import           Text.Printf

import           Syntax (Expr(..))

type Env = HashMap Text Val

data Val = Bottom | NumVal IV | Top deriving (Eq, Generic)

-- | 'eval' is an interpreter that approximates the forward collecting semantics:
--
-- FW :: Expr -> P(Env) -> P(Val)
-- FW e X = { eval env e | env ∈ X }
eval :: Expr -> Env -> Error String Val
eval e env = case e of
  Var x -> case M.lookup x env of
    Just v  -> return v
    Nothing -> Fail $ printf "variable %s not bound" (show x)
  NumLit n -> return $ NumVal $ Interval n n
  Add e1 e2 -> do
    v1 <- eval e1 env
    v2 <- eval e2 env
    case (v1,v2) of
      (NumVal n, NumVal m) -> return $ NumVal (add n m)
  where
    add (Interval i1 i2) (Interval j1 j2) = Interval (i1 + j1) (i2 + j2)

-- | 'evalᴮ' is an interpreter that approximates the backward collecting semantics:
--
-- BW :: Expr -> P(Env) -> P(Val) -> P(Env)
-- BW e X Y = { env ∈ X | eval env e ∈ Y }
evalᴮ :: Expr -> Env -> Val -> Error String Env
evalᴮ e env res = case e of
  NumLit n
    | n ∈ res   -> return env
    | otherwise -> return bottom
  Add e1 e2 -> do
    v1 <- eval e1 env
    v2 <- eval e2 env
    case (v1,v2) of
      (NumVal n, NumVal m) -> do
        let (n',m') = addᴮ n m res
        env1 <- evalᴮ e1 env n'
        env2 <- evalᴮ e2 env m'
        return $ env1 ⊓ env2
  where
    n ∈ NumVal (Interval i j) = i <= n && n <= j
    addᴮ n m res = _

instance PreOrd Val where
  Bottom   ⊑ _        = True
  _        ⊑ Top      = True
  NumVal x ⊑ NumVal y = x ⊑ y
  _ ⊑ _               = False

instance Complete Val where
  Bottom ⊔ y = y
  x ⊔ Bottom = x
  Top ⊔ _ = Top
  _ ⊔ Top = Top
  NumVal x ⊔ NumVal y = NumVal (x ⊔ y)

instance CoComplete Val where
  Top ⊓ y = y
  x ⊓ Top = x
  Bottom ⊓ _ = Bottom
  _ ⊓ Bottom = Bottom
  NumVal (Interval i1 i2) ⊓ NumVal (Interval j1 j2)
    | x <= y    = NumVal (Interval x y)
    | otherwise = Bottom
    where x = max i1 j1
          y = min i2 j2


instance Show Val where
  show Bottom = "⊥"
  show (NumVal iv) = show iv
  show Top    = "⊤"

instance Pretty Val where
  pretty = viaShow

type IV = Interval Int
