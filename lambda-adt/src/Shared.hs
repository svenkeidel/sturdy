{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module Shared where

import Prelude hiding (lookup)

import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Utils

import Data.Text(Text)

data Expr
  = Var Text
  | Lam Text Expr
  | App Expr Expr
  | Con Text [Expr]
  | Case Expr [(Pat,Expr)]
  | Y Expr
  deriving (Show,Eq)

data Pat = PatCon Text [Pat]
         | PatVar Text
  deriving (Show,Eq)

eval :: (ArrowChoice c, ArrowFix c, IsVal v c, IsClosure Expr v c) => c Expr v
eval = fixA $ \ev -> proc e0 -> case e0 of
  Var x -> lookup -< x
  Lam {} -> closure -< e0
  App e1 e2 -> do
    fun <- ev -< e1
    arg <- ev -< e2
    applyClosure ev -< (fun, arg)
  Y e -> ev -< App e (Y e)
  Con c es -> do
    vs <- mapA ev -< es
    con -< (c,vs)
  Case e cases -> do
    v <- ev -< e
    match ev -< (v,cases)

class Arrow c => IsVal v c | c -> v where
  lookup :: c Text v
  con :: c (Text,[v]) v
  match :: c Expr v -> c (v,[(Pat,Expr)]) v

class Arrow c => IsClosure exp v c where
  closure :: c exp v
  applyClosure :: c exp v -> c (v, v) v

-- data Val = Cls Expr Env | ConV Text [Val] deriving (Eq,Show)
-- type Env = Map Text Val
-- data Closure = Closure Expr Env deriving (Eq,Show)
