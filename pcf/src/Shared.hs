{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
module Shared where

import           Prelude hiding (succ, pred, lookup)
import           PCF (Expr)
import qualified PCF as E

import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Fail
import           Control.Arrow.Environment

import           Data.HashMap.Lazy (HashMap)
import           Data.Text (Text,unpack)
import           Text.Printf

type Env v = HashMap Text v

eval :: (ArrowChoice c, ArrowFix Expr v c, ArrowEnv Text v (Env v) c, ArrowFail String c, IsVal v c) => c Expr v
eval = fixA $ \ev -> proc e0 -> case e0 of
  E.Var x -> do
    m <- lookup -< x
    case m of
      Just v -> returnA -< v
      Nothing -> failA -< printf "variable %s not in scope" (unpack x)
  E.Lam x e -> do
    env <- getEnv -< ()
    closure -< (x, e, env)
  E.App e1 e2 -> do
    fun <- ev -< e1
    arg <- ev -< e2
    applyClosure ev -< (fun, arg)
  E.Y e -> ev -< E.App e (E.Y e)
  E.Zero -> zero -< ()
  E.Succ e -> do
    v <- ev -< e
    succ -< v
  E.Pred e -> do
    v <- ev -< e
    pred -< v
  E.IfZero e1 e2 e3 -> do
    v1 <- ev -< e1
    ifZero ev ev -< (v1, (e2, e3))

class Arrow c => IsVal v c | c -> v where
  succ :: c v v
  pred :: c v v
  zero :: c () v
  ifZero :: c x v -> c y v -> c (v, (x, y)) v

  closure :: c (Text, Expr, Env v) v
  applyClosure :: c Expr v -> c (v, v) v
