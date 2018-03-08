{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
module Shared where

import           Prelude hiding (succ, pred, lookup)
import           PCF (Expr(..))

import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Fail
import           Control.Arrow.Environment

import           Data.Text (Text,unpack)
import           Text.Printf

eval :: (ArrowChoice c, ArrowFix Expr v c, ArrowEnv Text v env c, ArrowFail String c, IsVal v c, IsClosure v env c) => c Expr v
eval = fixA $ \ev -> proc e0 -> case e0 of
  Var x -> do
    m <- lookup -< x
    case m of
      Just v -> returnA -< v
      Nothing -> failA -< printf "Variable \"%s\" not bound" (unpack x)
  Lam x e -> do
    env <- getEnv -< ()
    closure -< (Lam x e, env)
  App e1 e2 -> do
    fun <- ev -< e1
    arg <- ev -< e2
    applyClosure ev -< (fun, arg)
  Zero -> zero -< ()
  Succ e -> do
    v <- ev -< e
    succ -< v
  Pred e -> do
    v <- ev -< e
    pred -< v
  IfZero e1 e2 e3 -> do
    v1 <- ev -< e1
    ifZero ev ev -< (v1, (e2, e3))
  Y e -> do
    fun <- ev -< e
    env <- getEnv -< ()
    arg <- closure -< (Y e, env)
    applyClosure ev -< (fun, arg)

class Arrow c => IsVal v c | c -> v where
  succ :: c v v
  pred :: c v v
  zero :: c () v
  ifZero :: c x v -> c y v -> c (v, (x, y)) v

class Arrow c => IsClosure v env c | c -> env, c -> v where
  closure :: c (Expr, env) v
  applyClosure :: c Expr v -> c (v, v) v
