{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Concrete where

import           Prelude

import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Environment
import           Data.Error
import           Data.Environment (Env)
import           Data.Hashable
import           Data.Text (Text)
import           GHC.Generics

import           PCF (Expr(..))
import           Shared

data Closure = Closure Expr (Env Text Val) deriving (Eq,Show,Generic)

data Val = NumVal Int | ClosureVal Closure deriving (Eq, Show,Generic)

type Interp = Environment Text Val (ErrorArrow String (Fix (Env Text Val,Expr) (Error String Val)))

evalConcrete :: [(Text,Val)] -> Expr -> Error String Val
evalConcrete env e = runFix (runErrorArrow (runEnvironment eval)) (env,e)

instance IsVal Val Interp where
  succ = proc x -> case x of
    NumVal n -> returnA -< NumVal (n + 1)
    _ -> failA -< "Expected a number as argument for 'succ'"
  pred = proc x -> case x of
    NumVal n -> returnA -< NumVal (n - 1)
    _ -> failA -< "Expected a number as argument for 'pred'"
  zero = arr $ const (NumVal 0)
  ifZero f g = proc (v1, (x, y)) -> case v1 of
    NumVal 0 -> f -< x
    NumVal _ -> g -< y
    _ -> failA -< "Expected a number as condition for 'ifZero'"

instance IsClosure Val (Env Text Val) Interp where
  closure = arr $ \(e, env) -> ClosureVal $ Closure e env
  applyClosure f = proc (fun, arg) -> case fun of
    ClosureVal (Closure e env) -> case e of
      Lam x body -> do
        env' <- extendEnv -< (x,arg,env)
        localEnv f -< (env', body)
      _ -> do
        fun' <- localEnv f -< (env, e)
        applyClosure f -< (fun',arg)
    _ -> failA -< "Expected a closure"

instance Hashable Closure
instance Hashable Val
