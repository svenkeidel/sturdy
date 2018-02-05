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
import qualified Data.HashMap.Lazy as M
import           Data.Hashable
import           Data.Text (Text)
import           GHC.Generics

import           PCF (Expr)
import           Shared

data Closure = Closure Text Expr Env deriving (Eq,Show,Generic)
type Env = M.HashMap Text Val

data Val = NumVal Int | ClosureVal Closure deriving (Eq, Show,Generic)

type Interp = Environment Text Val (ErrorArrow String (->))

evalConcrete :: Env -> Expr -> Error String Val
evalConcrete env e = runErrorArrow (runEnvironment eval) (env,e)

instance ArrowFix Expr Val Interp where
  fixA f = f (fixA f)

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

instance IsClosure Val Env Interp where
  closure = arr $ \(x, e, env) -> ClosureVal $ Closure x e env
  applyClosure f = proc (fun, arg) -> case fun of
    ClosureVal (Closure x body env) -> do
      env' <- extendEnv -< (x,arg,env)
      localEnv f -< (env', body)
    _ -> failA -< "Expected a closure"

instance Hashable Closure
instance Hashable Val
