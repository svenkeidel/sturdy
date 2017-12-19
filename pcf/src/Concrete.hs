{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Concrete where

import           Prelude

import           Control.Arrow
import           Control.Monad.Trans.Reader

import qualified Data.Map as M
import           Data.Text (Text)

import           PCF (Expr (Lam))
import           Shared hiding (Env)
import           Utils

data Val = Num Int | Closure Expr Env deriving (Eq, Show)
type Env = M.Map Text Val

-- ReaderT (Env Maybe a) = Env -> Maybe a
type Interp = Kleisli (ReaderT Env Maybe)

evalConcrete :: Env -> Expr -> Maybe Val
evalConcrete env e = runReaderT (runKleisli eval e) env

instance ArrowFix Interp where
  fixA f = f (fixA f)

instance ArrowFail Interp where
  failA = Kleisli $ const (fail "")

instance IsEnv Env Val Interp where
  getEnv = Kleisli $ const ask
  lookup = proc x -> do
    env <- getEnv -< ()
    case M.lookup x env of
      Just v -> returnA -< v
      Nothing -> failA -< "Variable '" ++ show x ++ "' not bound"

instance IsVal Val Interp where
  succ = proc x -> case x of
    Num n -> returnA -< Num (n + 1)
    _ -> failA -< "Expected a number as argument for 'succ'"
  pred = proc x -> case x of
    Num n -> returnA -< Num (n - 1)
    _ -> failA -< "Expected a number as argument for 'pred'"
  zero = arr $ const (Num 0)
  ifZero f g = proc (v1, (x, y)) -> case v1 of
    Num 0 -> f -< x
    Num _ -> g -< y
    _ -> failA -< "Expected a number as condition for 'ifZero'"
  closure = arr $ \(e,env) -> Closure e env
  applyClosure f = proc (fun, arg) -> case (fun, arg) of
    (Closure (Lam x _ body) env, Num _) -> localA f -< (M.insert x arg env, body)
    _ -> failA -< "Expected a closure and a number argument"
