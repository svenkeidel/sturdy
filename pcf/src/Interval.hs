{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Interval where

import           Prelude

import           Control.Arrow
import           Control.Monad.Trans.Reader
import           Data.InfiniteNumbers
import           Data.Interval
import qualified Data.Map as M
import           Data.Order
import           Data.Text (Text)

import           PCF (Expr (Lam))
import           Shared hiding (Env)
import           Utils

data Val = Num (Interval (InfiniteNumber Int)) | Closure Expr Env deriving (Eq, Show)
type Env = M.Map Text Val

type Interp = Kleisli (ReaderT Env Maybe)

evalInterval :: Env -> Expr -> Maybe Val
evalInterval env e = runReaderT (runKleisli eval e) env

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
    Num n -> returnA -< Num $ withBounds1 (\x -> x + 1) n
    _ -> failA -< "Expected a number as argument for 'succ'"
  pred = proc x -> case x of
    Num n -> returnA -< Num $ withBounds1 (\x -> x - 1) n
    _ -> failA -< "Expected a number as argument for 'pred'"
  zero = arr $ const (Num (constant 0))
  ifZero f g = proc (Num (IV (i1, i2)), (x, y)) ->
    if (i1, i2) == (0, 0)
      then f -< x
      else if i1 > 0 || i2 < 0
        then g -< y
        else (f -< x) ⊔ (g -< y)
  closure = arr $ \(e, env) -> Closure e env
  applyClosure f = proc (fun, arg) -> case (fun, arg) of
    (Closure (Lam x _ body) env, Num _) -> localA f -< (M.insert x arg env, body)
    _ -> failA -< "Expected a closure and an interval argument"

-- TODO: instance Complete (Interp a b) where ...
