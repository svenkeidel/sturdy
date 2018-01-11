{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Sign where

import           Prelude

import           Control.Arrow
import           Control.Monad.Trans.Reader
import qualified Data.Map as M
import           Data.Order
import           Data.Sign
import           Data.Text (Text)

import           PCF (Expr (Lam))
import           Shared hiding (Env)
import           Utils

data Val = Num Sign | Closure Expr Env deriving (Show)
type Env = M.Map Text Val

type Interp = Kleisli (ReaderT Env Maybe)

evalSign :: Env -> Expr -> Maybe Val
evalSign env e = runReaderT (runKleisli eval e) env

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
  succ = proc s -> case s of
    Num Positive -> returnA -< Num Positive
    Num Zero -> returnA -< Num Positive
    Num Negative -> returnA -< Num Top
    Num Top -> returnA -< Num Top
    _ -> failA -< "Expected a number as argument for 'succ'"
  pred = proc s -> case s of
    Num Positive -> returnA -< Num Top
    Num Zero -> returnA -< Num Negative
    Num Negative -> returnA -< Num Negative
    Num Top -> returnA -< Num Top
    _ -> failA -< "Expected a number as argument for 'pred'"
  zero = arr $ const (Num Zero)
  ifZero f g = proc (v1, (x, y)) -> case v1 of
    Num Zero -> f -< x
    Num Top -> (f -< x) ⊔ (g -< y)
    Num _ -> g -< y
    _ -> failA -< "Expected a number as condition for 'ifZero'"
  closure = arr $ \(e, env) -> Closure e env
  applyClosure f = proc (fun, arg) -> case (fun, arg) of
    (Closure (Lam x _ body) env, Num _) -> localA f -< (M.insert x arg env, body)
    _ -> failA -< "Expected a closure and a number argument"

-- TODO: instance Complete (Interp a b) where ...
