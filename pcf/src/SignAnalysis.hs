{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module SignAnalysis where

import           Prelude

import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Monad.Trans.Reader
import           Data.Error
import           Data.Foldable (toList)
import qualified Data.HashMap.Lazy as M
import           Data.Hashable
import           Data.Order
import           Data.Powerset
import qualified Data.Sign as Sign
import           Data.Sign hiding (Top)
import           Data.Text (Text)

import           GHC.Generics

import           PCF (Expr (Lam))
import           Shared hiding (Env)
import           Utils

data Closure = Closure Expr Env deriving (Eq,Show,Generic)
data Val = Bot | NumVal Sign | ClosureVal (Pow Closure) | Top deriving (Eq,Show,Generic)
type Env = M.HashMap Text Val

type Interp = Kleisli (ReaderT Env (Error String))

evalSign :: Env -> Expr -> Error String Val
evalSign env e = runReaderT (runKleisli eval e) env

instance ArrowFix Interp where
  fixA f = f (fixA f)

instance IsEnv Env Val Interp where
  getEnv = Kleisli $ const ask
  lookup = proc x -> do
    env <- getEnv -< ()
    case M.lookup x env of
      Just v -> returnA -< v
      Nothing -> failA -< "Variable " ++ show x ++ " not bound"

instance IsVal Val Interp where
  succ = proc s -> case s of
    NumVal n -> returnA -< NumVal (n+1)
    _ -> failA -< "Expected a number as argument for 'succ'"
  pred = proc s -> case s of
    NumVal n -> returnA -< NumVal (n-1)
    _ -> failA -< "Expected a number as argument for 'pred'"
  zero = arr $ const (NumVal 0)
  ifZero f g = proc (v1, (x, y)) -> case v1 of
    NumVal Zero -> f -< x
    NumVal Sign.Top -> (f -< x) ⊔ (g -< y)
    NumVal _ -> g -< y
    _ -> failA -< "Expected a number as condition for 'ifZero'"
  closure = arr $ \(e, env) -> ClosureVal (return (Closure e env))
  applyClosure f = proc (fun, arg) -> case fun of
    ClosureVal cls -> lubA (proc (Closure (Lam x _ body) env) -> localA f -< (M.insert x arg env, body)) -<< toList cls
    _ -> failA -< "Expected a closure"

instance PreOrd Val where
  Bot ⊑ _ = True
  _ ⊑ Top = True
  NumVal n1 ⊑ NumVal n2 = n1 ⊑ n2
  ClosureVal c1 ⊑ ClosureVal c2 = c1 ⊑ c2
  _ ⊑ _ = False

instance LowerBounded Val where
  bottom = Bot

instance Complete Val where
  Bot ⊔ y = y
  x ⊔ Bot = x
  Top ⊔ _ = Top
  _ ⊔ Top = Top
  NumVal x ⊔ NumVal y = NumVal (x ⊔ y) 
  ClosureVal x ⊔ ClosureVal y = ClosureVal (x ⊔ y) 
  _ ⊔ _ = Top

instance Hashable Closure
instance Hashable Val
