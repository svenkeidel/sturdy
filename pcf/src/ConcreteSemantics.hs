{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Concrete semantics of PCF.
module ConcreteSemantics where

import Prelude hiding (fail)

import Control.Category
import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Environment
import Control.Arrow.Fix
import Control.Arrow.Conditional
import Control.Arrow.Transformer.Concrete.Environment
import Control.Arrow.Transformer.Concrete.Failure
import Control.Arrow.Transformer.Concrete.Fixpoint
import Control.Monad.State hiding (fail)

import Data.Concrete.Error
import Data.Concrete.Environment (Env)
import Data.Hashable
import Data.Text (Text)
import Data.Label

import GHC.Generics

import Syntax (Expr(..))
import SharedSemantics

data Closure = Closure Expr (Env Text Val) deriving (Eq,Generic)
data Val = NumVal Int | ClosureVal Closure deriving (Eq,Generic)

-- | The interpreter arrow for the concrete semantics that encapsulates the passing of the enviornment and the propagation of failure.
newtype Interp x y = Interp (Fix Expr Val (EnvT Text Val (FailureT String (->))) x y)

-- | Takes an arrow computation and executes it.
runInterp :: Interp x y -> [(Text,Val)] -> x -> Error String y
runInterp (Interp f) env x = runFix (runFailureT (runEnvT' f)) (env,x)

-- | The concrete interpreter function for PCF. The function is
-- implemented by instantiating the shared semantics with the concrete
-- interpreter arrow `Interp`.
evalConcrete :: [(Text,Val)] -> State Label Expr -> Error String Val
evalConcrete env e = runInterp eval env (generate e)

-- | Concrete instance of the interface for value operations.
instance IsVal Val Interp where
  succ = proc x -> case x of
    NumVal n -> returnA -< NumVal (n + 1)
    _ -> fail -< "Expected a number as argument for 'succ'"
  pred = proc x -> case x of
    NumVal n -> returnA -< NumVal (n - 1)
    _ -> fail -< "Expected a number as argument for 'pred'"
  zero = arr $ const (NumVal 0)

-- | Concrete instance of the interface for closure operations.
instance IsClosure Val (Env Text Val) Interp where
  closure = arr $ \(e, env) -> ClosureVal $ Closure e env
  applyClosure f = proc (fun, arg) -> case fun of
    ClosureVal (Closure e env) -> f -< ((e,env),arg)
    NumVal _ -> fail -< "Expected a closure"

instance ArrowCond Val x y z Interp where
  if_ f g = proc (v1, (x, y)) -> case v1 of
    NumVal 0 -> f -< x
    NumVal _ -> g -< y
    _ -> fail -< "Expected a number as condition for 'ifZero'"

-- All other instances for the concrete interpreter arrow can be
-- derived from the instances of the underlying arrow transformers.
deriving instance Category Interp
deriving instance Arrow Interp
deriving instance ArrowChoice Interp
deriving instance ArrowFail String Interp
deriving instance ArrowEnv Text Val (Env Text Val) Interp
deriving instance ArrowFix Expr Val Interp

instance Hashable Closure
instance Hashable Val
instance Show Closure where
  show (Closure e env) = show (e,env)
instance Show Val where
  show (NumVal n) = show n
  show (ClosureVal c) = show c
