{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- | Concrete semantics of PCF.
module ConcreteInterpreter where

import Prelude hiding (fail)

import Control.Category
import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Environment
import Control.Arrow.Fix
import Control.Arrow.Conditional
import Control.Arrow.Transformer.Concrete.Environment
import Control.Arrow.Transformer.Concrete.Failure
import Control.Monad.State hiding (fail)

import Data.Profunctor
import Data.Concrete.Failure
import Data.HashMap.Lazy (HashMap)
import Data.Hashable
import Data.Text (Text)
import Data.Label

import GHC.Generics

import Syntax (Expr(..))
import GenericInterpreter

data Closure = Closure Expr (HashMap Text Val) deriving (Eq,Generic)
data Val = NumVal Int | ClosureVal Closure deriving (Eq,Generic)

-- | The concrete interpreter function for PCF. The function is
-- implemented by instantiating the shared semantics with the concrete
-- interpreter arrow `Interp`.
evalConcrete :: [(Text,Val)] -> State Label Expr -> Failure String Val
evalConcrete env e =
  runFailureT
    (runEnvT'
      (runConcreteT
        eval))
    (env,generate e)

-- | Arrow transformer that implements the concrete value semantics
newtype ConcreteT c x y = ConcreteT { runConcreteT :: c x y }
  deriving (Profunctor,Category,Arrow,ArrowChoice,ArrowFail e)
deriving instance ArrowFix x y c => ArrowFix x y (ConcreteT c)
deriving instance ArrowEnv var Val env c => ArrowEnv var Val env (ConcreteT c)

-- | Concrete instance of the interface for value operations.
instance (ArrowChoice c, ArrowFail String c) => IsVal Val (ConcreteT c) where
  succ = proc x -> case x of
    NumVal n -> returnA -< NumVal (n + 1)
    _ -> fail -< "Expected a number as argument for 'succ'"
  pred = proc x -> case x of
    NumVal n -> returnA -< NumVal (n - 1)
    _ -> fail -< "Expected a number as argument for 'pred'"
  zero = arr $ const (NumVal 0)

-- | Concrete instance of the interface for closure operations.
instance (ArrowChoice c, ArrowFail String c) => IsClosure Val (HashMap Text Val) (ConcreteT c) where
  closure = arr $ \(e, env) -> ClosureVal $ Closure e env
  applyClosure f = proc (fun, arg) -> case fun of
    ClosureVal (Closure e env) -> f -< ((e,env),arg)
    NumVal _ -> fail -< "Expected a closure"

instance (ArrowChoice c, ArrowFail String c) => ArrowCond Val (ConcreteT c) where
  type Join (ConcreteT c) x y = ()
  if_ f g = proc (v1, (x, y)) -> case v1 of
    NumVal 0 -> f -< x
    NumVal _ -> g -< y
    _ -> fail -< "Expected a number as condition for 'ifZero'"

instance Hashable Closure
instance Hashable Val
instance Show Closure where
  show (Closure e env) = show (e,env)
instance Show Val where
  show (NumVal n) = show n
  show (ClosureVal c) = show c
