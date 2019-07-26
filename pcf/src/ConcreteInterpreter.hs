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

import Prelude hiding (fail,(.))

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Environment as Env
import Control.Arrow.Trans
import Control.Arrow.Transformer.Value
import Control.Arrow.Transformer.Concrete.Environment
import Control.Arrow.Transformer.Concrete.Failure
import Control.Monad.State hiding (fail)

import Data.Concrete.Error
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import Data.Hashable
import Data.Text (Text)
import Data.Label

import GHC.Generics(Generic)

import Syntax (Expr(..))
import GenericInterpreter

type Env = HashMap Text Val
data Closure = Closure Expr Env deriving (Eq,Generic)
data Val = NumVal Int | ClosureVal Closure deriving (Eq,Generic)

-- | The concrete interpreter function for PCF. The function is
-- implemented by instantiating the shared semantics with the concrete
-- interpreter arrow `Interp`.
evalConcrete :: [(Text,Val)] -> State Label Expr -> Error String Val
evalConcrete env e = run (eval :: ValueT Val (EnvT Text Val (FailureT String (->))) Expr Val) (M.fromList env,generate e)

-- | Concrete instance of the interface for value operations.
instance (ArrowChoice c, ArrowFail String c) => IsNum Val (ValueT Val c) where
  type Join y (ValueT Val c) = ()
  succ = proc x -> case x of
    NumVal n -> returnA -< NumVal (n + 1)
    _ -> fail -< "Expected a number as argument for 'succ'"
  pred = proc x -> case x of
    NumVal n -> returnA -< NumVal (n - 1)
    _ -> fail -< "Expected a number as argument for 'pred'"
  zero = arr $ const (NumVal 0)

  if_ f g = proc (v1, (x, y)) -> case v1 of
    NumVal 0 -> f -< x
    NumVal _ -> g -< y

    _ -> fail -< "Expected a number as condition for 'ifZero'"
-- | Concrete instance of the interface for closure operations.
instance (ArrowClosure var Val Env c, ArrowChoice c, ArrowFail String c) => IsClosure Val (ValueT Val c) where
  closure _ = proc e -> do
    env <- Env.ask -< ()
    returnA -< ClosureVal (Closure e env)
  applyClosure f = proc (fun, arg) -> case fun of
    ClosureVal (Closure e env) -> Env.local f -< (env,(e,arg))
    NumVal _ -> fail -< "Expected a closure"

instance Hashable Closure
instance Hashable Val
instance Show Closure where
  show (Closure e env) = show (e,env)
instance Show Val where
  show (NumVal n) = show n
  show (ClosureVal c) = show c
