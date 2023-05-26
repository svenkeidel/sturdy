{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- | Concrete semantics of PCF.
module ConcreteInterpreter where

import Prelude hiding (fail,(.))

import Control.Arrow
import Control.Arrow.Fail as Fail
import Control.Arrow.Closure as Cls
import Control.Arrow.Trans
import Control.Arrow.Transformer.Value
import Control.Arrow.Transformer.Concrete.Environment
import Control.Arrow.Transformer.Concrete.Failure
import Control.Monad.State hiding (fail)

import Data.Concrete.Error
import Data.Concrete.Closure
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import Data.Text (Text)
import Data.Label
import Data.Profunctor
import qualified Data.Function as Function

import GHC.Generics(Generic)

import Syntax (Expr(..))
import GenericInterpreter

type Env = HashMap Text Val
type Cls = Closure Expr Env
data Val = NumVal Int | ClosureVal Cls deriving (Eq,Generic)

-- | The concrete interpreter function for PCF. The function is
-- implemented by instantiating the shared semantics with the concrete
-- interpreter arrow `Interp`.
evalConcrete :: [(Text,Val)] -> State Label Expr -> Error String Val
evalConcrete env e = 
  let ?fixpointAlgorithm = Function.fix in
  run (eval :: 
    ValueT Val 
      (EnvT Env 
        (FailureT String 
          (->))) Expr Val)
  (M.fromList env,generate e)

-- | Concrete instance of the interface for value operations.
instance (ArrowClosure Expr Cls c, ArrowChoice c, ArrowFail String c, Fail.Join Val c) => IsVal Val (ValueT Val c) where
  type Join y (ValueT Val c) = (Fail.Join y c)

  succ = proc x -> case x of
    NumVal n -> returnA -< NumVal (n + 1)
    _ -> fail -< "Expected a number as argument for 'succ'"

  pred = proc x -> case x of
    NumVal n -> returnA -< NumVal (n - 1)
    _ -> fail -< "Expected a number as argument for 'pred'"

  mult = proc x -> case x of
    (NumVal n, NumVal m) -> returnA -< NumVal (n * m)
    _ -> fail -< "Expected two numbers as argument for 'mult'"

  zero = arr $ const (NumVal 0)

  if_ f g = proc (v1, (x, y)) -> case v1 of
    NumVal 0 -> f -< x
    NumVal _ -> g -< y
    _ -> fail -< "Expected a number as condition for 'ifZero'"

instance (ArrowChoice c, ArrowFail String c, ArrowClosure Expr Cls c)
    => ArrowClosure Expr Val (ValueT Val c) where
  type Join y Val (ValueT Val c) = (Cls.Join y Cls c, Fail.Join y c)
  closure = ValueT $ rmap ClosureVal Cls.closure
  apply (ValueT f) = ValueT $ proc (v,x) -> case v of
    ClosureVal cls -> Cls.apply f -< (cls,x)
    _ -> fail -< "Expected a closure"
  {-# INLINE closure #-}
  {-# INLINE apply #-}

instance IsClosure Val Env where
  traverseEnvironment _ (NumVal n) = pure (NumVal n)
  traverseEnvironment f (ClosureVal cl) = ClosureVal <$> traverse f cl

  mapEnvironment _ (NumVal n) = NumVal n
  mapEnvironment f (ClosureVal (Closure expr env)) = ClosureVal (Closure expr (f env))

instance Show Val where
  show (NumVal n) = show n
  show (ClosureVal cls) = show cls
