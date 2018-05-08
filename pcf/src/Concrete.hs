{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Concrete where

import Prelude

import Control.Category
import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Environment
import Control.Arrow.Fix
import Control.Arrow.Transformer.Concrete.Environment
import Control.Arrow.Transformer.Concrete.Except
import Control.Arrow.Transformer.Concrete.LeastFixPoint
import Control.Monad.State

import Data.Concrete.Error
import Data.Concrete.Environment (Env)
import Data.Hashable
import Data.Text (Text)
import Data.Label

import GHC.Generics

import PCF (Expr(..))
import Shared

data Closure = Closure Expr (Env Text Val) deriving (Eq,Generic)
data Val = NumVal Int | ClosureVal Closure deriving (Eq,Generic)
         
newtype Interp x y = Interp (Fix Expr Val (Environment Text Val (Except String (->))) x y)

runInterp :: Interp x y -> [(Text,Val)] -> x -> Error String y
runInterp (Interp f) env x = runLeastFixPoint (runExcept (runEnvironment f)) (env,x)

evalConcrete :: [(Text,Val)] -> State Label Expr -> Error String Val
evalConcrete env e = runInterp eval env (generate e)

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
    ClosureVal (Closure e env) -> f -< ((e,env),arg)
    NumVal _ -> failA -< "Expected a closure"

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
