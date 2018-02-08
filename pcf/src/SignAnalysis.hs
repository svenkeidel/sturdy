{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SignAnalysis where

import           Prelude hiding (id)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Environment

import           Data.Error
import           Data.Foldable (toList)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Hashable
import           Data.Order
import           Data.Powerset
import qualified Data.Sign as Sign
import           Data.Sign hiding (Bot,Top)
import           Data.Text (Text)
import           Data.Store (Store)

import           GHC.Generics

import           PCF (Expr)
import           Shared

data Closure = Closure Text Expr Env deriving (Eq,Show,Generic)
data Val = Bot | NumVal Sign | ClosureVal (Pow Closure) | Top deriving (Eq,Show,Generic)
type Env = M.HashMap Text Addr

type Addr = Text
type Interp = BoundedEnv Text Addr Val (ErrorArrow String (CacheArrow (HashMap Text Addr, Store Addr Val, Expr) (Error String (Store Addr Val, Val))))

instance LowerBounded String where
  bottom = "Program might not terminate"

evalSign :: HashMap Text Val -> Expr -> Error String Val
evalSign env e = runCacheArrow (runErrorArrow (runBoundedEnv eval)) (alloc,env,e)
  where alloc = id

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

instance IsClosure Val Env Interp where
  closure = arr $ \(x, e, env) -> ClosureVal (return (Closure x e env))
  applyClosure f = proc (fun, arg) -> case fun of
    ClosureVal cls -> lubA (proc (Closure x body env) -> do
      env' <- extendEnv -< (x,arg,env)
      localEnv f -< (env', body))
        -<< toList cls
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
