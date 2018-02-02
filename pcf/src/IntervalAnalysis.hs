{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module IntervalAnalysis where

import           Prelude hiding (Bounded)

import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.Utils
import           Data.Error
import           Data.Foldable (toList)
import qualified Data.HashMap.Lazy as M
import           Data.Hashable
import           Data.InfiniteNumbers
import           Data.Interval (Interval)
import qualified Data.Interval as I
import           Data.Order
import           Data.Powerset
import           Data.Text (Text)
import           Data.Widening
import           Data.Bounded
    
import           GHC.Generics

import           PCF (Expr)
import           Shared hiding (Env)

type IV = Interval (InfiniteNumber Int)
data Closure = Closure Text Expr Env deriving (Eq,Show,Generic)
data Val = Bot | NumVal (Bounded IV) | ClosureVal (Pow Closure) | Top deriving (Eq, Show, Generic)
type Env = M.HashMap Text Val

type Interp = ReaderArrow (IV,Env) (ErrorArrow String (->))

evalInterval :: IV -> Env -> Expr -> Error String Val
evalInterval bound env e = runErrorArrow (runReaderArrow eval) ((bound,env),e)

instance ArrowFix Expr Val Interp where
  fixA = undefined

instance Widening Val where
  NumVal v1 ▽ NumVal v2 = NumVal (v1 ▽ v2)
  v1 ▽ v2 = v1 ⊔ v2

instance IsEnv Env Val Interp where
  getEnv = askA >>> pi2
  lookup = proc x -> do
    env <- getEnv -< ()
    case M.lookup x env of
      Just v -> returnA -< v
      Nothing -> failA -< "Variable " ++ show x ++ " not bound"

instance IsVal Val Interp where
  succ = proc x -> case x of
    NumVal n -> returnA -< NumVal $ n + 1
    Top -> returnA -< Top
    _ -> failA -< "Expected a number as argument for 'succ'"
  pred = proc x -> case x of
    NumVal n -> returnA -< NumVal $ n - 1
    Top -> returnA -< Top
    _ -> failA -< "Expected a number as argument for 'pred'"
  zero = proc _ -> do
    b <- pi1 <<< askA -< ()
    returnA -< (NumVal (Bounded b 0))
  ifZero f g = proc v -> case v of
    (NumVal (Bounded _ (I.Interval i1 i2)), (x, y))
      | (i1, i2) == (0, 0) -> f -< x
      | i1 > 0 || i2 < 0 -> g -< y
      | otherwise -> (f -< x) ⊔ (g -< y)
    _ -> failA -< "Expected a number as condition for 'ifZero'"
  closure = arr $ \(x, e, env) -> ClosureVal (return (Closure x e env))
  applyClosure f = proc (fun, arg) -> case fun of
    ClosureVal cls -> lubA (proc (Closure x body env) -> localA' f -< (M.insert x arg env, body)) -<< toList cls
    _ -> failA -< "Expected a closure"
    where
      localA' g = proc (c,x) -> do
        b <- pi1 <<< askA -< ()
        localA g -< ((b,c),x)

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
