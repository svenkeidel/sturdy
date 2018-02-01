{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module IntervalAnalysis where

import           Prelude

import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Monad.Trans.Reader
import           Data.Error
import           Data.Foldable (toList)
import qualified Data.HashMap.Lazy as M
import           Data.Hashable
import           Data.InfiniteNumbers
import           Data.Interval
import           Data.Order
import           Data.Powerset
import           Data.Text (Text)
import           Data.Widening
    
import           GHC.Generics

import           PCF (Expr (Lam))
import           Shared hiding (Env)
import           Utils

type IV = Interval (InfiniteNumber Int)
data Closure = Closure Expr Env deriving (Eq,Show,Generic)
data Val = Bot | NumVal IV | ClosureVal (Pow Closure) | Top deriving (Eq, Show, Generic)
type Env = M.HashMap Text Val

type Interp = Kleisli (ReaderT (IV,Env) (Error String))

evalInterval :: IV -> Env -> Expr -> Error String Val
evalInterval bound env e = runReaderT (runKleisli eval e) (bound,env)

instance ArrowFix Interp where
  fixA f = widenedLfp _

instance IsEnv Env Val Interp where
  getEnv = Kleisli $ const (snd <$> ask)
  lookup = proc x -> do
    env <- getEnv -< ()
    case M.lookup x env of
      Just v -> returnA -< v
      Nothing -> failA -< "Variable " ++ show x ++ " not bound"

instance IsVal Val Interp where
  succ = proc x -> case x of
    NumVal n -> returnA -< NumVal $ withBounds1 (\y -> y + 1) n
    Top -> returnA -< Top
    _ -> failA -< "Expected a number as argument for 'succ'"
  pred = proc x -> case x of
    NumVal n -> returnA -< NumVal $ withBounds1 (\y -> y - 1) n
    Top -> returnA -< Top
    _ -> failA -< "Expected a number as argument for 'pred'"
  zero = arr $ const (NumVal (constant 0))
  ifZero f g = proc x -> case x of
    (NumVal (IV (i1, i2)), (x, y)) | (i1, i2) == (0, 0) -> f -< x
                                   | i1 > 0 || i2 < 0 -> g -< y
                                   | otherwise -> (f -< x) ⊔ (g -< y)
    _ -> failA -< "Expected a number as condition for 'ifZero'"
  closure = arr $ \(e, env) -> ClosureVal (return (Closure e env))
  applyClosure f = proc (fun, arg) -> case fun of
    ClosureVal cls -> lubA (proc (Closure (Lam x _ body) env) -> localA' f -< (M.insert x arg env, body)) -<< toList cls
    _ -> failA -< "Expected a closure"
    where
      localA' g = proc (c,x) -> do
        b <- Kleisli (const (fst <$> ask)) -< ()
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
