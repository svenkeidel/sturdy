{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module IntervalAnalysis where

import           Prelude hiding (id,Bounded)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.Environment

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
import           Data.Bounded
import           Data.HashMap.Lazy (HashMap)
import           Data.Store (Store)
    
import           GHC.Generics

import           PCF (Expr)
import           Shared

type IV = Interval (InfiniteNumber Int)
data Closure = Closure Text Expr Env deriving (Eq,Show,Generic)
data Val = Bot | NumVal (Bounded IV) | ClosureVal (Pow Closure) | Top deriving (Eq, Show, Generic)
type Env = M.HashMap Text Addr

type Addr = Text
type Interp = ReaderArrow IV (BoundedEnv Text Addr Val (ErrorArrow String (CacheArrow (HashMap Text Addr,Store Addr Val,(IV,Expr)) (Error String (Store Addr Val,Val)))))

instance LowerBounded String where
  bottom = "Program might not terminate"

evalInterval :: IV -> M.HashMap Text Val -> Expr -> Error String Val
evalInterval bound env e = runCacheArrow (runErrorArrow (runBoundedEnv (runReaderArrow eval))) (alloc,env,(bound,e))
  where
    -- CFA0-like analysis by using the variables as addresses.
    alloc = id

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
    b <- askA -< ()
    returnA -< (NumVal (Bounded b 0))
  ifZero f g = proc v -> case v of
    (NumVal (Bounded _ (I.Interval i1 i2)), (x, y))
      | (i1, i2) == (0, 0) -> f -< x
      | i1 > 0 || i2 < 0 -> g -< y
      | otherwise -> (f -< x) ⊔ (g -< y)
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
