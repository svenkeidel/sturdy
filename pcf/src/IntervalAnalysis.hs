{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module IntervalAnalysis where

import           Prelude hiding (Bounded)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Const
import           Control.Arrow.Fix
import           Control.Arrow.Environment
import           Control.Arrow.Transformer.Abstract.Contour hiding (toList)
import           Control.Arrow.Transformer.Abstract.BoundedEnvironment
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Const
import           Control.Monad.State hiding (lift)

import           Data.Foldable (toList)
import           Data.Hashable
import           Data.HashSet(HashSet)
import qualified Data.HashSet as S
import           Data.Label
import           Data.Order
import           Data.Text (Text)

import           Data.Abstract.Bounded
import           Data.Abstract.Environment(Env)
import           Data.Abstract.Error (Error)
import           Data.Abstract.InfiniteNumbers
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import           Data.Abstract.Store (Store)
import           Data.Abstract.Widening
import           Data.Abstract.Terminating
    
import           GHC.Generics

import           PCF (Expr)
import           Shared

type IV = Interval (InfiniteNumber Int)
data Closure = Closure Expr (Env Text Addr,Store Addr Val) deriving (Eq,Generic)
data Val = NumVal (Bounded IV) | ClosureVal (HashSet Closure) | Top deriving (Eq, Generic)

instance Show Val where
  show (NumVal iv) = show iv
  show (ClosureVal cls) = show cls
  show Top = "⊤"

instance Show Closure where
  show (Closure e _) = show e

type Addr = (Text,Contour)
newtype Interp x y =
  Interp (
    Fix Expr Val 
      (Const IV
        (Environment Text Addr Val
          (ContourArrow
            (Except String
              (~>))))) x y)

runInterp :: Interp x y -> IV -> Int -> [(Text,Val)] -> x -> Terminating (Error String y)
runInterp (Interp f) b k env x = 
  runFix
    (runExcept
      (runContourArrow k
        (runEnvironment
          (runConst b f))))
    (env,x)

evalInterval :: (?bound :: IV) => Int -> [(Text,Val)] -> State Label Expr -> Terminating (Error String Val)
evalInterval k env e = runInterp eval ?bound k env (generate e)

instance IsVal Val Interp where
  succ = proc x -> case x of
    Top -> returnA -< Top
    NumVal n -> returnA -< NumVal $ lift (+ 1) n
    ClosureVal _ -> failA -< "Expected a number as argument for 'succ'"
  pred = proc x -> case x of
    Top -> returnA -< Top
    NumVal n -> returnA -< NumVal $ lift (+ negate 1) n
    ClosureVal _ -> failA -< "Expected a number as argument for 'pred'"
  zero = proc _ -> do
    b <- askConst -< ()
    returnA -< (NumVal (Bounded b 0))
  ifZero f g = proc v -> case v of
    (Top, (x,y)) -> (f -< x) ⊔ (g -< y)
    (NumVal (Bounded _ (I.Interval i1 i2)), (x, y))
      | (i1, i2) == (0, 0) -> f -< x
      | i1 > 0 || i2 < 0 -> g -< y
      | otherwise -> (f -< x) ⊔ (g -< y)
    (ClosureVal _, _) -> failA -< "Expected a number as condition for 'ifZero'"

instance IsClosure Val (Env Text Addr,Store Addr Val) Interp where
  closure = arr $ \(e, env) -> ClosureVal (S.singleton (Closure e env))
  applyClosure f = proc (fun, arg) -> case fun of
    Top -> returnA -< Top
    ClosureVal cls -> lubA (proc (Closure e env,arg) -> f -< ((e,env),arg))
                        -< [ (c,arg) | c <- toList cls] 
    NumVal _ -> failA -< "Expected a closure"

deriving instance Category Interp
deriving instance Arrow Interp
deriving instance ArrowChoice Interp
deriving instance ArrowFail String Interp
deriving instance ArrowConst IV Interp
deriving instance ArrowEnv Text Val (Env Text (Text,Contour), Store (Text,Contour) Val) Interp
deriving instance ArrowFix Expr Val Interp
deriving instance PreOrd y => PreOrd (Interp x y)
deriving instance Complete y => Complete (Interp x y)
deriving instance PreOrd y => LowerBounded (Interp x y)

instance PreOrd Val where
  _ ⊑ Top = True
  NumVal n1 ⊑ NumVal n2 = n1 ⊑ n2
  ClosureVal c1 ⊑ ClosureVal c2 = all (`elem` c2) c1
  _ ⊑ _ = False

instance Complete Val where
  Top ⊔ _ = Top
  _ ⊔ Top = Top
  NumVal x ⊔ NumVal y = NumVal (x ⊔ y)
  ClosureVal x ⊔ ClosureVal y = ClosureVal (x `S.union` y)
  _ ⊔ _ = Top

instance Widening Val where
  -- Only intervals require widening, everything else has finite height.
  NumVal x ▽ NumVal y = NumVal (x ▽ y)
  x ▽ y =  x ⊔ y

instance HasLabel ((Env Text Addr,Store (Text, Contour) Val),Expr) where
  label ((_,_),e) = label e

instance Hashable Closure
instance Hashable Val
