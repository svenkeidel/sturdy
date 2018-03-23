{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module IntervalAnalysis where

import           Prelude hiding (Bounded)

import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.Reader
import           Control.Arrow.Environment
import           Control.Arrow.Transformer.Abstract.Contour hiding (toList)
import           Control.Arrow.Transformer.Abstract.BoundedEnvironment
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Reader
import           Control.Monad.State hiding (lift)

import           Data.Abstract.Bounded
import           Data.Abstract.Environment(Env)
import           Data.Abstract.Error (Error)
import           Data.Foldable (toList)
import           Data.Hashable
import           Data.HashSet(HashSet)
import qualified Data.HashSet as S
import           Data.Abstract.InfiniteNumbers
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import           Data.Label
import           Data.Order
import           Data.Abstract.Store (Store)
import           Data.Text (Text)
import           Data.Abstract.Widening
    
import           GHC.Generics

import           PCF (Expr)
import           Shared

type IV = Interval (InfiniteNumber Int)
data Closure = Closure Expr (Env Text Addr,Store Addr Val) deriving (Eq,Generic)
data Val = Bot | NumVal (Bounded IV) | ClosureVal (HashSet Closure) | Top deriving (Eq, Generic)

instance Show Val where
  show Bot = "⊥"
  show (NumVal iv) = show iv
  show (ClosureVal cls) = show cls
  show Top = "⊤"

instance Show Closure where
  show (Closure e _) = show e

type Addr = (Text,Contour)
type Interp =
  ReaderArrow IV
    (Environment Text Addr Val
      (ContourArrow
        (ErrorArrow String
          (CacheArrow (Env Text Addr,Store Addr Val,(IV,Expr))
                      (Error String Val)))))

evalInterval :: Int -> IV -> [(Text,Val)] -> State Label Expr -> Error String Val
evalInterval k bound env e =
  runCacheArrow
    (runErrorArrow
      (runContourArrow k
        (runEnvironment
          (runReaderArrow (eval :: Interp Expr Val)))))
    (env,(bound,generate e))

instance IsVal Val Interp where
  succ = proc x -> case x of
    Top -> returnA -< Top
    NumVal n -> returnA -< NumVal $ lift (+ 1) n
    ClosureVal _ -> failA -< "Expected a number as argument for 'succ'"
    Bot -> returnA -< Bot
  pred = proc x -> case x of
    Top -> returnA -< Top
    NumVal n -> returnA -< NumVal $ lift (+ negate 1) n
    ClosureVal _ -> failA -< "Expected a number as argument for 'pred'"
    Bot -> returnA -< Bot
  zero = proc _ -> do
    b <- askA -< ()
    returnA -< (NumVal (Bounded b 0))
  ifZero f g = proc v -> case v of
    (Top, (x,y)) -> (f -< x) ⊔ (g -< y)
    (NumVal (Bounded _ (I.Interval i1 i2)), (x, y))
      | (i1, i2) == (0, 0) -> f -< x
      | i1 > 0 || i2 < 0 -> g -< y
      | otherwise -> (f -< x) ⊔ (g -< y)
    (NumVal (Bounded _ I.Bot), _) -> returnA -< Bot
    (ClosureVal _, _) -> failA -< "Expected a number as condition for 'ifZero'"
    (Bot, _) -> returnA -< Bot


instance IsClosure Val (Env Text Addr,Store Addr Val) Interp where
  closure = arr $ \(e, env) -> ClosureVal (S.singleton (Closure e env))
  applyClosure f = proc (fun, arg) -> case fun of
    Top -> returnA -< Top
    ClosureVal cls -> lubA (proc (Closure e env,arg) -> f -< ((e,env),arg))
                        -< [ (c,arg) | c <- toList cls] 
    NumVal _ -> failA -< "Expected a closure"
    Bot -> returnA -< Bot

instance PreOrd Val where
  Bot ⊑ _ = True
  _ ⊑ Top = True
  NumVal n1 ⊑ NumVal n2 = n1 ⊑ n2
  ClosureVal c1 ⊑ ClosureVal c2 = all (`elem` c2) c1
  _ ⊑ _ = False

instance LowerBounded Val where
  bottom = Bot

instance Complete Val where
  Bot ⊔ x = x
  x ⊔ Bot = x
  Top ⊔ _ = Top
  _ ⊔ Top = Top
  NumVal x ⊔ NumVal y = NumVal (x ⊔ y)
  ClosureVal x ⊔ ClosureVal y = ClosureVal (x `S.union` y)
  _ ⊔ _ = Top

instance Widening Val where
  -- Only intervals require widening, everything else has finite height.
  NumVal x ▽ NumVal y = NumVal (x ▽ y)
  x ▽ y =  x ⊔ y

instance HasLabel (Env Text Addr,Store (Text, Contour) Val,(IV, Expr)) where
  label (_,_,(_,e)) = label e

instance Hashable Closure
instance Hashable Val
