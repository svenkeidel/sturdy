{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vals.Interval.Semantics where

import           Prelude hiding (Bool(..))
import qualified Prelude as P

import           Expressions
import           Shared
import qualified Vals.Concrete.Semantics as Concrete

import           Data.Abstract.Boolean (Bool)
import qualified Data.Abstract.Boolean as B
import           Data.Abstract.Error (Error)
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import           Data.Abstract.Store (Store)
import qualified Data.Abstract.Store as S
import           Data.Abstract.Widening
import qualified Data.Boolean as B
import           Data.Concrete.Powerset
import           Data.GaloisConnection
import           Data.Hashable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Order
import           Data.Text (Text)

import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.Fix
import           Control.Arrow.State
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.Fix

import           System.Random
import           GHC.Generics

data Val = Bot | BoolVal Bool | NumVal (Interval Double) | Top deriving (Eq,Generic)

type Interp = StoreArrow Text Val (Except String (Fix (Store Text Val,[Statement]) (Error String (Store Text Val,()))))

run :: [Statement] -> Error String (Store Text Val)
run ss = _ $ runFix (runExcept (runStore (Shared.run :: Interp [Statement] ()))) (S.empty,ss)

instance IsVal Val Interp where
  boolLit = arr $ \(b,_) -> case b of
    P.True -> BoolVal true
    P.False -> BoolVal false
  and = proc (v1,v2,_) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 `B.and` b2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two booleans as arguments for 'and'"
  or = proc (v1,v2,_) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 `B.or` b2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two booleans as arguments for 'ord'"
  not = proc (v,_) -> case v of
    BoolVal b -> returnA -< BoolVal (B.not b)
    Top -> returnA -< Top
    _ -> failA -< "Expected a boolean as argument for 'not'"
  numLit = arr $ \(x,_) -> NumVal (I.Interval x x)
  randomNum = arr $ const $ NumVal top
  add = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 + n2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two numbers as arguments for 'add'"
  sub = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 - n2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two numbers as arguments for 'sub'"
  mul = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 * n2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two numbers as arguments for 'mul'"
  div = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 / n2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two numbers as arguments for 'mul'"
  eq = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal (I.Interval m1 m2),NumVal (I.Interval n1 n2))
      | m1 == m2 && n1 == n2 && m1 == m2 -> returnA -< BoolVal B.True
      | m2 < n1 || n2 < m1 -> returnA -< BoolVal B.False
      | otherwise -> returnA -< BoolVal B.Top
    (NumVal _,NumVal _)   -> returnA -< Top
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 == b2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two values of the same type as arguments for 'eq'"

instance Run Val Interp where
  if_ f1 f2 = proc (v,(x,y),_) -> case v of
    BoolVal B.True -> f1 -< x
    BoolVal B.False -> f2 -< y
    BoolVal B.Top -> joined f1 f2 -< (x,y)
    Top -> top -< ()
    _ -> failA -< "Expected boolean as argument for 'if'"


instance HasStore Val Interp

instance PreOrd Val where
  Bot ⊑ _ = True
  _ ⊑ Top = True
  BoolVal b1 ⊑ BoolVal b2 = b1 == b2
  NumVal n1 ⊑ NumVal n2 = n1 ⊑ n2
  _ ⊑ _ = False

instance LowerBounded Val where
  bottom = Bot
instance UpperBounded Val where
  top = Top

instance Complete Val where
  BoolVal b1 ⊔ BoolVal b2 = if b1 == b2 then BoolVal b1 else Top
  NumVal n1 ⊔ NumVal n2 = NumVal $ n1 ⊔ n2
  Bot ⊔ a = a
  a ⊔ Bot = a
  _ ⊔ _ = Top

instance Widening Val where
  BoolVal b1 ▽ BoolVal b2 = BoolVal (b1 ⊔ b2)
  NumVal n1 ▽ NumVal n2 = NumVal (n1 ▽ n2)

instance Galois (Pow Concrete.Val) Val where
  alpha = lifted lift
    where lift (Concrete.BoolVal b) = BoolVal b
          lift (Concrete.NumVal n) = NumVal $ I.Interval n n
  gamma Bot = mempty
  gamma (BoolVal b) = return $ Concrete.BoolVal b
  gamma (NumVal (I.Interval m n)) = fromFoldable [Concrete.NumVal x | x <- [m..n]]
  gamma (NumVal I.Bot) = mempty
  gamma Top = gamma (BoolVal True) `union` gamma (BoolVal False) `union` gamma (NumVal top)

instance Show Val where
  show Bot = "⊥"
  show (NumVal iv) = show iv
  show (BoolVal b) = show b
  show Top = "⊤"

instance Hashable Val
