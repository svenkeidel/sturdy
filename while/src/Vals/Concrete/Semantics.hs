{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Vals.Concrete.Semantics where

import           Prelude

import           Expressions
import           Shared hiding (run)
import qualified Shared

import           Data.Concrete.Error
import qualified Data.Concrete.Store as S
import           Data.Concrete.Store (Store)
import           Data.Hashable
import           Data.Text (Text)

import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.State
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Concrete.Store
import           Control.Arrow.Transformer.Concrete.Fix

import           System.Random

import           GHC.Generics (Generic)

data Val = BoolVal Bool | NumVal Int deriving (Eq, Show, Generic)
type Interp = State StdGen (StoreArrow Text Val (Except String Fix))

run :: [Statement] -> Error String (Store Text Val)
run ss = fst <$> runFix (runExcept (runStore (runState Shared.run))) (S.empty,(mkStdGen 0,ss))

instance IsVal Val Interp where
  boolLit = arr (\(b,_) -> BoolVal b)
  and = proc (v1,v2,_) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 && b2)
    _ -> failA -< "Expected two booleans as arguments for 'and'"
  or = proc (v1,v2,_) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 || b2)
    _ -> failA -< "Expected two booleans as arguments for 'or'"
  not = proc (v,_) -> case v of
    BoolVal b -> returnA -< BoolVal (Prelude.not b)
    _ -> failA -< "Expected a boolean as argument for 'not'"
  numLit = arr (\(d,_) -> NumVal d)
  randomNum = proc _ -> do
    gen <- getA -< ()
    let (r, gen') = random gen
    putA -< gen'
    returnA -< NumVal r
  add = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 + n2)
    _ -> failA -< "Expected two numbers as arguments for 'add'"
  sub = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 - n2)
    _ -> failA -< "Expected two numbers as arguments for 'sub'"
  mul = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 * n2)
    _ -> failA -< "Expected two numbers as arguments for 'mul'"
  div = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 `Prelude.div` n2)
    _ -> failA -< "Expected two numbers as arguments for 'mul'"
  eq = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2)   -> returnA -< BoolVal (n1 == n2)
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 == b2)
    _ -> failA -< "Expected two values of the same type as arguments for 'eq'"

instance Run Val Interp where
  if_ f1 f2 = proc (v,(x,y),_) -> case v of
    BoolVal True -> f1 -< x
    BoolVal False -> f2 -< y
    _ -> failA -< "Expected boolean as argument for 'if'"

instance HasStore Val Interp

instance Hashable Val
