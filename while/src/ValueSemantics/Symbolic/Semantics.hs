{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vals.Symbolic.Semantics where

import Prelude (String, Double, Bool(..), const, ($), uncurry)

import WhileLanguage (HasStore(..), Statement, Label)
import qualified WhileLanguage as L
import Vals.Symbolic.Val

import Data.Order
import Data.Error
import Data.Text (Text)

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.State

-----------
-- Eval
-----------

lookup :: Arrow c => c (Text,Label) Val
lookup = arr $ uncurry L.Var

boolLit :: Arrow c => c (Bool,Label) Val
boolLit = arr $ uncurry L.BoolLit

and :: Arrow c => c (Val,Val,Label) Val
and = proc (v1,v2,l) -> returnA -< L.And v1 v2 l

or :: Arrow c => c (Val,Val,Label) Val
or = proc (v1,v2,l) -> returnA -< L.Or v1 v2 l

not :: Arrow c => c (Val,Label) Val
not = arr $ uncurry L.Not

numLit :: Arrow c => c (Double,Label) Val
numLit = arr $ uncurry L.NumLit

randomNum :: Arrow c => c Label Val
randomNum = arr L.RandomNum

add :: Arrow c => c (Val,Val,Label) Val
add = proc (v1,v2,l) -> returnA -< L.Add v1 v2 l

sub :: Arrow c => c (Val,Val,Label) Val
sub = proc (v1,v2,l) -> returnA -< L.Sub v1 v2 l

mul :: Arrow c => c (Val,Val,Label) Val
mul = proc (v1,v2,l) -> returnA -< L.Mul v1 v2 l

div :: Arrow c => c (Val,Val,Label) Val
div = proc (v1,v2,l) -> returnA -< L.Div v1 v2 l

eq :: Arrow c => c (Val,Val,Label) Val
eq = proc (v1,v2,l) -> returnA -< L.Eq v1 v2 l

fixEval :: Arrow c => (c L.Expr v -> c L.Expr v) -> c L.Expr v
fixEval f = f (fixEval f)


----------
-- Run
----------

store :: Arrow c => c (Text,Val,L.Label) ()
store = arr $ const ()

if_ :: (ArrowChoice c, ArrowFail String c, Complete (c ([L.Statement],[L.Statement]) ()))
    => c [L.Statement] () -> c [L.Statement] () -> c (Val,([L.Statement],[L.Statement]),L.Label) ()
if_ f1 f2 = proc (v,(x,y),_) -> case v of
  L.BoolLit True _ -> f1 -< x
  L.BoolLit False _ -> f2 -< y
  _ -> joined f1 f2 -< (x,y)

----------
-- Arrows
----------

type State = ()
initState :: State
initState = ()

type In a = (State,a)
type Out a = Error String (State,a)
type M = StateArrow State (ErrorArrow String (Fix (In [Statement]) (Out ())))

runM :: [Statement] -> Error String (State,())
runM ss = runFix (runErrorArrow (runStateArrow L.run)) (initState, ss)

run :: [Statement] -> Error String (Store,())
run = runM

instance HasStore M Store where
  getStore = getA
  putStore = putA

instance L.Eval M Val  where
  lookup = lookup
  boolLit = boolLit
  and = and
  or = or
  not = not
  numLit = numLit
  randomNum = randomNum
  add = add
  sub = sub
  mul = mul
  div = div
  eq = eq
  fixEval = fixEval

instance L.Run M Val where
  store = store
  if_ = if_
