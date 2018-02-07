{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Vals.Symbolic.Semantics where

import Prelude (String, Double, Maybe(..), Bool(..), Eq(..), Num(..), (&&), (/), const, ($), (.), uncurry)

import WhileLanguage (HasStore(..), Statement, Label)
import qualified WhileLanguage as L
import Vals.Symbolic.Val

import Data.Order
import Data.Error
import qualified Data.Map as Map
import Data.Text (Text)

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Utils

import Control.Monad.State hiding (State)
import Control.Monad.Except

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

fixRun :: ArrowChoice c => (c [L.Statement] () -> c L.Statement ()) -> c [L.Statement] ()
fixRun f = voidA $ mapA $ f (fixRun f)

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
type M = StateT State (Except String)
runM :: [Statement] -> Error String ((),State)
runM ss = fromEither $ runExcept $ runStateT (runKleisli L.run ss) initStore

run :: [Statement] -> Error String (Store,())
run = fmap (\(_,st) -> (st,())) . runM

instance HasStore (Kleisli M) Store where
  getStore = Kleisli $ \_ -> get
  putStore = Kleisli $ \st -> modify $ const st
  modifyStore = Kleisli  $ \f -> modify f

instance L.Eval (Kleisli M) Val  where
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

instance L.Run (Kleisli M) Val where
  fixRun = fixRun
  store = store
  if_ = if_
