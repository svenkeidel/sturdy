{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Props.UseDef.Interval where

import Prelude (String, ($), (.), fst, snd, fmap,reverse)

import WhileLanguage (HasStore(..), HasProp(..), Statement, Label)
import qualified WhileLanguage as L

import Vals.Interval.Val
import qualified Vals.Interval.Semantics as Interval

import Props.UseDef.Prop

import Data.Error
import Data.Text (Text)
import Data.Powerset

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.State
import Control.Arrow.Utils

lookup :: (ArrowChoice c, ArrowFail String c, HasStore c Store, HasProp c LiftedTrace) => c (Text,Label) Val
lookup = modifyProp (arr $ \((x,l),tr) -> powmap (TrUse x l :) tr)
     &&> Interval.lookup

store :: (ArrowChoice c, HasStore c Store, HasProp c LiftedTrace) => c (Text,Val,Label) ()
store = modifyProp (arr $ \((x,_,l),tr) -> powmap (TrDef x l :) tr)
    &&> Interval.store

----------
-- Arrows
----------

type State = (Store,LiftedTrace)
initState :: State
initState = (initStore, liftTrace initTrace)

type In a = (State,a)
type Out a = Error String (State,a)
type M = StateArrow State (ErrorArrow String (LeastFixPoint (In [Statement]) (Out ())))

runM :: [Statement] -> Error String (State,())
runM ss = runLeastFixPoint (runErrorArrow (runStateArrow L.run)) (initState, ss)

run :: [Statement] -> Error String (Store,LiftedTrace)
run = fmap (second (fmap reverse) . fst) . runM

instance L.HasStore M Store where
  getStore = getA >>> arr fst
  putStore = modifyA $ arr $ \(st,(_,rnd)) -> (st,rnd)

instance L.HasProp M LiftedTrace where
  getProp = getA >>> arr snd
  putProp = modifyA $ arr $ \(pr,(st,_)) -> (st,pr)

instance L.Eval M Val  where
  lookup = lookup
  boolLit = Interval.boolLit
  and = Interval.and
  or = Interval.or
  not = Interval.not
  numLit = Interval.numLit
  randomNum = Interval.randomNum
  add = Interval.add
  sub = Interval.sub
  mul = Interval.mul
  div = Interval.div
  eq = Interval.eq
  fixEval = Interval.fixEval

instance L.Run M Val where
  store = store
  if_ = Interval.if_