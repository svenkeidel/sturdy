{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Props.UseDef.DeadStores.Interval where

import Prelude (String, ($), (.), fst, snd, fmap)

import WhileLanguage (HasStore(..), HasProp(..), Statement, Label)
import qualified WhileLanguage as L

import Vals.Interval.Val
import qualified Vals.Interval.Semantics as Interval

import Props.UseDef.DeadStores.Prop

import Data.Error
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Utils
import Data.Order (bottom)

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Fix
import Control.Arrow.State
import Control.Arrow.Utils


lookup :: (ArrowChoice c, ArrowFail String c, HasStore c Store, HasProp c Prop) => c (Text,Label) Val
lookup = modifyProp (arr $ \((x,_),DeadStores maybeDead dead) ->
           DeadStores (Map.delete x maybeDead) dead)
     &&> Interval.lookup

store :: (ArrowChoice c, HasStore c Store, HasProp c Prop) => c (Text,Val,Label) ()
store = modifyProp (arr $ \((x,_,l),DeadStores maybeDead dead) ->
          DeadStores (Map.insert x (Set.singleton l) maybeDead)
                     (dead `Set.union` lookupM x maybeDead))
    &&> Interval.store


----------
-- Arrows
----------

type State = (Store,Prop)
initState :: State
initState = (initStore, bottom)

type In a = (State,a)
type Out a = Error String (State,a)
type M = StateArrow State (ErrorArrow String (LeastFixPoint (In [Statement]) (Out ())))

runM :: [Statement] -> Error String (State,())
runM ss = runLeastFixPoint (runErrorArrow (runStateArrow L.run)) (initState, ss)

run :: [Statement] -> Error String (Store,FDeadStores)
run = fmap (second finalizeDeadStores . fst) . runM

instance L.HasStore M Store where
  getStore = getA >>> arr fst
  putStore = modifyA $ arr $ \(st,(_,rnd)) -> (st,rnd)

instance L.HasProp M Prop where
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