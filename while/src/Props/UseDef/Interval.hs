{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Props.UseDef.Interval where

import Prelude (String, Double, Maybe(..), Bool(..), Eq(..), Num(..), (&&), (||), (/), const, ($), (.), fst, snd)
import qualified Prelude as Prelude

import WhileLanguage (HasStore(..), HasProp(..), Statement, Expr, Label)
import qualified WhileLanguage as L

import Vals.Interval.Val
import qualified Vals.Interval.Semantics as Interval

import Props.UseDef.Prop

import Data.Error
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Utils
import Data.Powerset

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Utils

import Control.Monad.State hiding (State)
import Control.Monad.Except

lookup :: (ArrowChoice c, ArrowFail String c, HasStore c Store, HasProp c LiftedTrace) => c (Text,Label) Val
lookup = (proc (x,l) -> modifyProp -< powmap (TrUse x l :))
     &&> Interval.lookup

store :: (ArrowChoice c, HasStore c Store, HasProp c LiftedTrace) => c (Text,Val,Label) ()
store = (proc (x,_,l) -> modifyProp -< powmap (TrDef x l :))
    &&> Interval.store

----------
-- Arrows
----------

type State = (Store,LiftedTrace)
type M = StateT State (Except String)
runM :: [Statement] -> Error String ((),State)
runM ss = fromEither $ runExcept $ runStateT (runKleisli L.run ss) (initStore,liftedTrace initTrace)

run :: [Statement] -> Error String (Store,LiftedTrace)
run = fmap (\(_,(st,pr)) -> (st,fmap Prelude.reverse pr)) . runM

instance L.HasStore (Kleisli M) Store where
  getStore = Kleisli $ \_ -> get >>= return . (\(st,_) -> st)
  putStore = Kleisli $ \st -> modify (\(_,pr) -> (st,pr))
  modifyStore = Kleisli $ \f -> modify (\(st,pr) -> (f st,pr))

instance L.HasProp (Kleisli M) LiftedTrace where
  getProp = Kleisli $ \_ -> get >>= return . (\(_,pr) -> pr)
  putProp = Kleisli $ \pr -> modify (\(st,_) -> (st,pr))
  modifyProp = Kleisli $ \f -> modify (\(st,pr) -> (st,f pr))

instance L.Eval (Kleisli M) Val  where
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

instance L.Run (Kleisli M) Val where
  fixRun = Interval.fixRun
  store = store
  if_ = Interval.if_