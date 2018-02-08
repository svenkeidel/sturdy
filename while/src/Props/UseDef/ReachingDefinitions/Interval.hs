{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Props.UseDef.ReachingDefinitions.Interval where

import Prelude (String, ($), (.), fst, snd,(<$>))

import WhileLanguage (HasStore(..), HasProp(..), Statement, Label)
import qualified WhileLanguage as L

import Vals.Interval.Val
import qualified Vals.Interval.Semantics as Interval

import Props.UseDef.ReachingDefinitions.Prop

import Data.Error
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Order

import Control.Arrow
import Control.Arrow.Utils

import Control.Monad.State
import Control.Monad.Except


store :: (ArrowChoice c, HasStore c Store, HasProp c Prop) => c (Text,Val,Label) ()
store = (proc (x,v,l) -> modifyProp -< \(ReachingDefs ds) -> ReachingDefs $ Map.insert x (Set.singleton l) ds)
                         -- all previous defs of `x` are killed and `l` is generated
    &&> Interval.store


----------
-- Arrows
----------

type M = StateT (Store,Prop) (Except String)
runM :: [Statement] -> Error String ((),(Store,Prop))
runM ss = fromEither $ runExcept $ runStateT (runKleisli L.run ss) (initStore,bottom)

run :: [Statement] -> Error String (Store,Prop)
run = fmap (\(_,(st,pr)) -> (st, pr)) . runM

instance L.HasStore (Kleisli M) Store where
  getStore = Kleisli $ \_ -> fst <$> get
  putStore = Kleisli $ \st -> modify (\(_,y) -> (st,y))
  modifyStore = Kleisli $ \f -> modify (first f)

instance L.HasProp (Kleisli M) Prop where
  getProp = Kleisli $ \_ -> snd <$> get
  putProp = Kleisli $ \pr -> modify (\(x,_) -> (x,pr))
  modifyProp = Kleisli $ \f -> modify (second f)

instance L.Eval (Kleisli M) Val  where
  lookup = Interval.lookup
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