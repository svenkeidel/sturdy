{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Props.LiveVariables.Interval where

import Prelude (String, Double, Maybe(..), Bool(..), Eq(..), Num(..), (&&), (||), (/), const, ($), (.), fst, snd)
import qualified Prelude as Prelude

import WhileLanguage (HasStore(..), HasProp(..), Statement, Expr, Label)
import qualified WhileLanguage as L

import Vals.Interval.Val
import qualified Vals.Interval.Semantic as Interval

import Props.LiveVariables.Prop

import Data.Error
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Utils

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Utils

import Control.Monad.State
import Control.Monad.Except

lookup :: (ArrowChoice c, ArrowFail String c, HasStore c Store, HasProp c CProp) => c (Text,Label) Val
lookup = (proc (x,l) -> modifyProp -< (\(LiveVars maybe live) ->
           -- `x` is live at all assignments `maybe(x)`, hence remove `x` from `maybe` and it to `live`
           LiveVars (Map.delete x maybe)
                    (Map.insertWith Set.union x (lookupM x maybe) live)))
     &&> Interval.lookup

store :: (ArrowChoice c, HasStore c Store, HasProp c CProp) => c (Text,Val,Label) ()
store = (proc (x,_,l) -> modifyProp -< (\(LiveVars maybe live) ->
          -- overwrite `maybe(x)={l}`, meaning that `x` was not live at any of the previous assignments `maybe(x)`
          LiveVars (Map.insert x (Set.singleton l) maybe) live))
    &&> Interval.store


----------
-- Arrows
----------

type M = StateT (Store,AProp) (Except String)
runM :: [Statement] -> Error String ((),(Store,AProp))
runM ss = fromEither $ runExcept $ runStateT (runKleisli L.run ss) (initStore,initAProp)

run :: [Statement] -> Error String (Store,FAProp)
run = fmap (\(_,(st,pr)) -> (st,finalizeLiveVars pr)) . runM

instance L.HasStore (Kleisli M) Store where
  getStore = Kleisli $ \_ -> get >>= return . fst
  putStore = Kleisli $ \st -> modify (\(_,y) -> (st,y))
  modifyStore = Kleisli $ \f -> modify (\(st,y) -> (f st,y))

instance L.HasProp (Kleisli M) AProp where
  getProp = Kleisli $ \_ -> get >>= return . snd
  putProp = Kleisli $ \pr -> modify (\(x,_) -> (x,pr))
  modifyProp = Kleisli $ \f -> modify (\(x,pr) -> (x,f pr))

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