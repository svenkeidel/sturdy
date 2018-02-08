{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Props.UseDef.Concrete where

import Prelude (String, ($), (.),const,(<$>))
import qualified Prelude

import WhileLanguage (HasStore(..), HasProp(..), Statement, Label)
import qualified WhileLanguage as L

import Vals.Concrete.Val
import qualified Vals.Concrete.Semantics as Concrete

import Props.UseDef.Prop

import Data.Error
import Data.Text (Text)

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Utils

import Control.Monad.State hiding (State)
import Control.Monad.Except

import System.Random



lookup :: (ArrowChoice c, ArrowFail String c, HasStore c Store, HasProp c Trace) => c (Text,Label) Val
lookup = (proc (x,l) -> modifyProp -< (TrUse x l :))
     &&> Concrete.lookup

store :: (ArrowChoice c, HasStore c Store, HasProp c Trace) => c (Text,Val,Label) ()
store = (proc (x,_,l) -> modifyProp -< (TrDef x l :))
    &&> Concrete.store

----------
-- Arrows
----------

type State = (Store,Trace,StdGen)
type M = StateT State (Except String)
runM :: [Statement] -> Error String ((),State)
runM ss = fromEither $ runExcept $ runStateT (runKleisli L.run ss) (initStore,initTrace,mkStdGen 0)

run :: [Statement] -> Error String (Store,Trace)
run = fmap (\(_,(st,pr,_)) -> (st,Prelude.reverse pr)) . runM

runLifted :: [Statement] -> Error String (LiftedStore,LiftedTrace)
runLifted = fmap (liftStore *** liftedTrace) . run

instance L.HasStore (Kleisli M) Store where
  getStore = Kleisli $ Prelude.const ((\ (st, _, _) -> st) <$> get)
  putStore = Kleisli $ \st -> modify (\(_,pr,gen) -> (st,pr,gen))
  modifyStore = Kleisli $ \f -> modify (\(st,pr,gen) -> (f st,pr,gen))

instance L.HasProp (Kleisli M) Trace where
  getProp = Kleisli $ const ((\ (_, pr, _) -> pr) <$> get)
  putProp = Kleisli $ \pr -> modify (\(st,_,gen) -> (st,pr,gen))
  modifyProp = Kleisli $ \f -> modify (\(st,pr,gen) -> (st,f pr,gen))

instance L.HasRandomGen (Kleisli M) where
  nextRandom = Kleisli $ \() -> do
    (st, pr, gen) <- get
    let (r, gen') = random gen
    put (st, pr, gen')
    return r

instance L.Eval (Kleisli M) Val  where
  lookup = lookup
  boolLit = Concrete.boolLit
  and = Concrete.and
  or = Concrete.or
  not = Concrete.not
  numLit = Concrete.numLit
  randomNum = Concrete.randomNum
  add = Concrete.add
  sub = Concrete.sub
  mul = Concrete.mul
  div = Concrete.div
  eq = Concrete.eq
  fixEval = Concrete.fixEval

instance L.Run (Kleisli M) Val where
  fixRun = Concrete.fixRun
  store = store
  if_ = Concrete.if_