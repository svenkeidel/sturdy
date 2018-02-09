{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Props.UseDef.DeadStores.Concrete where

import Prelude (String, Double, Maybe(..), Bool(..), Eq(..), Num(..), (&&), (||), (/), const, ($), (.), fst, snd)
import qualified Prelude as Prelude

import WhileLanguage (HasStore(..), HasProp(..), Statement, Expr, Label)
import qualified WhileLanguage as L

import Vals.Concrete.Val
import qualified Vals.Concrete.Semantics as Concrete

import Props.UseDef.DeadStores.Prop

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

import Control.Monad.State hiding (State)
import Control.Monad.Except

import System.Random
import Data.Order (bottom)

lookup :: (ArrowChoice c, ArrowFail String c, HasStore c Store, HasProp c Prop) => c (Text,Label) Val
lookup = (proc (x,l) -> modifyProp -< (\(DeadStores maybeDead dead) -> DeadStores (Map.delete x maybeDead) dead))
     &&> Concrete.lookup

store :: (ArrowChoice c, HasStore c Store, HasProp c Prop) => c (Text,Val,Label) ()
store = (proc (x,_,l) -> modifyProp -< (\(DeadStores maybeDead dead) ->
          DeadStores (Map.insert x (Set.singleton l) maybeDead)
                     (dead `Set.union` lookupM x maybeDead)))
    &&> Concrete.store

----------
-- Arrows
----------

type State = (Store,Prop,StdGen)
type M = StateT State (Except String)
runM :: [Statement] -> Error String ((),State)
runM ss = fromEither $ runExcept $ runStateT (runKleisli L.run ss) (initStore,bottom,mkStdGen 0)

run :: [Statement] -> Error String (Store,FProp)
run = fmap (\(_,(st,pr,gen)) -> (st,finalizeDeadStores pr)) . runM

runLifted :: [Statement] -> Error String (LiftedStore,FProp)
runLifted = fmap (\(st, pr) -> (liftStore st, pr)) . run

instance L.HasStore (Kleisli M) Store where
  getStore = Kleisli $ \_ -> get >>= return . (\(st,_,_) -> st)
  putStore = Kleisli $ \st -> modify (\(_,pr,gen) -> (st,pr,gen))
  modifyStore = Kleisli $ \f -> modify (\(st,pr,gen) -> (f st,pr,gen))

instance L.HasProp (Kleisli M) Prop where
  getProp = Kleisli $ \_ -> get >>= return . (\(_,pr,_) -> pr)
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