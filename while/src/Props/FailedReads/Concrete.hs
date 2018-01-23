{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Props.FailedReads.Concrete where

import Prelude (String, Double, Maybe(..), Bool(..), Eq(..), Num(..), (&&), (||), (/), const, ($), (.), fst, snd)
import qualified Prelude as Prelude

import WhileLanguage (HasStore(..), HasProp(..), Statement, Expr, Label)
import qualified WhileLanguage as L

import Vals.Concrete.Val
import qualified Vals.Concrete.Semantic as Concrete

import Props.FailedReads.Prop

import Data.Error
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Utils

import Control.Monad.State
import Control.Monad.Except


lookup :: (ArrowChoice c, ArrowFail String c, HasStore c Store, HasProp c CProp) => c Text Val
lookup = proc x -> do
  store <- getStore -< x
  case Map.lookup x store of
    Just v -> returnA -< v
    Nothing -> do
      modifyProp -< Set.insert x
      failA -< "variable not found"


----------
-- Arrows
----------

type M = StateT (Store,CProp) (Except String)
runM :: [Statement] -> Error String ((),(Store,CProp))
runM ss = fromEither $ runExcept $ runStateT (runKleisli L.run ss) (initStore,initCProp)

run :: [Statement] -> Error String (Store,CProp)
run = fmap snd . runM

runLifted :: [Statement] -> Error String (LiftedStore,CProp)
runLifted = fmap (\(st, pr) -> (liftStore st, liftCProp pr)) . run

instance L.HasStore (Kleisli M) Store where
  getStore = Kleisli $ \_ -> get >>= return . fst
  putStore = Kleisli $ \st -> modify (\(_,y) -> (st,y))
  modifyStore = Kleisli $ \f -> modify (\(st,y) -> (f st,y))

instance L.HasProp (Kleisli M) CProp where
  getProp = Kleisli $ \_ -> get >>= return . snd
  putProp = Kleisli $ \pr -> modify (\(x,_) -> (x,pr))
  modifyProp = Kleisli $ \f -> modify (\(x,pr) -> (x,f pr))

instance L.Eval (Kleisli M) Val  where
  lookup = lookup
  boolLit = Concrete.boolLit
  and = Concrete.and
  or = Concrete.or
  not = Concrete.not
  numLit = Concrete.numLit
  add = Concrete.add
  sub = Concrete.sub
  mul = Concrete.mul
  div = Concrete.div
  eq = Concrete.eq
  fixEval = Concrete.fixEval

instance L.Run (Kleisli M) Val where
  fixRun = Concrete.fixRun
  store = Concrete.store
  if_ = Concrete.if_