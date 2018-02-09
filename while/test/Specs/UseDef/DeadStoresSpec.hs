module Specs.UseDef.DeadStoresSpec(main,spec) where

import qualified Props.UseDef.Concrete as ConcreteTrace
import qualified Props.UseDef.DeadStores.Concrete as ConcreteAnalyze
import qualified Props.UseDef.DeadStores.Interval as IntervalAnalyze
import Props.UseDef.DeadStores.Prop ()

import WhileLanguageSoundness

import Vals.Interval.Val ()
import Data.GaloisConnection ()

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  whileSoundnessSpec ConcreteTrace.runLifted ConcreteAnalyze.runLifted
  whileSoundnessSpec ConcreteAnalyze.runLifted IntervalAnalyze.run
  whileSoundnessSpec ConcreteTrace.runLifted IntervalAnalyze.run


