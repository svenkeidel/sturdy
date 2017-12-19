module Specs.DeadWritesSpec(main,spec) where

import           ConcreteSemanticsDeadWrites
import           IntervalAnalysisDeadWrites
import           WhileLanguageSoundness

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = whileSoundnessSpec runConcrete runAbstract propConcrete propAbstract
