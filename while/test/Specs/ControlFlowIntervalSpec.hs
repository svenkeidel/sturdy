module Specs.ControlFlowIntervalSpec(main,spec) where

import           Props.ControlFlow.Concrete
import           Props.ControlFlow.Interval
import           WhileLanguageSoundness

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = whileSoundnessSpec runConcrete runAbstract propConcrete propAbstract
