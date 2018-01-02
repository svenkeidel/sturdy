module Specs.ControlFlowIntervalSpec(main,spec) where

import           ControlFlow.Concrete
import           ControlFlow.AbstractInterval
import           WhileLanguageSoundness

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = whileSoundnessSpec runConcrete runAbstract propConcrete propAbstract
