module Specs.ReadVarsSpec(main,spec) where

import           ConcreteSemanticsReadVars
import           IntervalAnalysisReadVars
import           WhileLanguageSoundness

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = whileSoundnessSpec runConcrete runAbstract propConcrete propAbstract
