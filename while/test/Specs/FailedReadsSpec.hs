module Specs.FailedReadsSpec(main,spec) where

import           ConcreteSemanticsFailedReads
import           IntervalAnalysisFailedReads
import           WhileLanguageSoundness

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = whileSoundnessSpec runConcrete runAbstract propConcrete propAbstract
