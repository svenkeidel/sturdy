module Specs.StatementTraceSpec(main,spec) where

import           ConcreteSemanticsStatementTrace
import           IntervalAnalysisStatementTrace
import           WhileLanguageSoundness

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = whileSoundnessSpec runConcrete runAbstract propConcrete propAbstract
