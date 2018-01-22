module Specs.SymbolicSpec where

import           ConcreteSemanticsReadVars
import           Symbolic
import           WhileLanguageSoundness

import Data.Error

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = whileSoundnessSpec runConcrete runAbstract (const $ Success ()) (const $ Success ())
