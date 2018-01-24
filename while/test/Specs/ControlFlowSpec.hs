module Specs.ControlFlowSpec(main,spec) where

import qualified Props.ControlFlow.Concrete as Concrete
import qualified Props.ControlFlow.Interval as Interval

import WhileLanguageSoundness

import Vals.Interval.Val ()
import Data.GaloisConnection ()

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = whileSoundnessSpec Concrete.runLifted Interval.run

