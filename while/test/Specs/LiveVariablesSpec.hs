module Specs.LiveVariablesSpec(main,spec) where

import qualified Props.LiveVariables.Concrete as Concrete
import qualified Props.LiveVariables.Interval as Interval

import WhileLanguageSoundness

import Vals.Interval.Val ()
import Data.GaloisConnection ()

import Data.Map

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = whileSoundnessSpec Concrete.runLifted Interval.run

