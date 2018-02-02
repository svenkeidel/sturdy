module Specs.FailedReadsSpec(main,spec) where

import qualified Props.FailedReads.Concrete as Concrete
import qualified Props.FailedReads.Interval as Interval

import WhileLanguageSoundness

import Vals.Interval.Val ()
import Data.GaloisConnection ()

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = whileSoundnessSpec Concrete.runLifted Interval.run
