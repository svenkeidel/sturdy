module Specs.DeadWritesSpec(main,spec) where

import qualified Props.DeadWrites.Concrete as Concrete
import qualified Props.DeadWrites.Interval as Interval

import WhileLanguageSoundness

import Vals.Interval.Val ()
import Data.GaloisConnection ()

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = whileSoundnessSpec Concrete.runLifted Interval.run

