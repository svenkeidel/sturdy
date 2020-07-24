{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module IntervalSpec where

import Data.Abstract.Error
import Data.Abstract.Interval

import IntervalAnalysis
import Syntax

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Interval Analysis" $ do
    it "should evaluate x + 2 to [2,5], if x ∈ [0,3]" $ do
      let env = [("x", iv 0 3)]
      eval ("x" + 2) env `shouldBe` Success (iv 2 5)

  describe "Backwards Analysis" $ do
    it "should evaluate x + 2 to [2,5], if x ∈ [0,3]" $ do
      let env = [("x", iv 0 3)]
      eval ("x" + 2) env `shouldBe` Success (iv 2 5)

    -- TODO: Add more tests

    -- TODO: Add more tests
  where
    iv :: Int -> Int -> Val
    iv i j = NumVal (Interval i j)
