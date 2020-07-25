{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module IntervalSpec where

import Data.Order
import Data.Abstract.Error

import IntervalAnalysis

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "the forward analysis" $ do
    context "when analyzing addition" $ do
      it "evaluates the expression x + 2 to [2,5], if x ∈ [0,3]" $ do
        let env = [("x", iv 0 3)]
        eval ("x" + 2) env `shouldBe` Success (iv 2 5)

  describe "the backwards analysis" $ do
    context "when analyzing a number literal" $ do
      it "leaves the environment untouched if the number is in the interval." $ do
        let env = [("x", iv 0 3)]
        evalᴮ 2 env (iv 1 3) `shouldBe` env
        evalᴮ 2 env (iv 2 4) `shouldBe` env
        evalᴮ 2 env (iv 3 5) `shouldBe` bottom

    context "when analyzing addition" $ do
      it "refines the variable x in x + 2" $ do
        let env = [("x", iv 0 3)]
        evalᴮ ("x" + 2) env (iv 0 1) `shouldBe` bottom
        evalᴮ ("x" + 2) env (iv 1 2) `shouldBe` [("x", iv 0 0)]
        evalᴮ ("x" + 2) env (iv 2 3) `shouldBe` [("x", iv 0 1)]
        evalᴮ ("x" + 2) env (iv 3 4) `shouldBe` [("x", iv 1 2)]
        evalᴮ ("x" + 2) env (iv 4 5) `shouldBe` [("x", iv 2 3)]
        evalᴮ ("x" + 2) env (iv 5 6) `shouldBe` [("x", iv 3 3)]
        evalᴮ ("x" + 2) env (iv 6 7) `shouldBe` bottom

