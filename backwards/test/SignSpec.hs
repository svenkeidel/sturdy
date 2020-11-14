{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module SignSpec where

import Prelude hiding ((&&), (||), not, (==), (<=))

import Data.Order
import Data.Abstract.Error

import SignAnalysis

import Syntax

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do  
  describe "the forward analysis" $ do
    context "when analyzing addition" $ do
      it "evaluates the expression Pos + Pos to Pos" $ do
        let env = [("x", sPos), ("y", sPos)]
        eval ("x" + "y") env `shouldBe` Success (sPos)

  describe "the forward analysis" $ do
    context "when analyzing addition" $ do
      it "evaluates the expression Pos + Pos to Pos" $ do
        let env = [("x", sPos), ("y", sNeg)]
        eval ("x" + "y") env `shouldBe` Success (sTop)

  describe "the forward analysis" $ do
    context "when analyzing addition" $ do
      it "evaluates the expression Pos - Pos to Top" $ do
        let env = [("x", sPos), ("y", sPos)]
        eval ("x" - "y") env `shouldBe` Success (sTop)

  describe "the forward analysis" $ do
    context "when analyzing addition" $ do
      it "evaluates the expression Neg - Zero to Neg" $ do
        let env = [("x", sNeg), ("y", sZero)]
        eval ("x" - "y") env `shouldBe` Success (sNeg)

  describe "the forward analysis" $ do
    context "when analyzing addition" $ do
      it "evaluates the expression Pos - Neg to Pos" $ do
        let env = [("x", sPos), ("y", sNeg)]
        eval ("x" - "y") env `shouldBe` Success (sPos)

  describe "the forward analysis" $ do
    context "when analyzing addition" $ do
      it "evaluates the expression Neg * Neg to Pos" $ do
        let env = [("x", sNeg), ("y", sNeg)]
        eval ("x" * "y") env `shouldBe` Success (sPos)

  describe "the forward analysis" $ do
    context "when analyzing addition" $ do
      it "evaluates the expression Neg * Pos to Neg" $ do
        let env = [("x", sNeg), ("y", sPos)]
        eval ("x" * "y") env `shouldBe` Success (sNeg)

  --- backward analysis

  describe "the backwards analysis of the Sign Domains" $ do
    context "when analyzing Pos + Pos -> Pos" $ do
      it "does not learn anything new about the input domains." $ do
        let env = [("x", sPos), ("y", sPos)]
        evalᴮ ("x" + "y") env (sPos) `shouldBe` [("x", sPos), ("y", sPos)]

  describe "the backwards analysis of the Sign Domains" $ do
    context "when analyzing Top + Neg -> Zero" $ do
      it "learns that we can substitute Top by Pos yielding: Pos + Neg -> Zero" $ do
        let env2 = [("x", sTop), ("y", sNeg)]
        evalᴮ ("x" + "y") env2 (sZero) `shouldBe` [("x", sPos), ("y", sNeg)]

  describe "the backwards analysis of the Sign Domains" $ do
    context "when analyzing Top + Neg + 2 -> Zero" $ do
      it "does not learn anything new about the input domains." $ do
        let env3 = [("x", sTop), ("y", sNeg)]
        evalᴮ ("x" + "y" + 2) env3 (sZero) `shouldBe` [("x", sTop), ("y", sNeg)]
  
  describe "the backwards analysis of the Sign Domains" $ do
    context "when analyzing Top + 2 -> Zero" $ do
      it "learns that we can substitute Top by Neg." $ do
        let env3 = [("x", sTop)]
        evalᴮ ("x" + 2) env3 (sZero) `shouldBe` [("x", sNeg)]

  describe "the backwards analysis of the Sign Domains" $ do
    context "when analyzing Zero - Top -> Zero" $ do
      it "learns that we can substitute Top by Zero." $ do
        let env = [("x", sZero), ("y", sTop)]
        evalᴮ ("x" - "y") env (sZero) `shouldBe` [("x", sZero), ("y", sZero)]

  describe "the backwards analysis of the Sign Domains" $ do
    context "when analyzing Neg * Top -> Neg" $ do
      it "learns that we can substitute Top by Pos." $ do
        let env = [("x", sNeg), ("y", sTop)]
        evalᴮ ("x" * "y") env (sNeg) `shouldBe` [("x", sNeg), ("y", sPos)]

  describe "the backwards analysis of the Sign Domains" $ do
    context "when analyzing 4*(Top + 2) -> Pos" $ do
      it "does not learn anything new about the input domains." $ do
        let env = [("x", sTop)]
        evalᴮ (4*("x" + 2)) env (sPos) `shouldBe` [("x", sTop)]

  describe "the backwards analysis of the Sign Domains" $ do
    context "when analyzing Pos + Neg * Top -> Neg" $ do
      it "does not learn anything new about the input domains." $ do
        let env = [("x", sPos), ("y", sNeg), ("z", sTop)]
        evalᴮ ("x" + "y" * "z") env (sPos) `shouldBe` [("x", sPos), ("y", sNeg), ("z", sTop)]

  -- Backwards Boolean Domain

  describe "the backwards analysis of the Bool Domains" $ do
    context "when analyzing Top and True -> False" $ do
      it "Learns that we can substitute Top by False" $ do
        let env = [("x", bTop), ("y", bTrue)]
        evalᴮ ("x" && "y") env (bFalse) `shouldBe` [("x", bFalse), ("y", bTrue)]

  describe "the backwards analysis of the Bool Domains" $ do
    context "when analyzing Not Top -> False" $ do
      it "Learns that we can substitute Top by True" $ do
        let env = [("x", bTop)]
        evalᴮ (not "x") env (bFalse) `shouldBe` [("x", bTrue)]
  
  describe "the backwards analysis of the Bool Domains" $ do
    context "when analyzing Not (Top and True) -> False" $ do
      it "Learns that we can substitute Top by True" $ do
        let env = [("x", bTop), ("y", bTrue)]
        evalᴮ (not ("x" && "y")) env (bFalse) `shouldBe` [("x", bTrue), ("y", bTrue)]


  describe "the backwards analysis of the Bool Domains" $ do
    context "when analyzing Top < Neg -> True" $ do
      it "Learns that we can substitute Top by Neg" $ do
        let env = [("x", sTop), ("y", sNeg)]
        evalᴮ ("x" <= "y") env (bTrue) `shouldBe` [("x", sNeg), ("y", sNeg)]

 -- Backwards type domains
  describe "the backwards analysis of the Type Domains" $ do
    context "when analyzing Top + Neg -> True" $ do
      it "Learns that we can substitute Top by Neg" $ do
        let env = [("x", sPos), ("y", sNeg), ("z", tTop)]
        evalᴮ (("x" + "y") `typeof` ("z")) env (bTrue) `shouldBe` [("x", sPos), ("y", sNeg), ("z", tNumber)]


-- forward tests 

  describe "the forward analysis" $ do
    context "when analyzing addition" $ do
      it "evaluates the expression Top and Top to Top" $ do
        let env = [("x", bTop), ("y", bTop)]
        eval ("x" && "y") env `shouldBe` Success (bTop)

  describe "the forward analysis" $ do
    context "when analyzing addition" $ do
      it "evaluates the expression True and False to False" $ do
        let env = [("x", bTrue), ("y", bFalse)]
        eval ("x" && "y") env `shouldBe` Success (bFalse)

  describe "the forward analysis" $ do
    context "when analyzing addition" $ do
      it "evaluates the expression True == true to True" $ do
        let env = [("x", bTrue)]
        eval ("x" == true) env `shouldBe` Success (bTrue)

  describe "the forward analysis" $ do
    context "when analyzing addition" $ do
      it "evaluates the expression Top and False to False" $ do
        let env = [("x", bTop), ("y", bFalse)]
        eval ("x" && "y") env `shouldBe` Success (bFalse)

  describe "the forward analysis" $ do
    context "when analyzing Leq" $ do
      it "evaluates the expression Neg <= Pos to True" $ do
        let env = [("x", sNeg), ("y", sPos)]
        eval ("x" <= "y") env `shouldBe` Success (bTrue)

  describe "the forward analysis" $ do
    context "when analyzing typeOf" $ do
      it "evaluates the expression (Top && Top) typeOf Boolean to True" $ do
        let env = [("x", bTop), ("y", bTrue)]
        eval (("x" && "y") `typeof` (boolean)) env `shouldBe` Success (bTrue)

  describe "the forward analysis" $ do
    context "when analyzing typeOf" $ do
      it "evaluates the expression (Top + Neg) typeOf Numver to True" $ do
        let env = [("x", sTop), ("y", sNeg)]
        eval (("x" + "y") `typeof` (number)) env `shouldBe` Success (bTrue)

  describe "the forward analysis" $ do
    context "when analyzing If" $ do
      it "evaluates the expression If x: Top && True Then x: Top && True Else Top || True to True with help of the Backward Analysis." $ do
        let env = [("x", bTop), ("y", bTrue), ("z", sNeg), ("a", sZero)]
        eval (If ("x" && "y") ("x" && "y") ("x" || "y")) env `shouldBe` Success (bTrue)

  describe "the forward analysis" $ do
    context "when analyzing If" $ do
      it "evaluates the expression If x: Top <= y: Neg Then x: Top * z: Pos Else y: Neg to Neg with help of the Backward Analysis." $ do
        let env = [("x", sTop), ("y", sNeg), ("z", sPos)]
        eval (If ("x" <= "y") ("x" * "z") ("y")) env `shouldBe` Success (sNeg)
  
  describe "the forward analysis" $ do
    context "when analyzing If" $ do
      it "evaluates the expression If (x: Neg <= y: Pos) && not (z : Pos <= 0) then x : Neg * z : Pos else y: Neg to Neg with help of the Backward Analysis." $ do
        let env = [("x", sTop), ("y", sNeg), ("z", sPos), ("a", sZero)]
        eval (If (("x" <= "y") && (not ("z" <= 0))) ("x" * "z") ("y")) env `shouldBe` Success (sNeg)

  describe "the forward analysis" $ do
    context "when analyzing If" $ do
      it "evaluates the expression If x: Top <= y: Neg Then x: Top * z: Pos Else x: Neg * y: Neg - 1 to Top and cant substitute Top by Neg." $ do
        let env = [("x", sTop), ("y", sNeg), ("z", sPos)]
        eval (If ("x" <= "y") ("x" * "z") ("x" * "y" - 1)) env `shouldBe` Success (sTop)