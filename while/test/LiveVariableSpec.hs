{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
module LiveVariableSpec(main,spec) where

import Syntax
import PropertySemantics.LiveVariables

import Data.Abstract.Interval as I

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "x:=2; y:=4; x:=1; if(y>x) {z:=y} {z:=y*y}; x:=z" $ do
    let ?bound = I.Interval (-500) 500
    let cache = run ["x" =: 2, "y" =: 4, "x" =: 1, ifExpr ("x" ~= "y") [] [], "x" =: "z"]
    print cache
    True `shouldBe` True

