{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module IntervalAnalysisSpec where

import Prelude hiding ((<),Bool(..))
import Syntax
import IntervalAnalysis

import Data.Abstract.Interval as I
import Data.Abstract.Terminating
import Data.Abstract.Error
import Data.Abstract.Boolean(Bool(..))

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "x:=2; y:=4; x:=1; z:=0; if(y>x) {z:=y} {z:=y*y}; x:=z" $ do
    let ?bound = I.Interval (-500) 500
    let res = run 3 [("x",0),("y",1),("z",2)] ["x" =: 2, "y" =: 4, "x" =: 1, ifExpr ("x" ~= "y") ["z" =: "y"] ["z" =: "y" * "y"], "x" =: "z"]
    res `shouldBe` Terminating (Success [(0,num 16 16), (1,num 4 4), (2,num 16 16)])

  it "9 < 10 == true" $ do
    let ?bound = I.Interval (-500) 500
    let res = run 3 [("x",0)] ["x" =: 9 < 10]
    res `shouldBe` Terminating (Success [(0, BoolVal True)])

  it "10 < 10 == false" $ do
    let ?bound = I.Interval (-500) 500
    let res = run 3 [("x",0)]["x" =: 10 < 10]
    res `shouldBe` Terminating (Success [(0, BoolVal False)])

  it "while(true) {}" $ do
    let ?bound = I.Interval (-500) 500
    let res = run 3 [] [while true []]
    res `shouldBe` NonTerminating

  it "x := 1; while(x < 10){x:= x + 1}" $ do
    let ?bound = I.Interval (-500) 500
    let res = run 15 [("x",0)] ["x" =: 1, while ("x" < 10) ["x" =: "x" + 1]]
    res `shouldBe` Terminating (Success [(0, num 10 10)])

  it "x := 1; while(x < 10){x:= x + 1; while(x > 0) {x := x - 1}}" $ do
    let ?bound = I.Interval (-500) 500
    let res = run 15 [] ["x" =: 1, while ("x" < 10) ["x" =: "x" + 1, while (0 < "x") ["x" =: "x" - 1 ]]]
    res `shouldBe` NonTerminating

  it "x := 1; y := 1; while(x < 3){x:= x + 1; i := 1, while(i < 2) {y := y + 1}}" $ do
    let ?bound = I.Interval (-500) 500
    let res = run 10 [("x",0),("y",1)] ["x" =: 1, "y" =: 1, while ("x" < 3) ["x" =: "x" + 1, "i" =: 1, while ("i" < 2) ["i" =: "i" + 1, "y" =: "y" + 1 ]]]
    res `shouldBe` Terminating (Success [(0, num 3 3),(1,num 3 3),(14,num 2 2)])

  where
    num i j = NumVal (I.Interval i j)
