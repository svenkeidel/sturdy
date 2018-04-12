{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
module IntervalAnalysisSpec where

import Syntax
import ValueSemantics.Interval

import Data.Abstract.Interval as I
import Data.Abstract.Terminating
import Data.Abstract.Error
import Data.Abstract.Bounded
import qualified Data.Abstract.Store as S

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "x:=2; y:=4; x:=1; if(y>x) {z:=y} {z:=y*y}; x:=z" $ do
    let ?bound = I.Interval (-500) 500
    let res = run ["x" =: 2, "y" =: 4, "x" =: 1, ifExpr ("x" ~= "y") ["z" =: "y"] ["z" =: ("y" * "y")], "x" =: "z"]
    res `shouldBe` Terminating (Success (S.fromList [("x",num 16 16), ("y",num 4 4), ("z",num 16 16)]))

  it "while(true) {}" $ do
    let ?bound = I.Interval (-500) 500
    let res = run [while true []]
    res `shouldBe` NonTerminating

  it "x := 1; while(x < 100){x:= x + 1}"

  where
    num i j = NumVal (bounded (I.Interval i j))
