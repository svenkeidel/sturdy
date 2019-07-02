{-# LANGUAGE OverloadedStrings #-}
module WhileAbstractSpec where

import           SturdyStyle.AbstractInterpreter
import           Syntax

import           Data.Abstract.Error
import qualified Data.Abstract.Map as M
import           Data.Abstract.Terminating
import           Data.Abstract.InfiniteNumbers

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "x:=1; y:=1; while(x < 10){y:=x+y; x:=x+1}" $ do
    let stmts = [ assign "x" (numLit 1),
                  assign "y" (numLit 2),
                  while (lt (var "x") (numLit 10)) [
                    assign "y" (add (var "x") (var "y")),
                    assign "x" (add (var "x") (numLit 1))
                  ]
                ]
    run 20 stmts `shouldBe` Terminating (Success (M.fromList [(1, NumVal (Interval 11 11)), (3, NumVal (Interval 57 57))]))
