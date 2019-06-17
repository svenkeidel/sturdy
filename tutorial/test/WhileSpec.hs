{-# LANGUAGE OverloadedStrings #-}
module WhileSpec where

import           SturdyStyle.ConcreteInterpreter
import           Syntax

import           Data.Concrete.Error
import qualified Data.HashMap.Lazy as M

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
    run stmts `shouldBe` Success (M.fromList [(1, NumVal 10), (3, NumVal 47)])
