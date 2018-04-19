{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module ReachingDefinitionsSpec(main,spec) where

import Prelude hiding ((<))
import Syntax
import PropertySemantics.ReachingDefinitions
import Data.Label

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "x:=5; y:=1; while(x>1){y:=x*y; x:=x-1}" $ do
    let stmts = generate (sequence ["x" =: 5, "y" =: 1, while (1 < "x") ["y" =: "x" * "y", "x" =: "x" - 1]])
    run stmts [("x",Nothing),("y",Nothing)] `shouldBe`
      zip (blocks stmts) [
        ([("x",Nothing),("y",Nothing)],                           [("x",Just 1),("y",Nothing)]),                            -- 1:  x:=5
        ([("x",Just 1),("y",Nothing)],                            [("x",Just 1),("y",Just 3)]),                             -- 3:  y:=1
        ([("x",Just 1),("x",Just 15),("y",Just 3),("y",Just 11)], [("x",Just 1),("x",Just 15),("y",Just 3),("y",Just 11)]), -- while(1 < x) {...}
        ([("x",Just 1),("x",Just 15),("y",Just 3),("y",Just 11)], [("x",Just 1),("x",Just 15),("y",Just 11)]),              -- 11: y:=x*x
        ([("x",Just 1),("x",Just 15),("y",Just 11)],              [("x",Just 15),("y",Just 11)])                            -- 15: x:=x-1
      ]

