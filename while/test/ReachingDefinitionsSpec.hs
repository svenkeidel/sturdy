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
  it "x:=5; y:=1; while(x>1){y:=x*y; x:=x-1}; z := y" $ do
    let stmts = generate (sequence ["x" =: 5, "y" =: 1, while (1 < "x") ["y" =: "x" * "y", "x" =: "x" - 1], "z" =: "y"])
    run stmts `shouldContain`
      zip (blocks stmts) [
        -- Entry
        [],                          -- 1:  x:=5
        [("x",[1])],                 -- 3:  y:=1
        [("x",[1,15]),("y",[3,11])], -- while(1 < x) {...}
        [("x",[1,15]),("y",[3,11])], -- 11: y:=x*x
        [("x",[1,15]),("y",[11])],   -- 15: x:=x-1
        [("x",[1,15]),("y",[3,11])]  -- z:=y
      ]
