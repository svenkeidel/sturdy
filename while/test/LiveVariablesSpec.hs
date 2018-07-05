{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module LiveVariablesSpec(main,spec) where

import Prelude hiding ((<))
import Syntax
import PropertySemantics.LiveVariables
import Data.Label

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "x:=5; y:=1; while(x>1){y:=x*y; x:=x-1}; z := y" $ do
    pendingWith "does not terminate"
    let stmts = generate (sequence ["x" =: 5, "y" =: 1, while (1 < "x") ["y" =: "x" * "y", "x" =: "x" - 1], "z" =: "y"])
    run stmts `shouldContain`
      zip (blocks stmts) [
        -- Entry
        [],                          -- 1:  x:=5
        [],                 -- 3:  y:=1
        [], -- while(1 < x) {...}
        [], -- 11: y:=x*x
        [],   -- 15: x:=x-1
        []  -- z:=y
      ]
