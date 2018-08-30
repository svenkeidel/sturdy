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
spec = do
  it "x:=5; y:=x" $ do
    let stmts = generate (sequence ["x" =: 5, "y" =: "x"])
    run stmts `shouldContain`
      zip (blocks stmts) [
        -- Entry Sets
        [],    -- x:=5
        ["x"]  -- y:=x
      ]

  it "x:=5; x:=6; y:=x" $ do
    let stmts = generate (sequence ["x" =: 5, "x" =: 6, "y" =: "x"])
    run stmts `shouldContain`
      zip (blocks stmts) [
        -- Entry Sets
        [],    -- x:=5
        [],    -- x:=6
        ["x"]  -- y:=x
      ]

  it "x:=5; {y:=6}; z:=x" $ do
    pendingWith "Cannot handle scoping yet"
    let stmts = generate (sequence ["x" =: 5, begin ["y" =: 6], "z" =: "x"])
    run stmts `shouldContain`
      zip (blocks stmts) [
        -- Entry Sets
        [],    -- x:=5
        ["x"],    -- y:=6
        ["x"]  -- y:=x
      ]


  it "x:=5; y:=1; while(x>1){y:=x*y; x:=x-1}; z := y" $ do
    let stmts = generate (sequence ["x" =: 5, "y" =: 1, while (1 < "x") ["y" =: "x" * "y", "x" =: "x" - 1], "z" =: "y"])
    run stmts `shouldContain`
      zip (blocks stmts) [
        -- Entry
        [],        -- x:=5
        ["x"],     -- y:=1
        ["x","y"], -- while(1 < x) {...}
        ["x","y"],   -- y:=x*y
        ["x"],       -- x:=x-1
        ["y"]      -- z:=y
      ]
