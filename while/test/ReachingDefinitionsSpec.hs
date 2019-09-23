{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ImplicitParams #-}
module ReachingDefinitionsSpec(main,spec) where

import Prelude hiding ((<))
import Syntax
import ReachingDefinitionsAnalysis
import Data.Abstract.Interval as I

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  it "x:=2; y:=4; x:=1; z:=0; if(x==y) {z:=y} {z:=y*y}; x:=z" $
    let ?bound = I.Interval (-500) 500 in
    let stmts = [
                  "x" =: 2,
                  "y" =: 4,
                  "x" =: 1,
                  "z" =: 0,
                  ifExpr ("x" ~= "y") [
                    "z" =: "y"
                  ] [
                     "z" =: "y" * "y"
                  ],
                  "x" =: "z"
                ]
    in run 10 stmts `shouldMatchList` [
          -- Entry-Set
          (1, []),                                  -- x := 2
          (3, [("x",[1])]),                         -- y := 4
          (5, [("x",[1]), ("y",[3])]),              -- x := 1
          (7, [("x",[5]), ("y",[3])]),              -- z := 0
          (8, [("x",[5]), ("y",[3]), ("z",[7])]),   -- if (x == y) {
                                                    --   z := y  // is never executed.
                                                    -- } else {
          (19,[("x",[5]), ("y",[3]), ("z",[7])]),   --   z := y * y
                                                    -- }
          (21,[("x",[5]), ("y",[3]), ("z",[19,7])]) -- x := z
        ]

  it "x:=5; y:=1; while(1<x){y:=x*y; x:=x-1}; z := y" $
    let ?bound = I.Interval 0 10 in
    let stmts = ["x" =: 5,
                 "y" =: 1,
                 while (1 < "x") [
                   "y" =: "x" * "y",
                   "x" =: "x" - 1
                 ],
                 "z" =: "y"
                ]
    in run 1 stmts `shouldMatchList` [
         -- Entry-Set
         (1, []),                          -- x:=5
         (3, [("x",[1])]),                 -- y:=1
         (4, [("x",[1,16]),("y",[3,12])]), -- while(1 < x) {
         (12,[("x",[1,16]),("y",[3,12])]), --   y:=x*x
         (16,[("x",[1,16]),("y",[12])]),   --   x:=x-1
                                           -- }
         (18,[("x",[1,16]), ("y",[3,12])]) -- z:=y
       ]


  it "x := 1; y := 1; while(x < 3){x:= x + 1; i := 1, while(i < 2) {y := y + 1}}; z:=y" $
    let ?bound = I.Interval 0 10 in
    let stmts = [
                  "x" =: 1,
                  "y" =: 1,
                  while ("x" < 3) [
                    "x" =: "x" + 1,
                    "i" =: 1,
                    while ("i" < 2) [
                      "i" =: "i" + 1,
                      "y" =: "y" + 1
                    ]
                  ],
                  "z" =: "y"
                ]
    in run 1 stmts `shouldMatchList` [
         -- Entry-Set
         (1, []),                                          -- x := 1
         (3, [("x",[1])]),                                 -- y := 1
         (4, [("x",[1,12]),("y",[3,27])]),                 -- while(x < 3) {
         (12,[("x",[1,12]),("y",[3,27])]),                 --   x := x + 1
         (14,[("x",[12]),  ("y",[3,27])]),                 --   i := 1
         (15,[("x",[12]),  ("y",[3,27]), ("i",[23,14])]),  --   while(i < 2) {
         (23,[("x",[12]),  ("y",[3,27]), ("i",[23,14])]),  --     i := i + 1
         (27,[("x",[12]),  ("y",[3,27]), ("i",[23])]),     --     y := y + 1 } }
         (29,[("x",[1,12]),("y",[3,27])])                  -- z := y
       ]
