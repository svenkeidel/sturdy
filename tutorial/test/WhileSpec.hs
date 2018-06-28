{-# LANGUAGE OverloadedStrings #-}
module WhileSpec where

import           ArrowStyle
import           Syntax

import qualified Data.Map as Store

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "x:=1; y:=1; while(x < 10){y:=x+y; x:=x+1}" $ do
    let stmts = [ Assign "x" (NumLit 5),
                  Assign "y" (NumLit 1),
                  While (Lt (Var "x") (NumLit 10)) [
                    Assign "y" (Add (Var "x") (Var "y")),
                    Assign "x" (Add (Var "x") (NumLit 1))
                  ]
                ]
    run' stmts Store.empty `shouldBe`
      Right (Store.fromList [("x", NumVal 10), ("y", NumVal 36)])
