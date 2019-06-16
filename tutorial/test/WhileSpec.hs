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
    let stmts = [ Assign "x" (NumLit 5),
                  Assign "y" (NumLit 1),
                  While (Lt (Var "x") (NumLit 10)) [
                    Assign "y" (Add (Var "x") (Var "y")),
                    Assign "x" (Add (Var "x") (NumLit 1))
                  ]
                ]
    run stmts `shouldBe`
      Success (M.fromList [("x", NumVal 10), ("y", NumVal 36)])
