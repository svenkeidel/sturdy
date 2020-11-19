{-# LANGUAGE OverloadedStrings #-}
module WhileConcreteSpec where

import           SturdyStyle.ConcreteInterpreter
import           Syntax

import           Data.Concrete.Error
import qualified Data.HashMap.Lazy as M
import           Data.Label

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "x:=1; y:=1; while(x < 10){y:=x+y; x:=x+1}" $ do
    let stmts = [ assign "x" (numLit 1),
                  assign "y" (numLit 2),
                  while (lt (var "x") (numLit 10)) [
                    assign "y" (add (var "x") (var "y")),
                    assign "x" (add (var "x") (numLit 1))
                  ]
                ]
    let (res, env) = runWithInitVals [("x", NumVal 0), ("y", NumVal 0)] stmts
    res `shouldBe` Success (M.fromList [(env M.! "x", NumVal 10),
                                        (env M.! "y", NumVal 47)])

  it "x:=1; y:=2" $ do
    let stmts = [ assign "x" (numLit 1),
                  assign "y" (numLit 2) ]
    let (res, env) = runWithInitVals [("x", NumVal 0), ("y", NumVal 0)] stmts
    res `shouldBe` Success (M.fromList [(env M.! "x", NumVal 1),
                                        (env M.! "y", NumVal 2)])

  it "x:=0; y:=1; while(y < (z+1)){x:=x+y; y:=y+1}" $ do
    let stmts = [ assign "x" (numLit 0),
                  assign "y" (numLit 1),
                  while (lt (var "y") (add (var "z") (numLit 1))) [
                    assign "x" (add (var "x") (var "y")),
                    assign "y" (add (var "y") (numLit 1))
                  ]
                ]
    let env0 = [("x",NumVal 0),("y",NumVal 0),("z",NumVal 10)]
    let (res, env) = runWithInitVals env0 stmts
    res `shouldBe` Success (M.fromList [(env M.! "x", NumVal 55),
                                        (env M.! "y", NumVal 11),
                                        (env M.! "z", NumVal 10)])
