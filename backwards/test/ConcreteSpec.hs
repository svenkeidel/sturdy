{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module ConcreteSpec where

import Data.Concrete.Error

import ConcreteInterpreter
import Syntax

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Concrete Interpreter" $ do
    it "should evaluate x + 2 to 5, if x = 3" $ do
      let env = [("x", 3)]
      eval env ("x" + 2) `shouldBe` Success (NumVal 5)

    -- TODO: Add more tests
