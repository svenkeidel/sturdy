{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module ConcreteSpec where

import Data.Concrete.Error

import ConcreteInterpreter

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "the concrete interpreter" $
    context "when evaluating an addition" $
      it "reduces the expression x + 2 to 5, if x = 3" $ do
        let env = [("x", 3)]
        eval ("x" + 2) env `shouldBe` Success (NumVal 5)

  -- TODO: Add more tests
