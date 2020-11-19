{-# LANGUAGE OverloadedStrings #-}
module ConcreteSpec where

import Prelude hiding (succ,pred)
import SharedSpecs
import ConcreteInterpreter
import Data.Concrete.Error
import Syntax
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  sharedSpec (toEither . evalConcrete []) (NumVal . fromIntegral)

  describe "behavior specific to concrete semantics" $
    it "should analyse addition correctly" $ do
      evalConcrete [] (let_ [("add",add)] (app "add" [zero,two])) `shouldBe` Success (NumVal 2)
      evalConcrete [] (let_ [("add",add)] (app "add" [one,two])) `shouldBe` Success (NumVal 3)
  where
    add = lam ["x","y"] $ ifZero "x" "y" (succ (app "add" [pred "x","y"]))
    one = succ zero
    two = succ one
