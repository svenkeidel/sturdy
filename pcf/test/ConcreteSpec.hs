{-# LANGUAGE OverloadedStrings #-}
module ConcreteSpec where

import Prelude hiding (succ,pred)
import SharedSpecs
import Concrete
import Data.Error
import PCF
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  sharedSpec evalConcrete (NumVal . fromIntegral)

  describe "behavior specific to concrete semantics" $
    it "should analyse addition correctly" $ do
      evalConcrete [] (app (app add zero) two) `shouldBe` Success (NumVal 2)
      evalConcrete [] (app (app add one) two) `shouldBe` Success (NumVal 3)

  where
    add = fix $ lam "add" $ lam "x" $ lam "y" $ ifZero "x" "y" (succ (app (app "add" (pred "x")) "y"))
    one = succ zero
    two = succ one
