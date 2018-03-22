{-# LANGUAGE OverloadedStrings #-}
module ConcreteSpec where

import           SharedSpecs
import           Concrete
import           Data.Error
import           PCF (Expr(..))
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  sharedSpec evalConcrete (NumVal . fromIntegral)

  describe "behavior specific to concrete semantics" $
    it "should analyse addition correctly" $ do
      -- evalConcrete [] (App (App add zero) two) `shouldBe` Success (NumVal 2)
      evalConcrete [] (App (App add one) two) `shouldBe` Success (NumVal 3)

  where
    add = Y $ Lam "add" $ Lam "x" $ Lam "y" $ IfZero "x" "y" (Succ (App (App "add" (Pred "x")) "y"))
    zero = Zero
    one = Succ zero
    two = Succ one
