{-# LANGUAGE OverloadedStrings #-}
module SharedSpecs where

import Prelude hiding (succ,pred)

import Control.Monad.State

import Data.Error
import Data.Text (Text)
import Data.Label

import PCF
import Test.Hspec

sharedSpec :: (Show v, Eq v) => ([(Text,v)] -> State Label Expr -> Error String v) -> (Int -> v)-> Spec
sharedSpec eval fromInt = describe "shared language behavior" $ do
  it "should look up a bound variable" $
    eval [("x",fromInt 5)] "x" `shouldBe` Success (fromInt 5)

  it "should fail when looking up an unbound variable" $
    eval [] "x" `shouldBe` Error "Variable \"x\" not bound"

  it "should apply a function" $
    eval [] (app (lam "x" (succ "x")) zero) `shouldBe` Success (fromInt 1)

  it "should fail when applying something other than a function" $
    eval [] (app zero zero) `shouldBe` Error "Expected a closure"

  it "should compute 0 + 1 + 1 = 2" $
    eval [] (succ (succ zero)) `shouldBe` Success (fromInt 2)

  it "should fail when using succ on something other than a number" $
    eval [] (succ (lam "x" zero)) `shouldBe` Error "Expected a number as argument for 'succ'"

  it "should fail when using pred on something other than a number" $
    eval [] (pred (lam "x" zero)) `shouldBe` Error "Expected a number as argument for 'pred'"

  it "should execute the then branch on IfZero on zero" $
    eval [] (ifZero zero (succ zero) zero)
      `shouldBe` Success (fromInt 1)

  it "should execute the else branch on IfZero on completely non-zero" $
    eval [] (ifZero (succ zero) (succ zero) zero)
      `shouldBe` Success (fromInt 0)

  it "should fail when using a non-number condition for IfZero" $
    eval [] (ifZero (lam "x" zero) zero zero) `shouldBe` Error "Expected a number as condition for 'ifZero'"
