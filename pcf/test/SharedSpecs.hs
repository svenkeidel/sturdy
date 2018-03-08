{-# LANGUAGE OverloadedStrings #-}
module SharedSpecs where

import           Data.Error
import           Data.Text (Text)
import           PCF (Expr)
import qualified PCF as E
import           Test.Hspec

sharedSpec :: (Show v, Eq v) => ([(Text,v)] -> Expr -> Error String v) -> (Int -> v)-> Spec
sharedSpec eval fromInt = describe "shared language behavior" $ do
  it "should look up a bound variable" $
    eval [("x",fromInt 5)] "x" `shouldBe` Success (fromInt 5)

  it "should fail when looking up an unbound variable" $
    eval [] "x" `shouldBe` Error "Variable \"x\" not bound"

  it "should apply a function" $
    eval [] (E.App (E.Lam "x" (E.Succ "x")) E.Zero) `shouldBe` Success (fromInt 1)

  it "should fail when applying something other than a function" $
    eval [] (E.App E.Zero E.Zero) `shouldBe` Error "Expected a closure"

  it "should compute 0 + 1 + 1 = 2" $
    eval [] (E.Succ (E.Succ E.Zero)) `shouldBe` Success (fromInt 2)

  it "should fail when using succ on something other than a number" $
    eval [] (E.Succ (E.Lam "x" E.Zero)) `shouldBe` Error "Expected a number as argument for 'succ'"

  it "should fail when using pred on something other than a number" $
    eval [] (E.Pred (E.Lam "x" E.Zero)) `shouldBe` Error "Expected a number as argument for 'pred'"

  it "should execute the then branch on IfZero on zero" $
    eval [] (E.IfZero E.Zero (E.Succ E.Zero) E.Zero)
      `shouldBe` Success (fromInt 1)

  it "should execute the else branch on IfZero on completely non-zero" $
    eval [] (E.IfZero (E.Succ E.Zero) (E.Succ E.Zero) E.Zero)
      `shouldBe` Success (fromInt 0)

  it "should fail when using a non-number condition for IfZero" $
    eval [] (E.IfZero (E.Lam "x" E.Zero) E.Zero E.Zero) `shouldBe` Error "Expected a number as condition for 'ifZero'"
