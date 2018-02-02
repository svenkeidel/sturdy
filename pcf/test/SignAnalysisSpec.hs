{-# LANGUAGE OverloadedStrings #-}
module SignAnalysisSpec where

import           Data.Error
import qualified Data.HashMap.Lazy as M
import qualified Data.Powerset as P
import           Data.Sign
import qualified Data.Text as T
import qualified PCF as E
import           SignAnalysis hiding (Top)

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "evalSign" $ do
    it "should look up a bound variable" $
      let k = "x"
          v = NumVal Positive
      in evalSign (M.singleton k v) (E.Var k) `shouldBe` Success v

    it "should fail when looking up an unbound variable" $
      let k = "x"
      in evalSign M.empty (E.Var k) `shouldBe` Error "Variable \"x\" not bound"

    it "should create a closure" $
      let k = "x"
          v = E.Succ (E.Var k)
      in evalSign M.empty (E.Lam k v) `shouldBe` Success (ClosureVal (P.singleton (Closure k v M.empty)))

    it "should apply a function" $
      let k = "x"
          v = E.Lam k (E.Succ (E.Var k)) in
        evalSign M.empty (E.App v E.Zero) `shouldBe` Success (NumVal Positive)

    it "should fail when applying something other than a function" $
      evalSign M.empty (E.App E.Zero E.Zero) `shouldBe` Error "Expected a closure"

    --it "will recurse infinitely when using Y" $
    --  evalSign M.empty (E.Y (E.Succ E.Zero)) `shouldBe` Success (NumVal 1)

    it "should correctly give the sign of zero" $
       evalSign M.empty E.Zero `shouldBe` Success (NumVal Zero)

    it "should correctly give the sign of succ of 0" $
      evalSign M.empty (E.Succ E.Zero) `shouldBe` Success (NumVal Positive)

    it "should correctly give the sign of succ of succ of 0" $
      evalSign M.empty (E.Succ (E.Succ E.Zero)) `shouldBe` Success (NumVal Positive)

    it "should correctly give the sign of pred of 0" $
      evalSign M.empty (E.Pred E.Zero) `shouldBe` Success (NumVal Negative)

    it "should correctly give the sign of pred of pred of 0" $
      evalSign M.empty (E.Pred (E.Pred E.Zero)) `shouldBe` Success (NumVal Negative)

    it "should correctly overapproximate the sign of pred of succ of 0" $
      evalSign M.empty (E.Pred (E.Succ E.Zero)) `shouldBe` Success (NumVal Top)

    it "should correctly overapproximate the sign of succ of pred of 0" $
      evalSign M.empty (E.Succ (E.Pred E.Zero)) `shouldBe` Success (NumVal Top)

    it "should fail when using succ on something other than a number" $
      let bogus = E.Lam (T.singleton 'x') E.Zero
      in evalSign M.empty (E.Succ bogus) `shouldBe` Error "Expected a number as argument for 'succ'"

    it "should fail when using pred on something other than a number" $
      let bogus = E.Lam (T.singleton 'x') E.Zero
      in evalSign M.empty (E.Pred bogus) `shouldBe` Error "Expected a number as argument for 'pred'"

    it "should execute the then branch on IfZero on zero" $
      evalSign M.empty (E.IfZero E.Zero (E.Succ E.Zero) E.Zero)
        `shouldBe` Success (NumVal Positive)

    it "should execute the else branch on IfZero on non-zero" $
      evalSign M.empty (E.IfZero (E.Succ E.Zero) (E.Succ E.Zero) E.Zero)
        `shouldBe` Success (NumVal Zero)

    it "should execute both branches on IfZero on top" $
      evalSign M.empty (E.IfZero (E.Succ (E.Pred E.Zero)) (E.Succ E.Zero) E.Zero) `shouldBe` Success (NumVal Top)

    it "should fail when using a non-number condition for IfZero" $
      let bogus = E.Lam (T.singleton 'x') E.Zero
      in evalSign M.empty (E.IfZero bogus E.Zero E.Zero) `shouldBe` Error "Expected a number as condition for 'ifZero'"
