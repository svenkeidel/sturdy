{-# LANGUAGE OverloadedStrings #-}
module IntervalAnalysisSpec where

import           Data.Bounded
import           Data.Error
import qualified Data.HashMap.Lazy as M
import qualified Data.Interval as I
import qualified Data.Powerset as P
import qualified Data.Text as T
import           IntervalAnalysis
import qualified PCF as E
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "evalInterval" $ do
    it "should look up a bound variable" $
      let k = "x"; v = NumVal 5
      in evalInterval lim (M.singleton k v) (E.Var k) `shouldBe` Success v

    it "should fail when looking up an unbound variable" $
      evalInterval lim M.empty (E.Var "x") `shouldBe` Error "Variable \"x\" not bound"

    it "should create a closure" $
      let v = E.Succ (E.Var "x")
      in evalInterval lim M.empty (E.Lam "x" v) `shouldBe` Success (ClosureVal (P.singleton (Closure "x" v M.empty)))

    it "should apply a function" $
      evalInterval lim M.empty (E.App (E.Lam "x" (E.Succ "x")) E.Zero) `shouldBe` Success (NumVal (Bounded lim 1))

    it "should fail when applying something other than a function" $
      evalInterval lim M.empty (E.App E.Zero E.Zero) `shouldBe` Error "Expected a closure"

    --it "will recurse infinitely when using Y" $
    --  evalInterval M.empty (E.Y (E.Succ E.Zero)) `shouldBe` Success (NumVal (IV (1, 1)))

    it "should correctly give the interval of zero" $
       evalInterval lim M.empty E.Zero `shouldBe` Success (NumVal (Bounded lim 0))

    it "should correctly give the interval of succ of 0" $
      evalInterval lim M.empty (E.Succ E.Zero) `shouldBe` Success (NumVal (Bounded lim 1))

    it "should correctly give the interval of succ of succ of 0" $
      evalInterval lim M.empty (E.Succ (E.Succ E.Zero)) `shouldBe` Success (NumVal (Bounded lim 2))

    it "should correctly give the interval of pred of 0" $
      evalInterval lim M.empty (E.Pred E.Zero) `shouldBe` Success (NumVal (Bounded lim (-1)))

    it "should correctly give the interval of pred of pred of 0" $
      evalInterval lim M.empty (E.Pred (E.Pred E.Zero)) `shouldBe` Success (NumVal (Bounded lim (-2)))

    it "should correctly give the interval of pred of succ of 0" $
      evalInterval lim M.empty (E.Pred (E.Succ E.Zero)) `shouldBe` Success (NumVal (Bounded lim 0))

    it "should correctly give the interval of succ of pred of 0" $
      evalInterval lim M.empty (E.Succ (E.Pred E.Zero)) `shouldBe` Success (NumVal (Bounded lim 0))

    it "should fail when using succ on something other than a number" $
      let bogus = E.Lam (T.singleton 'x') E.Zero
      in evalInterval lim M.empty (E.Succ bogus) `shouldBe` Error "Expected a number as argument for 'succ'"

    it "should fail when using pred on something other than a number" $
      let bogus = E.Lam (T.singleton 'x') E.Zero
      in evalInterval lim M.empty (E.Pred bogus) `shouldBe` Error "Expected a number as argument for 'pred'"

    it "should execute the then branch on IfZero on zero" $
      evalInterval lim M.empty (E.IfZero E.Zero (E.Succ E.Zero) E.Zero)
        `shouldBe` Success (NumVal (Bounded lim 1))

    it "should execute the else branch on IfZero on completely non-zero" $
      evalInterval lim M.empty (E.IfZero (E.Succ E.Zero) (E.Succ E.Zero) E.Zero)
        `shouldBe` Success (NumVal (Bounded lim 0))

    -- TODO: it seems that the current implementation cannot ever get here.
    it "should execute both branches on IfZero on interval containing zero" $
    --  evalInterval M.empty (E.IfZero (E.Succ E.Zero) (E.Succ E.Zero) E.Zero)
    --    `shouldBe` Success (NumVal (IV (0, 0)))
        pendingWith "it seems that the current implementation cannot ever get here"

    it "should fail when using a non-number condition for IfZero" $
      let bogus = E.Lam (T.singleton 'x') E.Zero
      in evalInterval lim M.empty (E.IfZero bogus E.Zero E.Zero) `shouldBe` Error "Expected a number as condition for 'ifZero'"

    where
      lim = I.Interval (-100) 100
