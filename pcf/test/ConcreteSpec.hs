{-# LANGUAGE OverloadedStrings #-}
module ConcreteSpec where

import           Concrete
import           Data.Error
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import qualified PCF as E
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "evalConcrete" $ do
    it "should look up a bound variable" $
      evalConcrete (M.singleton "x" (NumVal 3)) "x" `shouldBe` Success (NumVal 3)

    it "should fail when looking up an unbound variable" $
      evalConcrete M.empty "x" `shouldBe` Error "Variable \"x\" not bound"

    it "should create a closure" $
      let v = E.Succ (E.Var "x")
      in evalConcrete M.empty (E.Lam "x" v) `shouldBe` Success (ClosureVal (Closure "x" v M.empty))

    it "should apply a function" $
      let k = T.singleton 'x'
          v = E.Lam k (E.Succ (E.Var k))
      in evalConcrete M.empty (E.App v E.Zero) `shouldBe` Success (NumVal 1)

    it "should fail when applying something other than a function" $
      evalConcrete M.empty (E.App E.Zero E.Zero) `shouldBe` Error "Expected a closure"

    --it "will recurse infinitely when using Y" $
    --  evalConcrete M.empty (E.Y (E.Succ E.Zero)) `shouldBe` Success (NumVal 1)

    it "should give zero" $
      evalConcrete M.empty E.Zero `shouldBe` Success (NumVal 0)

    it "should give succ of zero" $
      evalConcrete M.empty (E.Succ E.Zero) `shouldBe` Success (NumVal 1)

    it "should give succ of succ of zero" $
      evalConcrete M.empty (E.Succ (E.Succ E.Zero)) `shouldBe` Success (NumVal 2)

    it "should give pred of zero" $
      evalConcrete M.empty (E.Pred E.Zero) `shouldBe` Success (NumVal (-1))

    it "should give pred of pred of zero" $
      evalConcrete M.empty (E.Pred (E.Pred E.Zero)) `shouldBe` Success (NumVal (-2))

    it "should give pred of succ of 0" $
      evalConcrete M.empty (E.Pred (E.Succ E.Zero)) `shouldBe` Success (NumVal 0)

    it "should give succ of pred of 0" $
      evalConcrete M.empty (E.Succ (E.Pred E.Zero)) `shouldBe` Success (NumVal 0)

    it "should fail when using succ on something other than a number" $
      let bogus = E.Lam (T.singleton 'x') E.Zero
      in evalConcrete M.empty (E.Succ bogus) `shouldBe` Error "Expected a number as argument for 'succ'"

    it "should fail when using pred on something other than a number" $
      let bogus = E.Lam (T.singleton 'x') E.Zero
      in evalConcrete M.empty (E.Pred bogus) `shouldBe` Error "Expected a number as argument for 'pred'"

    it "should execute the then branch on IfZero on zero" $
      evalConcrete M.empty (E.IfZero E.Zero (E.Succ E.Zero) E.Zero)
        `shouldBe` Success (NumVal 1)

    it "should execute the else branch on IfZero on non-zero" $
      evalConcrete M.empty (E.IfZero (E.Succ E.Zero) (E.Succ E.Zero) E.Zero)
        `shouldBe` Success (NumVal 0)

    it "should fail when using a non-number condition for IfZero" $
      let bogus = E.Lam (T.singleton 'x') E.Zero
      in evalConcrete M.empty (E.IfZero bogus E.Zero E.Zero) `shouldBe` Error "Expected a number as condition for 'ifZero'"
