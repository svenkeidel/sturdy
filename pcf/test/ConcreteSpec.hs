module ConcreteSpec where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified PCF as E
import           Concrete

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "evalConcrete" $ do
    it "should look up a bound variable" $
      let x = T.singleton 'x' in
        evalConcrete (M.insert x (Num 3) M.empty) (E.Var x) `shouldBe` Just (Num 3)

    it "should fail when looking up an unbound variable" $
      let x = T.singleton 'x' in
        evalConcrete M.empty (E.Var x) `shouldBe` Nothing

    it "should create a closure of a FunT" $
      let x = T.singleton 'x'
          cls = E.Lam x (E.FunT E.NumT E.NumT) (E.Succ (E.Var x)) in
        evalConcrete M.empty cls
          `shouldBe` Just (Closure cls M.empty)

    it "should create a closure of a NumT" $
      let x = T.singleton 'x'
          cls = E.Lam x E.NumT (E.Var x) in
        evalConcrete M.empty cls
          `shouldBe` Just (Closure cls M.empty)

    it "should apply a function" $
      let x = T.singleton 'x'
          cls = E.Lam x (E.FunT E.NumT E.NumT) (E.Succ (E.Var x)) in
        evalConcrete M.empty (E.App cls E.Zero)
          `shouldBe` Just (Num 1)

    it "should fail when applying something other than a function" $
      evalConcrete M.empty (E.App E.Zero E.Zero) `shouldBe` Nothing

    --it "will recurse infinitely when using Y" $
    --  evalConcrete M.empty (E.Y (E.Succ E.Zero)) `shouldBe` Just (Num 1)

    it "should give zero" $
      evalConcrete M.empty E.Zero `shouldBe` Just (Num 0)

    it "should give succ of zero" $
      evalConcrete M.empty (E.Succ E.Zero) `shouldBe` Just (Num 1)

    it "should give succ of succ of zero" $
      evalConcrete M.empty (E.Succ (E.Succ E.Zero)) `shouldBe` Just (Num 2)

    it "should give pred of zero" $
      evalConcrete M.empty (E.Pred E.Zero) `shouldBe` Just (Num (-1))

    it "should give pred of pred of zero" $
      evalConcrete M.empty (E.Pred (E.Pred E.Zero)) `shouldBe` Just (Num (-2))

    it "should give pred of succ of 0" $
      evalConcrete M.empty (E.Pred (E.Succ E.Zero)) `shouldBe` Just (Num 0)

    it "should give succ of pred of 0" $
      evalConcrete M.empty (E.Succ (E.Pred E.Zero)) `shouldBe` Just (Num 0)

    it "should execute the then branch on IfZero on zero" $
      evalConcrete M.empty (E.IfZero E.Zero (E.Succ E.Zero) E.Zero)
        `shouldBe` Just (Num 1)

    it "should execute the else branch on IfZero on non-zero" $
      evalConcrete M.empty (E.IfZero (E.Succ E.Zero) (E.Succ E.Zero) E.Zero)
        `shouldBe` Just (Num 0)

    -- TODO: fibonacci
