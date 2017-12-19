module SignSpec where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified PCF as E
import           Sign
import           Shared

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "evalSign" $ do
    it "should look up a bound variable" $
      let x = T.singleton 'x' in
        evalSign (M.insert x (Num Positive) M.empty) (E.Var x) `shouldBe` Just (Num Positive)

    it "should fail when looking up an unbound variable" $
      let x = T.singleton 'x' in
        evalSign M.empty (E.Var x) `shouldBe` Nothing

    it "should create a closure of a FunT" $
      let x = T.singleton 'x'
          cls = E.Lam x (E.FunT E.NumT E.NumT) (E.Succ (E.Var x)) in
        evalSign M.empty cls
          `shouldBe` Just (Closure cls M.empty)

    it "should create a closure of a NumT" $
      let x = T.singleton 'x'
          cls = E.Lam x E.NumT (E.Var x) in
        evalSign M.empty cls
          `shouldBe` Just (Closure cls M.empty)

    it "should apply a function" $
      let x = T.singleton 'x'
          cls = E.Lam x (E.FunT E.NumT E.NumT) (E.Succ (E.Var x)) in
        evalSign M.empty (E.App cls E.Zero)
          `shouldBe` Just (Num Positive)

    it "should fail when applying something other than a function" $
      evalSign M.empty (E.App E.Zero E.Zero) `shouldBe` Nothing

    --it "will recurse infinitely when using Y" $
    --  evalSign M.empty (E.Y (E.Succ E.Zero)) `shouldBe` Just (Num 1)

    it "should correctly give the sign of zero" $
       evalSign M.empty E.Zero `shouldBe` Just (Num Zero)

    it "should correctly give the sign of succ of 0" $
      evalSign M.empty (E.Succ E.Zero)
        `shouldBe`
          Just (Num Positive)

    it "should correctly give the sign of succ of succ of 0" $
      evalSign M.empty (E.Succ (E.Succ E.Zero))
        `shouldBe`
          Just (Num Positive)

    it "should correctly give the sign of pred of 0" $
      evalSign M.empty (E.Pred E.Zero)
        `shouldBe`
          Just (Num Negative)

    it "should correctly give the sign of pred of pred of 0" $
      evalSign M.empty (E.Pred (E.Pred E.Zero))
        `shouldBe`
          Just (Num Negative)

    it "should correctly overapproximate the sign of pred of succ of 0" $
      evalSign M.empty (E.Pred (E.Succ E.Zero))
        `shouldBe`
          Just (Num Top)

    it "should correctly overapproximate the sign of succ of pred of 0" $
      evalSign M.empty (E.Succ (E.Pred E.Zero))
        `shouldBe`
          Just (Num Top)

    it "should execute the then branch on IfZero on zero" $
      evalSign M.empty (E.IfZero E.Zero (E.Succ E.Zero) E.Zero)
        `shouldBe` Just (Num Positive)

    it "should execute the else branch on IfZero on non-zero" $
      evalSign M.empty (E.IfZero (E.Succ E.Zero) (E.Succ E.Zero) E.Zero)
        `shouldBe` Just (Num Zero)

    -- TODO: fibonacci
