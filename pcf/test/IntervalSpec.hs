module IntervalSpec where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified PCF as E
import           Interval
import           Shared

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "evalInterval" $ do
    it "should look up a bound variable" $
      let x = T.singleton 'x' in
        evalInterval (M.insert x (Num (IV (2, 5))) M.empty) (E.Var x) `shouldBe` Just (Num (IV (2, 5)))

    it "should fail when looking up an unbound variable" $
      let x = T.singleton 'x' in
        evalInterval M.empty (E.Var x) `shouldBe` Nothing

    it "should create a closure of a FunT" $
      let x = T.singleton 'x'
          cls = E.Lam x (E.FunT E.NumT E.NumT) (E.Succ (E.Var x)) in
        evalInterval M.empty cls
          `shouldBe` Just (Closure cls M.empty)

    it "should create a closure of a NumT" $
      let x = T.singleton 'x'
          cls = E.Lam x E.NumT (E.Var x) in
        evalInterval M.empty cls
          `shouldBe` Just (Closure cls M.empty)

    it "should apply a function" $
      let x = T.singleton 'x'
          cls = E.Lam x (E.FunT E.NumT E.NumT) (E.Succ (E.Var x)) in
        evalInterval M.empty (E.App cls E.Zero)
          `shouldBe` Just (Num (IV (1, 1)))

    it "should fail when applying something other than a function" $
      evalInterval M.empty (E.App E.Zero E.Zero) `shouldBe` Nothing

    --it "will recurse infinitely when using Y" $
    --  evalInterval M.empty (E.Y (E.Succ E.Zero)) `shouldBe` Just (Num (IV (1, 1)))

    it "should correctly give the interval of zero" $
       evalInterval M.empty E.Zero `shouldBe` Just (Num (IV (0, 0)))

    it "should correctly give the interval of succ of 0" $
      evalInterval M.empty (E.Succ E.Zero)
        `shouldBe`
          Just (Num (IV (1, 1)))

    it "should correctly give the interval of succ of succ of 0" $
      evalInterval M.empty (E.Succ (E.Succ E.Zero))
        `shouldBe`
          Just (Num (IV (2, 2)))

    it "should correctly give the interval of pred of 0" $
      evalInterval M.empty (E.Pred E.Zero)
        `shouldBe`
          Just (Num (IV ((-1), (-1))))

    it "should correctly give the interval of pred of pred of 0" $
      evalInterval M.empty (E.Pred (E.Pred E.Zero))
        `shouldBe`
          Just (Num (IV ((-2), (-2))))

    it "should correctly give the interval of pred of succ of 0" $
      evalInterval M.empty (E.Pred (E.Succ E.Zero))
        `shouldBe`
          Just (Num (IV (0, 0)))

    it "should correctly give the interval of succ of pred of 0" $
      evalInterval M.empty (E.Succ (E.Pred E.Zero))
        `shouldBe`
          Just (Num (IV (0, 0)))

    it "should execute the then branch on IfZero on zero" $
      evalInterval M.empty (E.IfZero E.Zero (E.Succ E.Zero) E.Zero)
        `shouldBe` Just (Num (IV (1, 1)))

    it "should execute the else branch on IfZero on completely non-zero" $
      evalInterval M.empty (E.IfZero (E.Succ E.Zero) (E.Succ E.Zero) E.Zero)
        `shouldBe` Just (Num (IV (0, 0)))

    -- TODO: it seems that the current implementation cannot ever get here.
    it "should execute both branches on IfZero on interval containing zero" $
    --evalInterval M.empty (E.IfZero (E.Succ E.Zero) (E.Succ E.Zero) E.Zero)
    --  `shouldBe` Just (Num (IV (0, 0)))
        pendingWith "it seems that the current implementation cannot ever get here"

    -- TOOD: fibonacci
