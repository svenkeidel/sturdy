module IntervalAnalysisSpec where

import           Data.Error
import qualified Data.HashMap.Lazy as M
import           Data.Interval
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
      let k = T.singleton 'x'
          v = NumVal (IV (2, 5))
      in evalInterval (M.singleton k v) (E.Var k) `shouldBe` Success v

    it "should fail when looking up an unbound variable" $
      let k = T.singleton 'x'
      in evalInterval M.empty (E.Var k) `shouldBe` Error "Variable \"x\" not bound"

    it "should create a closure of a FunT" $
      let k = T.singleton 'x'
          v = E.Lam k (E.FunT E.NumT E.NumT) (E.Succ (E.Var k))
      in evalInterval M.empty v `shouldBe` Success (ClosureVal (P.singleton (Closure v M.empty)))

    it "should create a closure of a NumT" $
      let k = T.singleton 'x'
          v = E.Lam k E.NumT (E.Var k)
      in evalInterval M.empty v `shouldBe` Success (ClosureVal (P.singleton (Closure v M.empty)))

    it "should apply a function" $
      let k = T.singleton 'x'
          v = E.Lam k (E.FunT E.NumT E.NumT) (E.Succ (E.Var k))
      in evalInterval M.empty (E.App v E.Zero) `shouldBe` Success (NumVal (IV (1, 1)))

    it "should fail when applying something other than a function" $
      evalInterval M.empty (E.App E.Zero E.Zero) `shouldBe` Error "Expected a closure"

    --it "will recurse infinitely when using Y" $
    --  evalInterval M.empty (E.Y (E.Succ E.Zero)) `shouldBe` Success (NumVal (IV (1, 1)))

    it "should correctly give the interval of zero" $
       evalInterval M.empty E.Zero `shouldBe` Success (NumVal (IV (0, 0)))

    it "should correctly give the interval of succ of 0" $
      evalInterval M.empty (E.Succ E.Zero) `shouldBe` Success (NumVal (IV (1, 1)))

    it "should correctly give the interval of succ of succ of 0" $
      evalInterval M.empty (E.Succ (E.Succ E.Zero)) `shouldBe` Success (NumVal (IV (2, 2)))

    it "should correctly give the interval of pred of 0" $
      evalInterval M.empty (E.Pred E.Zero) `shouldBe` Success (NumVal (IV ((-1), (-1))))

    it "should correctly give the interval of pred of pred of 0" $
      evalInterval M.empty (E.Pred (E.Pred E.Zero)) `shouldBe` Success (NumVal (IV ((-2), (-2))))

    it "should correctly give the interval of pred of succ of 0" $
      evalInterval M.empty (E.Pred (E.Succ E.Zero)) `shouldBe` Success (NumVal (IV (0, 0)))

    it "should correctly give the interval of succ of pred of 0" $
      evalInterval M.empty (E.Succ (E.Pred E.Zero)) `shouldBe` Success (NumVal (IV (0, 0)))

    it "should fail when using succ on something other than a number" $
      let bogus = E.Lam (T.singleton 'x') E.NumT E.Zero
      in evalInterval M.empty (E.Succ bogus) `shouldBe` Error "Expected a number as argument for 'succ'"

    it "should fail when using pred on something other than a number" $
      let bogus = E.Lam (T.singleton 'x') E.NumT E.Zero
      in evalInterval M.empty (E.Pred bogus) `shouldBe` Error "Expected a number as argument for 'pred'"

    it "should execute the then branch on IfZero on zero" $
      evalInterval M.empty (E.IfZero E.Zero (E.Succ E.Zero) E.Zero)
        `shouldBe` Success (NumVal (IV (1, 1)))

    it "should execute the else branch on IfZero on completely non-zero" $
      evalInterval M.empty (E.IfZero (E.Succ E.Zero) (E.Succ E.Zero) E.Zero)
        `shouldBe` Success (NumVal (IV (0, 0)))

    -- TODO: it seems that the current implementation cannot ever get here.
    it "should execute both branches on IfZero on interval containing zero" $
    --  evalInterval M.empty (E.IfZero (E.Succ E.Zero) (E.Succ E.Zero) E.Zero)
    --    `shouldBe` Success (NumVal (IV (0, 0)))
        pendingWith "it seems that the current implementation cannot ever get here"

    it "should fail when using a non-number condition for IfZero" $
      let bogus = E.Lam (T.singleton 'x') E.NumT E.Zero
      in evalInterval M.empty (E.IfZero bogus E.Zero E.Zero) `shouldBe` Error "Expected a number as condition for 'ifZero'"
