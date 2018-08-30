{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
module IntervalAnalysisSpec where

import           Prelude hiding (succ,pred)
import           SharedSpecs

import           Data.Abstract.Bounded
import           Data.Abstract.PropagateError hiding (toEither)
import qualified Data.Abstract.Interval as I
import           Data.Abstract.InfiniteNumbers
import           Data.Abstract.Terminating hiding (toEither)
import           IntervalAnalysis
import           Syntax
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let ?bound = I.Interval (-100) 100 in
        sharedSpec (\env e -> toEither $ evalInterval 3 env e) (NumVal . bounded . fromIntegral)

  describe "behavior specific to interval analysis" $ do
    it "should execute both branches on IfZero on interval containing zero" $
      let ?bound = I.Interval (-100) 100
      in evalInterval 3 [("x", num (-5) 5)]
          (ifZero "x" (succ zero) (pred zero))
          `shouldBe` Terminating (Success (num (-1) 1))

    it "should compute 0 + -1 + 1 = 0" $
      let ?bound = I.Interval (-100) 100
      in evalInterval 3 [] (succ (pred zero)) `shouldBe`
           Terminating (Success (num 0 0))

    it "should analyse addition correctly" $
      -- evalInterval 0 lim [] (App (App add five) two) `shouldBe` Success Top
      let ?bound = I.Interval 0 5
      in do
        evalInterval 10 [] (app (app add zero) two) `shouldBe` Terminating (Success (num 2 2))
        evalInterval 10 [] (app (app add one) two) `shouldBe` Terminating (Success (num 3 3))

        -- The problem is that the check for `IfZero` does not improve the
        -- value for the first parameter of `add` in the environment. This means that
        -- at some point addition is called with add [-1,0] [2,2] and [-1,0] fails the bound check and the top interval is returned.
        evalInterval 10 [("x", num 0 1)] (app (app add "x") two) `shouldBe`
          Terminating (Success (num NegInfinity Infinity))

    it "should terminate for the non-terminating program" $
      let ?bound = I.Interval 0 5
      in do evalInterval 5 [] (fix (lam "x" "x")) `shouldSatisfy`
              \c -> case c of Terminating (Success (ClosureVal _)) -> True; _ -> False
            evalInterval 5 [] (fix (lam "f" (lam "x" (app "f" "x")))) `shouldSatisfy`
              \c -> case c of Terminating (Success (ClosureVal _)) -> True; _ -> False

  where
    add = fix $ lam "add" $ lam "x" $ lam "y" $ ifZero "x" "y" (succ (app (app "add" (pred "x")) "y"))

    one = succ zero
    two = succ one

    num i j = NumVal $ bounded $ I.Interval i j

    toEither :: Terminating (Error String a) -> Either String a
    toEither (Terminating (Fail e)) = Left e
    toEither (Terminating (Success x)) = Right x
    toEither NonTerminating = Left "NonTerminating"
