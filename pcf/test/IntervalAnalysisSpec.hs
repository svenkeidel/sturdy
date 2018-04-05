{-# LANGUAGE OverloadedStrings #-}
module IntervalAnalysisSpec where

import           Prelude hiding (succ,pred)
import           SharedSpecs

import           Data.Abstract.Bounded
import           Data.Abstract.Error
import qualified Data.Abstract.Interval as I
import           Data.Abstract.InfiniteNumbers
import           Data.Abstract.Terminating
import           IntervalAnalysis
import           PCF
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let lim = I.Interval (-100) 100
      bounded = Bounded lim
    in sharedSpec (\env e -> toEither $ evalInterval 3 lim env e) (NumVal . bounded . fromIntegral)

  describe "behavior specific to interval analysis" $ do
    it "should execute both branches on IfZero on interval containing zero" $
      let lim = I.Interval (-100) 100
          bounded i j = NumVal (Bounded lim (I.Interval i j))
          intervalWithZero = bounded (-5) 5
      in evalInterval 3 lim [("x", intervalWithZero)]
          (ifZero "x" (succ zero) (pred zero))
          `shouldBe` Terminating (Success (bounded (-1) 1))

    it "should compute 0 + -1 + 1 = 0" $
      let lim = I.Interval (-100) 100
          bounded i j = NumVal (Bounded lim (I.Interval i j))
      in evalInterval 3 lim [] (succ (pred zero)) `shouldBe`
           Terminating (Success (bounded 0 0))

    it "should analyse addition correctly" $
      -- evalInterval 0 lim [] (App (App add five) two) `shouldBe` Success Top
      let lim = I.Interval 0 5
          bounded i j = NumVal (Bounded lim (I.Interval i j))
      in do
        evalInterval 10 lim [] (app (app add zero) two) `shouldBe` Terminating (Success (bounded 2 2))
        evalInterval 10 lim [] (app (app add one) two) `shouldBe` Terminating (Success (bounded 3 3))

        -- The problem is that the check for `IfZero` does not improve the
        -- value for the first parameter of `add` in the environment. This means that
        -- at some point addition is called with add [-1,0] [2,2] and [-1,0] fails the bound check and the top interval is returned.
        evalInterval 10 lim [("x", bounded 0 1)] (app (app add "x") two) `shouldBe`
          Terminating (Success (bounded NegInfinity Infinity))

    it "should terminate for the non terminating program" $
      let lim = I.Interval 0 5
      in do evalInterval 5 lim [] (fix (lam "x" "x")) `shouldSatisfy`
              \c -> case c of Terminating (Success (ClosureVal _)) -> True; _ -> False
            evalInterval 5 lim [] (fix (lam "f" (lam "x" (app "f" "x")))) `shouldSatisfy`
              \c -> case c of Terminating (Success (ClosureVal _)) -> True; _ -> False

  where
    add = fix $ lam "add" $ lam "x" $ lam "y" $ ifZero "x" "y" (succ (app (app "add" (pred "x")) "y"))

    one = succ zero
    two = succ one

    toEither :: Terminating (Error String a) -> Either String a
    toEither (Terminating (Fail e)) = Left e
    toEither (Terminating (Success x)) = Right x
    toEither NonTerminating = Left "NonTerminating"
