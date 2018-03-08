{-# LANGUAGE OverloadedStrings #-}
module IntervalAnalysisSpec where

import           SharedSpecs

import           Data.Bounded
import           Data.Error
import qualified Data.Interval as I
import           Data.InfiniteNumbers
import           IntervalAnalysis
import           PCF
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let lim = I.Interval (-100) 100
      bounded = Bounded lim
    in sharedSpec (evalInterval 3 lim) (NumVal . bounded . fromIntegral)

  describe "behavior specific to interval analysis" $ do
    it "should execute both branches on IfZero on interval containing zero" $
      let lim = I.Interval (-100) 100
          bounded i j = NumVal (Bounded lim (I.Interval i j))
          intervalWithZero = bounded (-5) 5
      in evalInterval 3 lim [("x", intervalWithZero)]
          (IfZero "x" (Succ Zero) (Pred Zero))
          `shouldBe` Success (bounded (-1) 1)

    it "should compute 0 + -1 + 1 = 0" $
      let lim = I.Interval (-100) 100
          bounded i j = NumVal (Bounded lim (I.Interval i j))
      in evalInterval 3 lim [] (Succ (Pred Zero)) `shouldBe`
           Success (bounded 0 0)

    it "should analyse addition correctly" $
      -- evalInterval 0 lim [] (App (App add five) two) `shouldBe` Success Top
      let lim = I.Interval 0 5
          bounded i j = NumVal (Bounded lim (I.Interval i j))
      in do
        evalInterval 10 lim [] (App (App add zero) two) `shouldBe` Success (bounded 2 2)
        evalInterval 10 lim [] (App (App add one) two) `shouldBe` Success (bounded 3 3)

        -- The problem is that the check for `IfZero` does not improve the
        -- value for the first parameter of `add` in the environment. This means that
        -- at some point addition is called with add [-1,0] [2,2] and [-1,0] fails the bound check and the top interval is returned.
        evalInterval 10 lim [("x", bounded 0 1)] (App (App add "x") two) `shouldBe` Success (bounded NegInfinity Infinity)

    -- it "should analyze the factorial function correctly" $
    --   evalInterval 3 lim [] (App fix (Lam "fac" (Lam "x" (IfZero "x" one (App (App mult "x") (App "fact" (Pred "x"))))))) `shouldBe`
    --     Success (NumVal (Bounded lim 0))



  where
    -- mult = App fix $ Lam "mult" $ Lam "x" $ Lam "y" $
    --   IfZero "x" Zero (App (App add "y") (App (App "mult" (Pred "x")) "y"))

    add = Y $ Lam "add" $ Lam "x" $ Lam "y" $
      IfZero "x" "y" (Succ (App (App "add" (Pred "x")) "y"))

    zero = Zero
    one = Succ zero
    two = Succ one
    -- three = Succ two
    -- four = Succ three
    -- five = Succ four
    -- six = Succ five
    -- seven = Succ six
