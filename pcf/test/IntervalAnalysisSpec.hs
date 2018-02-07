{-# LANGUAGE OverloadedStrings #-}
module IntervalAnalysisSpec where

import           SharedSpecs

import           Data.Bounded
import           Data.Error
import qualified Data.HashMap.Lazy as M
import qualified Data.Interval as I
import           IntervalAnalysis
import           PCF
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  sharedSpec (evalInterval lim) (NumVal . Bounded lim . fromIntegral)
  describe "behavior specific to interval analysis" $ do
    it "should execute both branches on IfZero on interval containing zero" $
      let intervalWithZero = NumVal (Bounded lim (I.Interval (-5) 5))
      in evalInterval lim (M.fromList [("x", intervalWithZero)])
          (IfZero "x" (Succ E.Zero) (E.Pred E.Zero))
          `shouldBe` Success (NumVal (Bounded lim (I.Interval (-1) 1)))

    it "should compute 0 + -1 + 1 = 0" $
      evalInterval lim M.empty (E.Succ (E.Pred E.Zero)) `shouldBe`
        Success (NumVal (Bounded lim 0))

  where
    lim = I.Interval (-100) 100
