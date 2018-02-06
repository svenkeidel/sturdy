{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
module FixpointCacheSpec where

import           Prelude hiding (lookup)
import           Data.Function

import           Control.Arrow
import           Control.Arrow.Fix

import           Data.Interval (Interval)
import           Data.Sign (Sign)
import           Data.Order

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "fib" $ do
    runCacheArrow (fixA fib) 10 `shouldBe` fix fib 10
    runCacheArrow (fixA fib) 15 `shouldBe` fix fib 15

  describe "the analyis of a diverging program" $
    it "should terminate with bottom" $ do
      runCacheArrow (fixA diverge) 5 `shouldBe` bottom

  where
    fib :: ArrowChoice c => c Int (Interval Int) -> c Int (Interval Int)
    fib f = proc n -> case n of
      0 -> returnA -< 0
      1 -> returnA -< 1
      _ -> do
        x <- f -< n-1
        y <- f -< n-2
        returnA -< x + y

    diverge :: ArrowChoice c => c Int Sign -> c Int Sign
    diverge f = proc n -> case n of
      0 -> f -< 0
      _ -> f -< (n-1)


