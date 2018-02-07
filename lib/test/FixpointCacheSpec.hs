{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module FixpointCacheSpec where

import           Prelude hiding (lookup)
import           Data.Function

import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Fail

import           Data.Interval (Interval)
import           Data.Sign (Sign)
import           Data.Order
import           Data.Error
import qualified Data.Store as S

import           Test.Hspec

main :: IO ()
main = hspec spec

type Cache x y = CacheArrow x y (->) x y

type CacheError x y = CacheArrow x y (ErrorArrow () (->)) x y
-- CacheArrow x y (ErrorArrow () (->)) a b
--  = (ErrorArrow () (->)) ((Store x y,Store x y),a) (Store x y,b)
--  = ((Store x y,Store x y),a) -> Error () (Store x y,b)

type ErrorCache x y = ErrorArrow () (CacheArrow x (Error () y) (->)) x y
-- ErrorArrow () (CacheArrow x (Error () y) (->)) a b
--  = (CacheArrow x (Error () y) (->)) a (Error () b)
--  = (CacheArrow x (Error () y) (->)) ((Store x y,Store x y),a) (Store x y,Error () b)

spec :: Spec
spec = do
  it "fib" $ do
    runCacheArrow (fixA fib :: Cache Int (Interval Int)) 10 `shouldBe` fix fib 10
    runCacheArrow (fixA fib :: Cache Int (Interval Int)) 15 `shouldBe` fix fib 15

  describe "the analyis of a diverging program" $
    it "should terminate with bottom" $ do
      runCacheArrow (fixA diverge :: Cache Int Sign) 5 `shouldBe` bottom

  describe "the analysis of a failing program" $
    it "should" $ do
      runErrorArrow (runCacheArrow (fixA recurseFail :: CacheError Int Sign)) 5 `shouldBe` Success 0
      runCacheArrow' (runErrorArrow (fixA recurseFail :: ErrorCache Int Sign)) 5 `shouldBe` (S.fromList [(n,Success 0) | n <- [0..4]], Success 0)

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

    recurseFail :: (ArrowFail () c, Complete (c ((),Sign) Sign), ArrowChoice c) => c Int Sign -> c Int Sign
    recurseFail f = proc n -> case n of
      0 -> joined failA returnA -< ((),0)
      _ -> f -< (n-1)

