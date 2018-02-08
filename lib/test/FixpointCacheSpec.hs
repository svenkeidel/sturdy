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
import           Control.Arrow.State

import           Data.Interval (Interval)
import           Data.Sign (Sign)
import           Data.Order
import           Data.Error
import qualified Data.Store as S

import           Test.Hspec

main :: IO ()
main = hspec spec

type Cache x y = CacheArrow x y x y
type ErrorCache x y = ErrorArrow () (CacheArrow x (Error () y)) x y
type StateCache x y = StateArrow Int (CacheArrow (Int,x) (Int,y)) x y

spec :: Spec
spec = do
  describe "the analysis of the fibonacci numbers" $
    let fib :: ArrowChoice c => c Int (Interval Int) -> c Int (Interval Int)
        fib f = proc n -> case n of
          0 -> returnA -< 0
          1 -> returnA -< 1
          _ -> do
            x <- f -< n-1
            y <- f -< n-2
            returnA -< x + y
    in it "should memoize numbers that have been computed before already" $ do
         runCacheArrow (fixA fib :: Cache Int (Interval Int)) 10 `shouldBe` fix fib 10
         runCacheArrow (fixA fib :: Cache Int (Interval Int)) 15 `shouldBe` fix fib 15

  describe "the analyis of a diverging program" $
    let diverge :: Cache Int Sign -> Cache Int Sign
        diverge f = proc n -> case n of
          0 -> f -< 0
          _ -> f -< (n-1)
    in it "should terminate with bottom" $
         runCacheArrow (fixA diverge) 5
           `shouldBe` bottom

  describe "the analysis of a failing program" $
    let recurseFail :: ErrorCache Int Sign -> ErrorCache Int Sign
        recurseFail f = proc n -> case n of
          0 -> failA -< ()
          _ -> f -< (n-1)
    in it "should fail, but update the fixpoint cache" $
         runCacheArrow' (runErrorArrow (fixA recurseFail)) 5
            `shouldBe` (S.fromList [(n,Error ()) | n <- [0..4]], Error ())

  describe "the analysis of a stateful program" $
    let timesTwo :: StateCache Int () -> StateCache Int ()
        timesTwo f = proc n -> case n of
          0 -> returnA -< ()
          _ -> do
            s <- getA -< ()
            putA -< s+1
            f -< (n-1)
            s' <- getA -< ()
            putA -< s'+1

    in it "should cache the state of the program" $
         runCacheArrow' (runStateArrow (fixA timesTwo)) (0,5)
           `shouldBe` (S.fromList [((n,5-n),(10-n,())) | n <- [1..5]],(10,()))
