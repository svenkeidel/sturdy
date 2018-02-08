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
import qualified Data.Interval as I
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
    let fib f =
          ifLowerThan 0
            (proc _ -> returnA -< 0)
            (ifEq 1 (proc _ -> returnA -< 1)
                    (proc n -> do
                      x <- f -< n-1
                      y <- f -< n-2
                      returnA -< x + y))

    in it "should memoize numbers that have been computed before already" $ do
         runCacheArrow (fixA fib :: Cache (Interval Int) (Interval Int)) (I.Interval 5 10) `shouldBe` I.Interval 5 55
         -- runCacheArrow (fixA fib :: Cache (Interval Int) (Interval Int)) 15 `shouldBe` fix fib 15

  describe "the analysis of the factorial function" $
    let fact :: (ArrowChoice c, Complete (c (Interval Int,Interval Int) (Interval Int))) => c (Interval Int) (Interval Int) -> c (Interval Int) (Interval Int)
        fact f = proc n -> do
          ifLowerThan 0 (proc _ -> returnA -< 1)
                        (proc n -> do {x <- f -< (n-1); returnA -< n * x}) -< n
    in it "absfact [5,10] should produce [fact 5,fact 10]" $ runCacheArrow (fixA fact :: Cache (Interval Int) (Interval Int)) (I.Interval 5 10) `shouldBe` I.Interval 120 3628800

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
  where

    ifEq :: (ArrowChoice c, Complete (c (Interval Int, Interval Int) (Interval Int)), Complete (c (Interval Int,(Interval Int, Interval Int)) (Interval Int))) => Int -> c (Interval Int) (Interval Int) -> c (Interval Int) (Interval Int) -> c (Interval Int) (Interval Int)
    ifEq l f g = proc b -> case b of
      I.Bot -> returnA -< I.Bot
      I.Interval m n
        | m == l && n == l -> f -< b
        | n < l || l < m -> g -< b
        | otherwise -> joined f (joined g g) -< (I.Interval l l,(toBot $ I.Interval m (l-1), toBot $ I.Interval (l+1) n))
      where
        toBot :: Interval Int -> Interval Int
        toBot I.Bot = bottom
        toBot x@(I.Interval m n)
          | n < m = bottom
          | otherwise = x
      

    ifLowerThan :: (ArrowChoice c, Complete (c (Interval Int,Interval Int) (Interval Int))) => Int -> c (Interval Int) (Interval Int) -> c (Interval Int) (Interval Int) -> c (Interval Int) (Interval Int)
    ifLowerThan l f g = proc b -> case b of
      I.Bot -> returnA -< I.Bot
      I.Interval m n
        | n <= l -> f -< b
        | l < m -> g -< b
        | otherwise -> joined f g -< (I.Interval m l, I.Interval (l+1) n)
