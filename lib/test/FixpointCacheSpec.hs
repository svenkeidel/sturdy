{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module FixpointCacheSpec where

import           Prelude hiding (lookup)

import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Fail
import           Control.Arrow.State

import           Data.Interval (Interval)
import qualified Data.Interval as I
import           Data.Sign (Sign)
import qualified Data.Sign as S
import           Data.Order
import           Data.Error
import           Data.InfiniteNumbers
import qualified Data.Store as S
import           Data.Boolean(AbsBool,Logic(..))
import           Data.Hashable
import           GHC.Generics

import           Test.Hspec

main :: IO ()
main = hspec spec

type Cache x y = CacheArrow x y x y
type ErrorCache x y = ErrorArrow () (CacheArrow x (Error () y)) x y
type StateCache x y = StateArrow (InfiniteNumber Int) (CacheArrow (InfiniteNumber Int,x) (InfiniteNumber Int,y)) x y
type IV = Interval (InfiniteNumber Int)

spec :: Spec
spec = do
  describe "the analysis of the fibonacci numbers" $
    let fib f =
          ifLowerThan 0
            (proc _ -> returnA -< 0)
            (ifLowerThan 1 (proc _ -> returnA -< 1)
                           (proc n -> do
                              x <- f -< n-1
                              y <- f -< n-2
                              returnA -< x + y))

    in it "should memoize numbers that have been computed before already" $ do
         runCacheArrow (fixA fib :: Cache IV IV) (I.Interval 5 10) `shouldBe` I.Interval 5 55
         -- runCacheArrow (fixA fib :: Cache (Interval Int) (Interval Int)) 15 `shouldBe` fix fib 15

  describe "the analysis of the factorial function" $
    let fact f = proc n -> do
          ifLowerThan 1 (proc _ -> returnA -< 1)
                        (proc n -> do {x <- f -< (n-1); returnA -< n * x}) -< n
    in it "fact [-∞,∞] should produce [1,∞]" $
         runCacheArrow (fixA fact :: Cache IV IV) (I.Interval NegInfinity Infinity) `shouldBe` I.Interval 1 Infinity

  describe "the analysis of foo" $
    let foo :: Cache Sign Sign -> Cache Sign Sign
        foo f = proc s -> case s of
          S.Positive -> f -< S.Negative
          S.Negative -> do
            x <- f -< S.Positive
            returnA -< S.Negative ⊔ x
          _ -> error "undefined" -< ()

    in it "foo positive should produce top" $ do
         pendingWith "Our optimization is unsound"
         runCacheArrow (fixA foo :: Cache Sign Sign) S.Positive `shouldBe` S.Top

  describe "the even and odd functions" $
    let evenOdd :: Cache (EvenOdd,IV) AbsBool -> Cache (EvenOdd,IV) AbsBool
        evenOdd f = proc (e,x) -> case e of
          Even -> ifLowerThan 0 (proc _ -> returnA -< true)
                                (ifLowerThan 1 (proc _ -> returnA -< false)
                                               (proc x -> f -< (Odd,x-1))) -< x
          Odd -> ifLowerThan 0 (proc _ -> returnA -< false)
                                (ifLowerThan 1 (proc _ -> returnA -< true)
                                               (proc x -> f -< (Even,x-1))) -< x
    in it "even([-∞,∞]) should produce top" $
         runCacheArrow (fixA evenOdd) (Even,I.Interval 0 Infinity) `shouldBe` top

  describe "the ackermann function" $
    let ackermann :: Cache (IV,IV) IV -> Cache (IV,IV) IV
        ackermann f = proc (m,n) ->
           ifLowerThan 0 (proc _ -> returnA -< n+1)
                         (proc m' -> ifLowerThan 0 (proc _ -> f -< (m'-1,1))
                                                   (proc n' -> do { x <- f -< (m,n'-1); f -< (m'-1,x)}) -<< n) -<< m
    in it "ackerman ([0,∞], [0,∞]) should be [0,∞] " $
         runCacheArrow (fixA ackermann) (I.Interval 0 Infinity,I.Interval 0 Infinity)
           `shouldBe` I.Interval 1 Infinity

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
            `shouldBe` (S.fromList [(n,Error ()) | n <- [0..5]], Error ())

  describe "the analysis of a stateful program" $
    let timesTwo :: StateCache (InfiniteNumber Int) () -> StateCache (InfiniteNumber Int) ()
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
           `shouldBe` (S.fromList [((Number n,5- Number n),(10-Number n,())) | n <- [0..5]],(10,()))
  where

    ifLowerThan :: (Num n, Ord n, LowerBounded x, ArrowChoice c, Complete (c (Interval n,Interval n) x)) => n -> c (Interval n) x -> c (Interval n) x -> c (Interval n) x
    ifLowerThan l f g = proc b -> case b of
      I.Bot -> returnA -< bottom
      I.Interval m n
        | n <= l -> f -< b
        | l < m -> g -< b
        | otherwise -> joined f g -< (toBot $ I.Interval m l, toBot $ I.Interval (l+1) n)

    toBot :: Ord n => Interval n -> Interval n
    toBot I.Bot = I.Bot
    toBot x@(I.Interval m n)
      | n < m = I.Bot
      | otherwise = x

data EvenOdd = Even | Odd deriving (Eq,Generic,Show)
instance Hashable EvenOdd
