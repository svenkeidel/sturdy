{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module FixpointCacheSpec where

import           Prelude hiding (lookup,Bounded)

import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Fail
import           Control.Arrow.State

import           Data.Boolean(AbsBool,Logic(..))
import           Data.Bounded
import           Data.Error
import           Data.Hashable
import           Data.InfiniteNumbers
import           Data.Interval (Interval)
import qualified Data.Interval as I
import           Data.Order
import           Data.Sign (Sign)
import qualified Data.Sign as S
import qualified Data.Store as S
import           GHC.Generics

import           Test.Hspec

main :: IO ()
main = hspec spec

type Cache x y = CacheArrow x y x y
type ErrorCache x y = ErrorArrow () (CacheArrow x (Error () y)) x y
type StateCache x y = StateArrow IV (CacheArrow (IV,x) (IV,y)) x y
type IV = Bounded (Interval (InfiniteNumber Int))

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
        bounded = Bounded (I.Interval 0 100)

    in it "should memoize numbers that have been computed before already" $ do
         runCacheArrow (fixA fib :: Cache IV IV) (bounded (I.Interval 5 10)) `shouldBe` (bounded (I.Interval 5 55))
         runCacheArrow (fixA fib :: Cache IV IV) (bounded (I.Interval 0 Infinity)) `shouldBe` (bounded top)

  describe "the analysis of the factorial function" $
    let fact f = proc n -> do
          ifLowerThan 1 (proc _ -> returnA -< 1)
                        (proc n -> do {x <- f -< (n-1); returnA -< n * x}) -< n
        bound = I.Interval NegInfinity Infinity
    in it "fact [-inf,inf] should produce [1,inf]" $

         runCacheArrow (fixA fact :: Cache IV IV) (Bounded bound (I.Interval NegInfinity Infinity))
           `shouldBe` (Bounded bound (I.Interval 1 Infinity))

  describe "the analysis of foo" $
    let foo :: Cache Sign Sign -> Cache Sign Sign
        foo f = proc s -> case s of
          S.Positive -> f -< S.Negative
          S.Negative -> do
            x <- f -< S.Positive
            returnA -< S.Negative ⊔ g x
          _ -> error "undefined" -< ()
        g S.Bot = S.Bot
        g _ = S.Positive

    in it "foo positive should produce top" $ do
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

        bound = I.Interval NegInfinity Infinity
    in it "even([-inf,inf]) should produce top" $
         runCacheArrow (fixA evenOdd) (Even,Bounded bound (I.Interval 0 Infinity)) `shouldBe` top

  describe "the ackermann function" $
    let ackermann :: Cache (IV,IV) IV -> Cache (IV,IV) IV
        ackermann f = proc (m,n) ->
           ifLowerThan 0 (proc _ -> returnA -< n+1)
                         (proc m' -> ifLowerThan 0 (proc _ -> f -< (m'-1,1))
                                                   (proc n' -> do { x <- f -< (m,n'-1); f -< (m'-1,x)}) -<< n) -<< m

        bounded = Bounded (I.Interval (-50) 50)
    in it "ackerman ([0,inf], [0,inf]) should be [0,inf] " $ do
         runCacheArrow (fixA ackermann) (bounded (I.Interval 0 Infinity), bounded (I.Interval 0 Infinity))
           `shouldBe` (bounded top)

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
    let timesTwo :: StateCache IV () -> StateCache IV ()
        timesTwo f = proc n -> case n of
          0 -> returnA -< ()
          _ -> do
            s <- getA -< ()
            putA -< s+ bounded 1
            f -< (n-1)
            s' <- getA -< ()
            putA -< s'+ bounded 1

        bounded = Bounded (I.Interval 0 20) . fromIntegral
    in it "should cache the state of the program" $
         runCacheArrow' (runStateArrow (fixA timesTwo)) (bounded 0,bounded 5) `shouldBe`
           (S.fromList [((bounded n,5-bounded n),(10-bounded n,())) | n <- [0..5::Int]],(10,()))
  where

    ifLowerThan :: (Num n, Ord n, LowerBounded x, ArrowChoice c, Complete (c (Bounded (Interval n), Bounded (Interval n)) x)) => n -> c (Bounded (Interval n)) x -> c (Bounded (Interval n)) x -> c (Bounded (Interval n)) x
    ifLowerThan l f g = proc b@(Bounded o x) -> case x of
      I.Bot -> returnA -< bottom
      I.Interval m n
        | n <= l -> f -< b
        | l < m -> g -< b
        | otherwise -> joined f g -< (toBot $ Bounded o (I.Interval m l), toBot $ Bounded o (I.Interval (l+1) n))

    toBot :: Ord n => Bounded (Interval n) -> Bounded (Interval n)
    toBot (Bounded o I.Bot) = Bounded o I.Bot
    toBot x@(Bounded o (I.Interval m n))
      | n < m = Bounded o I.Bot
      | otherwise = x

data EvenOdd = Even | Odd deriving (Eq,Generic,Show)
instance Hashable EvenOdd
