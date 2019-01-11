{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImplicitParams #-}
module FixSpec where

import           Prelude hiding (lookup,Bounded,Bool(..),fail)

import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Transformer.Abstract.Fixpoint
import           Control.Arrow.Transformer.Abstract.Failure
import           Control.Arrow.Transformer.State
import           Control.Arrow.Fail
import           Control.Arrow.State

import           Data.Boolean(Logic(..))
import           Data.Abstract.Boolean(Bool)
import           Data.Abstract.Failure
import           Data.Abstract.InfiniteNumbers
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import           Data.Abstract.Sign (Sign)
import qualified Data.Abstract.Map as S
import           Data.Abstract.Terminating
import qualified Data.Abstract.Widening as W
import qualified Data.Abstract.StackWidening as SW

import           Data.Order
import           Data.Hashable

import           GHC.Generics

import           Test.Hspec

main :: IO ()
main = hspec spec

type Cache s x y = Fix x y (FixT s () () (->)) x y
type ErrorFix s x y = Fix x y (FailureT () (FixT s () () (->))) x y
type StateFix s x y = Fix x y (StateT IV (FixT s () () (->))) x y
type IV = Interval (InfiniteNumber Int)

spec :: Spec
spec = do
  describe "the analysis of the fibonacci numbers" $
    let fib :: Cache s IV IV
        fib = fix $ \f ->
          ifLowerThan 0
            (proc _ -> returnA -< I.Interval 0 0)
            (ifLowerThan 1 (proc _ -> returnA -< I.Interval 1 1)
                           (proc n -> do
                              x <- f -< n - I.Interval 1 1
                              y <- f -< n - I.Interval 2 2
                              returnA -< x + y))

    in it "should memoize numbers that have been computed before already" $ do
         runFixT' SW.finite W.finite fib (I.Interval 5 10) `shouldBe` return (I.Interval 5 55)
         runFixT' SW.finite I.widening fib (I.Interval 0 Infinity) `shouldBe` return (I.Interval 0 Infinity)

  describe "the analysis of the factorial function" $
    let fact :: Cache s IV IV
        fact = fix $ \f -> proc n -> do
          ifLowerThan 1 (proc _ -> returnA -< I.Interval 1 1)
                        (proc n -> do {x <- f -< (n-I.Interval 1 1); returnA -< n * x}) -< n
    in it "fact [-inf,inf] should produce [1,inf]" $
         runFixT' SW.finite I.widening fact top `shouldBe` return (I.Interval 1 Infinity)

  describe "the even and odd functions" $
    let evenOdd :: Cache s (EvenOdd,IV) Bool
        evenOdd = fix $ \f -> proc (e,x) -> case e of
          Even -> ifLowerThan 0 (proc _ -> returnA -< true)
                                (ifLowerThan 1 (proc _ -> returnA -< false)
                                               (proc x -> f -< (Odd,x-I.Interval 1 1))) -< x
          Odd -> ifLowerThan 0 (proc _ -> returnA -< false)
                                (ifLowerThan 1 (proc _ -> returnA -< true)
                                               (proc x -> f -< (Even,x-I.Interval 1 1))) -< x
    in it "even([-inf,inf]) should produce top" $
         runFixT' SW.finite W.finite evenOdd (Even,I.Interval 0 Infinity) `shouldBe` top

  describe "the ackermann function" $
    let ackermann :: Cache s (IV,IV) IV
        ackermann = fix $ \f -> proc (m,n) ->
          ifLowerThan 0
            (proc _ -> returnA -< n + I.Interval 1 1)
            (proc m' -> ifLowerThan 0
                          (proc _ -> f -< (m'- I.Interval 1 1, I.Interval 1 1))
                          (proc n' -> do x <- f -< (m,n'-I.Interval 1 1)
                                         f -< (m'- I.Interval 1 1, x)) -<< n)
            -<< m
    in it "ackerman ([0,inf], [0,inf]) should be [0,inf] " $ do
         runFixT' (SW.stack (SW.reuse (const head) SW.topOut)) W.finite ackermann (I.Interval 0 Infinity, I.Interval 0 Infinity)
           `shouldBe` return (I.Interval 1 Infinity)

  describe "the analyis of a diverging program" $
    let diverge :: Cache s Int Sign
        diverge = fix $ \f -> proc n -> case n of
          0 -> f -< 0
          _ -> f -< (n-1)
    in it "should terminate with bottom" $
         runFixT diverge 5 `shouldBe` bottom

  describe "the analysis of a failing program" $
    let recurseFail :: ErrorFix s Int Sign
        recurseFail = fix $ \f -> proc n -> case n of
          0 -> fail -< ()
          _ -> f -< (n-1)
    in it "should fail, but update the fixpoint cache" $
         runFixT'' SW.finite SW.Unit W.finite (runFailureT recurseFail) 5
            `shouldBe` (S.fromList [(n,Terminating (Fail ())) | n <- [0..5]], return (Fail ()))

  describe "the analysis of a stateful program" $
    let timesTwo :: StateFix s IV ()
        timesTwo = fix $ \f -> proc n -> case n of
          I.Interval 0 0 -> returnA -< ()
          _ -> do
            s <- get -< ()
            put -< s + I.Interval 1 1
            f -< n-1
            s' <- get -< ()
            put -< s'+ I.Interval 1 1
    in it "should cache the state of the program" $
         runFixT'' SW.finite SW.Unit W.finite (runStateT timesTwo) (0,5) `shouldBe`
           (S.fromList [((fromIntegral n,5-fromIntegral n),
                          return (10-fromIntegral n,())) | n <- [0..5::Int]],
            return (10,()))
  where

    ifLowerThan :: (Num n, Ord n, ArrowChoice c, Complete (c (Interval n, Interval n) x)) => n -> c (Interval n) x -> c (Interval n) x -> c (Interval n) x
    ifLowerThan l f g = proc x -> case x of
      I.Interval m n
        | n <= l -> f -< x
        | l < m -> g -< x
        | m <= l && l+1 <= n -> joined f g -< (I.Interval m l, I.Interval (l+1) n)
        | otherwise -> f -< I.Interval m l

data EvenOdd = Even | Odd deriving (Eq,Generic,Show)
instance Hashable EvenOdd
