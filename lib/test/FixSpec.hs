{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImplicitParams #-}
module FixSpec where

import           Prelude hiding (lookup,Bounded,Bool(..))

import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.State
import           Control.Arrow.Fail
import           Control.Arrow.State

import           Data.Boolean(Logic(..))
import           Data.Abstract.Boolean(Bool)
import           Data.Abstract.Bounded
import           Data.Abstract.Error
import           Data.Abstract.InfiniteNumbers
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import           Data.Abstract.Sign (Sign)
import qualified Data.Abstract.Store as S
import           Data.Abstract.Terminating

import           Data.Order
import           Data.Hashable

import           GHC.Generics

import           Test.Hspec

main :: IO ()
main = hspec spec

type Cache x y = Fix x y x y
type ErrorFix x y = Except () (Fix x (Error () y)) x y
type StateFix x y = State IV (Fix (IV,x) (IV,y)) x y
type IV = Bounded (Interval (InfiniteNumber Int))

spec :: Spec
spec = do
  describe "the analysis of the fibonacci numbers" $
    let ?bound = I.Interval (-500) 500 in
    let fib :: Cache IV IV -> Cache IV IV
        fib f =
          ifLowerThan 0
            (proc _ -> returnA -< bounded (I.Interval 0 0))
            (ifLowerThan 1 (proc _ -> returnA -< bounded (I.Interval 1 1))
                           (proc n -> do
                              x <- f -< n - bounded (I.Interval 1 1)
                              y <- f -< n - bounded (I.Interval 2 2)
                              returnA -< x + y))

    in it "should memoize numbers that have been computed before already" $ do
         runFix (fixA fib :: Cache IV IV) (bounded (I.Interval 5 10)) `shouldBe` return (bounded (I.Interval 5 55))
         runFix (fixA fib :: Cache IV IV) (bounded (I.Interval 0 Infinity)) `shouldBe` return (bounded top)

  describe "the analysis of the factorial function" $
    let ?bound = top in
    let fact f = proc n -> do
          ifLowerThan 1 (proc _ -> returnA -< bounded (I.Interval 1 1))
                        (proc n -> do {x <- f -< (n-bounded (I.Interval 1 1)); returnA -< n * x}) -< n
    in it "fact [-inf,inf] should produce [1,inf]" $
         runFix (fixA fact :: Cache IV IV) (bounded top)
           `shouldBe` return (bounded (I.Interval 1 Infinity))

  describe "the even and odd functions" $
    let ?bound = top in
    let evenOdd :: Cache (EvenOdd,IV) Bool -> Cache (EvenOdd,IV) Bool
        evenOdd f = proc (e,x) -> case e of
          Even -> ifLowerThan 0 (proc _ -> returnA -< true)
                                (ifLowerThan 1 (proc _ -> returnA -< false)
                                               (proc x -> f -< (Odd,x-bounded (I.Interval 1 1)))) -< x
          Odd -> ifLowerThan 0 (proc _ -> returnA -< false)
                                (ifLowerThan 1 (proc _ -> returnA -< true)
                                               (proc x -> f -< (Even,x-bounded (I.Interval 1 1)))) -< x
    in it "even([-inf,inf]) should produce top" $
         runFix (fixA evenOdd) (Even,bounded (I.Interval 0 Infinity)) `shouldBe` top

  describe "the ackermann function" $
    let ?bound = I.Interval (-50) 50 in
    let ackermann :: Cache (IV,IV) IV -> Cache (IV,IV) IV
        ackermann f = proc (m,n) ->
          ifLowerThan 0
            (proc _ -> returnA -< n + bounded (I.Interval 1 1))
            (proc m' -> ifLowerThan 0
                          (proc _ -> f -< (m'- bounded (I.Interval 1 1), bounded (I.Interval 1 1)))
                          (proc n' -> do x <- f -< (m,n'-bounded (I.Interval 1 1))
                                         f -< (m'- bounded (I.Interval 1 1), x)) -<< n)
            -<< m
    in it "ackerman ([0,inf], [0,inf]) should be [0,inf] " $ do
         runFix (fixA ackermann) (bounded (I.Interval 0 Infinity), bounded (I.Interval 0 Infinity))
           `shouldBe` return (bounded top)

  describe "the analyis of a diverging program" $
    let diverge :: Cache Int Sign -> Cache Int Sign
        diverge f = proc n -> case n of
          0 -> f -< 0
          _ -> f -< (n-1)
    in it "should terminate with bottom" $
         runFix (fixA diverge) 5
           `shouldBe` bottom

  describe "the analysis of a failing program" $
    let recurseFail :: ErrorFix Int Sign -> ErrorFix Int Sign
        recurseFail f = proc n -> case n of
          0 -> failA -< ()
          _ -> f -< (n-1)
    in it "should fail, but update the fixpoint cache" $
         runFix' (runExcept (fixA recurseFail)) 5
            `shouldBe` (S.fromList [(n,Terminating (Fail ())) | n <- [0..5]], return (Fail ()))

  describe "the analysis of a stateful program" $
    let ?bound = I.Interval (-50) 50 in
    let timesTwo :: StateFix IV () -> StateFix IV ()
        timesTwo f = proc n -> case n of
          Bounded _ (I.Interval 0 0) -> returnA -< ()
          _ -> do
            s <- getA -< ()
            putA -< s + bounded (I.Interval 1 1)
            f -< (n- bounded 1)
            s' <- getA -< ()
            putA -< s'+ bounded (I.Interval 1 1)
    in it "should cache the state of the program" $
         runFix' (runState (fixA timesTwo)) (bounded 0, bounded 5) `shouldBe`
           (S.fromList [((bounded (fromIntegral n),bounded 5-bounded (fromIntegral n)),
                          return (bounded 10-bounded (fromIntegral n),())) | n <- [0..5::Int]],
            return (bounded 10,()))
  where

    ifLowerThan :: (Num n, Ord n, ArrowChoice c, Complete (c (Bounded (Interval n), Bounded (Interval n)) x)) => n -> c (Bounded (Interval n)) x -> c (Bounded (Interval n)) x -> c (Bounded (Interval n)) x
    ifLowerThan l f g = proc b@(Bounded o x) -> case x of
      I.Interval m n
        | n <= l -> f -< b
        | l < m -> g -< b
        | m <= l && l+1 <= n -> joined f g -< (Bounded o (I.Interval m l), Bounded o (I.Interval (l+1) n))
        | otherwise -> f -< Bounded o (I.Interval m l)

data EvenOdd = Even | Odd deriving (Eq,Generic,Show)
instance Hashable EvenOdd
