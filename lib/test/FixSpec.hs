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
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Terminating

import           Data.Identifiable
import           Data.Boolean(Logic(..))
import           Data.Abstract.Boolean(Bool)
import           Data.Abstract.InfiniteNumbers
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import           Data.Abstract.Sign (Sign)
import           Data.Abstract.Terminating (Terminating)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.Widening (Widening)
import qualified Data.Abstract.Widening as W

import           Data.Abstract.IterationStrategy (IterationStrategy,IsEmpty)
import qualified Data.Abstract.IterationStrategy as S

import           Data.Order
import           Data.Hashable
import           Data.Profunctor
-- import           Data.Lens (iso')

import           GHC.Generics

import           Test.Hspec

main :: IO ()
main = hspec spec

type Arr s c x y  = Fix x y (TerminatingT (FixT s c () () (->))) x y
type IV = Interval (InfiniteNumber Int)

spec :: Spec
spec = do
  describe "fibonacci" $
    let fib :: Arr s c IV IV
        fib = fix $ \f ->
          ifLowerThan 0
            (proc _ -> returnA -< I.Interval 0 0)
            (ifLowerThan 1 (proc _ -> returnA -< I.Interval 1 1)
                           (proc n -> do
                              x <- f -< n - I.Interval 1 1
                              y <- f -< n - I.Interval 2 2
                              returnA -< x + y))

    in do
      it "fib[5,10] should be [5,55]" $
         let ?strat = S.finite in
         run fib (iv 5 10) `shouldBe` return (iv 5 55)

      it "fib[100,110] with widening should be [0,∞]" $
         let ?widen = I.widening in
         let ?strat = S.fromWidening I.widening $ cached
         in run fib (iv 100 110) `shouldBe` return (iv 0 Infinity)

      it "fib[1,∞] should be [0,∞]" $
         let ?widen = I.widening in
         let ?strat = S.reuseFirst $ cached
         in run fib (iv 0 Infinity) `shouldBe` return (iv 0 Infinity)

  describe "factorial" $
    let fact :: Arr s c IV IV
        fact = fix $ \f ->
          ifLowerThan 1 (proc _ -> returnA -< iv 1 1)
                        (proc n -> do x <- f -< (n - iv 1 1)
                                      returnA -< n * x)
    in do
      it "fact[5,10] should be [5!,10!] = [12,3628800]" $
         let ?strat = S.finite in
         run fact (iv 5 10) `shouldBe` return (iv 120 3628800)

      it "fact[10,15] with stack size 3 should be [1,∞] * [8,13] * [9,14] * [10,15] = [720,∞]" $
         let ?widen = I.widening in
         let ?strat = S.maxSize 3 cached $ S.fromWidening I.widening $ cached
         in run fact (iv 10 15) `shouldBe` return (iv 720 Infinity)

      it "fact[0,∞] should be [1,∞]" $
         let ?widen = I.widening in
         let ?strat = S.reuseFirst $ cached
         in run fact (iv 0 Infinity) `shouldBe` return (iv 1 Infinity)

  describe "ackermann" $
    let ackermann :: Arr s c (IV,IV) IV
        ackermann = fix $ \f -> proc (m,n) ->
          ifLowerThan 0
            (proc _ -> returnA -< n + iv 1 1)
            (proc m' -> ifLowerThan 0
                          (proc _ -> f -< (m'- iv 1 1, iv 1 1))
                          (proc n' -> do x <- f -< (m,n'-iv 1 1)
                                         f -< (m'- iv 1 1, x)) -<< n)
            -<< m
    in do
      it "ack([0,3],[0,3]) should be [1,61] " $
         let ?strat = S.finite in
         run ackermann (iv 0 3, iv 0 3) `shouldBe` return (iv 1 61)

      it "ack([0,3],[0,3]) with stack reuse should be [1,∞]" $
         let ?widen = I.widening in
         let ?strat = S.reuseFirst $ cached
         in run ackermann (iv 0 3, iv 0 3) `shouldBe` return (iv 1 Infinity)

      it "ack([0,∞],[0,∞]) should be [1,∞] " $
         let ?widen = I.widening in
         let ?strat = S.reuseFirst $ cached
         in run ackermann (iv 0 Infinity, iv 0 Infinity) `shouldBe` return (iv 1 Infinity)

  describe "mutual recursive functions" $
    let evenOdd :: Arr s c (EvenOdd,IV) Bool
        evenOdd = fix $ \f -> proc (e,x) -> case e of
          Even -> ifLowerThan 0 (proc _ -> returnA -< true)
                                (ifLowerThan 1 (proc _ -> returnA -< false)
                                               (proc x -> f -< (Odd,x-I.Interval 1 1))) -< x
          Odd -> ifLowerThan 0 (proc _ -> returnA -< false)
                                (ifLowerThan 1 (proc _ -> returnA -< true)
                                               (proc x -> f -< (Even,x-I.Interval 1 1))) -< x
    in
      it "even([0,∞]) should be top" $
         let ?widen = W.finite in
         let ?strat = cached
         in run evenOdd (Even,iv 0 Infinity) `shouldBe` top

  describe "non-terminating function" $
    let diverge :: Arr s c Int Sign
        diverge = fix $ \f -> proc n -> case n of
          0 -> f -< 0
          _ -> f -< (n-1)
    in it "should terminate with bottom" $
         let ?widen = W.finite in
         let ?strat = S.reuseFirst $ cached
         in
         run diverge 5 `shouldBe` bottom

  where
    run :: (Identifiable a, Complete b, IsEmpty (stack a (Terminating b)), IsEmpty (cache a (Terminating b)), ?strat :: IterationStrategy stack cache a (Terminating b))
        => Arr stack cache a b -> a -> Terminating b
    run f a = runFixT ?strat (runTerminatingT f) a
    
    cached :: (Show a, Show b, Identifiable a,PreOrd b,?widen :: Widening b) => IterationStrategy S.Stack S.Cache a (Terminating b)
    cached = S.cached (T.widening ?widen)

    ifLowerThan :: (Num n, Ord n, ArrowChoice c, Profunctor c, Complete (c (Interval n, Interval n) x)) => n -> c (Interval n) x -> c (Interval n) x -> c (Interval n) x
    ifLowerThan l f g = proc x -> case x of
      I.Interval m n
        | n <= l -> f -< x
        | l < m -> g -< x
        | m <= l && l+1 <= n -> joined f g -< (I.Interval m l, I.Interval (l+1) n)
        | otherwise -> f -< I.Interval m l

    iv :: InfiniteNumber Int -> InfiniteNumber Int -> IV
    iv n m = I.Interval n m

data EvenOdd = Even | Odd deriving (Eq,Generic,Show)
instance Hashable EvenOdd
instance PreOrd EvenOdd where
  e1 ⊑ e2 = e1 == e2
