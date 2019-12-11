{-# OPTIONS_GHC -funfolding-use-threshold=1500 -fsimpl-tick-factor=200 #-}
module Main where

import           Prelude hiding (id,(.),lookup,Bounded,Bool(..),fail)

import           TestPrograms

import           Control.Category
import           Control.Arrow.Fix.Combinator
import           Control.Arrow.Trans
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Chaotic
import           Control.Arrow.Transformer.Abstract.Fix.Cache hiding (Widening)

import           Data.Identifiable
import qualified Data.Metric as M
import           Data.Abstract.InfiniteNumbers
import qualified Data.Abstract.Interval as I
import           Data.Abstract.Terminating (Terminating)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.Widening (Widening)
import qualified Data.Abstract.Widening as W

import           Criterion
import           Criterion.Main

type Simple a b = TerminatingT (FixT a (Terminating b) (CacheT Cache a (Terminating b) (->))) a b
type Advanced a b = TerminatingT (FixT a (Terminating b) (ChaoticT a (CacheT Cache a (Terminating b) (->)))) a b

{-# SPECIALIZE fib :: Simple IV IV #-}
{-# SPECIALIZE fib :: Advanced IV IV #-}

{-# SPECIALIZE fact :: Simple IV IV #-}
{-# SPECIALIZE fact :: Advanced IV IV #-}

main :: IO ()
main = do
  defaultMain
    [
      bgroup "fibonacci" [
        fibonacci (iv 5 10),
        fibonacci (iv 10 15),
        fibonacci (iv 15 20)
      ],
      bgroup "factorial" [
        factorial (iv 5 10),
        factorial (iv 10 15),
        factorial (iv 15 20)
      ]
    ]

  where
    fibonacci arg =
      bgroup (show arg) [
        bench "simple" $ nf fibSimple arg,
        bench "inner" $ nf fibInner arg,
        bench "outer" $ nf fibOuter arg
      ]

    fibSimple = snd . run (fib :: Simple IV IV) chaotic W.finite
    fibInner = snd . run (fib :: Advanced IV IV) iterateInner W.finite
    fibOuter = snd . run (fib :: Advanced IV IV) iterateOuter W.finite

    factorial arg =
      bgroup (show arg) [
        bench "simple" $ nf factSimple arg,
        bench "inner" $ nf factInner arg,
        bench "outer" $ nf factOuter arg
      ]

    factSimple = snd . run (fact :: Simple IV IV) chaotic W.finite
    factInner = snd . run (fact :: Advanced IV IV) iterateInner W.finite
    factOuter = snd . run (fact :: Advanced IV IV) iterateOuter W.finite

--   describe "fibonacci" $ do
--     it "fib[5,10] should be [5,55]" $
--        let ?strat = Strat id in
--        let ?widen = W.finite in
--        run "fib" fib (iv 5 10) `shouldBe'` return (iv 5 55)

--     it "fib[100,110] with widening should be [0,∞]" $
--        let ?strat = Strat (widenInput I.widening) in
--        let ?widen = I.widening in
--        run "fib" fib (iv 100 110) `shouldBe'` return (iv 0 Infinity)

--     it "fib[1,∞] should be [0,∞]" $
--        let ?strat = Strat (reuseByMetric euclid) in
--        let ?widen = I.widening in
--        run "fib" fib (iv 0 Infinity) `shouldBe'` return (iv 0 Infinity)

--   describe "factorial" $ do
--     it "fact[5,10] should be [5!,10!] = [12,3628800]" $
--        let ?strat = Strat id in
--        let ?widen = W.finite in
--        run "fact" fact (iv 5 10) `shouldBe'` return (iv 120 3628800)

--     it "fact[10,15] with stack size 3 should be [10,15] * [9,14] * [8,13] * [1,∞] = [720,∞]" $
--        let ?strat = Strat (maxSize 3 (widenInput I.widening)) in 
--        let ?widen = I.widening in
--        run "fact" fact (iv 10 15) `shouldBe'` return (iv 720 Infinity)

--     it "fact[0,∞] should be [1,∞]" $
--        let ?strat = Strat id in
--        let ?widen = I.widening in
--        run "fact" fact (iv 0 Infinity) `shouldBe'` return (iv 1 Infinity)

--   describe "ackermann" $ do
--     it "ack([0,3],[0,3]) should be [1,61] " $
--        let ?strat = Strat id in
--        let ?widen = W.finite in
--        run "ackermann" ackermann (iv 0 3, iv 0 3) `shouldBe'` return (iv 1 61)

--     it "ack([0,∞],[0,∞]) with euclidian reuseByMetric should be [1,∞] " $
--        let ?strat = Strat (reuseByMetric (euclid M.** euclid)) in
--        let ?widen = I.widening in do
--        pendingWith "FIXME: reuseByMetric is broken"
--        run "ackermann" ackermann (iv 0 Infinity, iv 0 Infinity) `shouldBe'` return (iv 1 Infinity)

--   describe "even odd" $
--     it "even([0,∞]) should be top" $
--        let ?strat = Strat (reuseByMetric (M.unit M.** euclid)) in
--        let ?widen = W.finite in
--        run "even" evenOdd (Even,iv 0 Infinity) `shouldBe'` top

--   describe "diverge" $
--     it "should terminate with bottom" $
--       let ?strat = Strat id in
--       let ?widen = W.finite in
--       run "diverge" diverge 5 `shouldBe'` bottom
--   where
--     shouldBe' f g = do
--       x <- f
--       x `shouldBe` g
--     {-# INLINE shouldBe' #-}
-- {-# INLINE sharedSpec #-}

