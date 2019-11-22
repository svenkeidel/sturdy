{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FixpointSpec where

import           Prelude hiding (id,(.),lookup,Bounded,Bool(..),fail)

import           TestPrograms

import           Control.Category
import           Control.Arrow.Fix.Combinator
import qualified Control.Arrow.Trans as Arrow
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Chaotic
import           Control.Arrow.Transformer.Abstract.Fix.Cache hiding (Widening)
import           Control.Arrow.Transformer.Abstract.Fix.Stack
-- import           Control.Arrow.Transformer.Abstract.Fix.Trace

import           Data.Identifiable
import qualified Data.Metric as M
import           Data.Abstract.InfiniteNumbers
import qualified Data.Abstract.Interval as I
import           Data.Abstract.Terminating (Terminating)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.Widening (Widening)
import qualified Data.Abstract.Widening as W

import           Data.Order

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  --describe "Parallel" (sharedSpec (\f -> snd . Arrow.run (toParallel f) (S.stackWidening ?stackWiden (S.parallel (T.widening ?widen)))))
  describe "Chaotic" $ do
    describe "iterate inner component" $
      sharedSpec (\f -> snd . Arrow.run (toChaotic f) (getStrat ?strat . iterateInner) (T.widening ?widen))
    describe "iterate outer component" $
      sharedSpec (\f -> snd . Arrow.run (toChaotic f) (getStrat ?strat . iterateOuter) (T.widening ?widen))

sharedSpec :: (forall a b. (Show a, Show b, Identifiable a, PreOrd a, Complete b, ?strat :: Strat a b, ?widen :: Widening b)
                => Arr a b -> a -> Terminating b) -> Spec
sharedSpec run = do
  describe "fibonacci" $ do
    it "fib[5,10] should be [5,55]" $
       let ?strat = Strat reuseExact in
       let ?widen = W.finite in
       run fib (iv 5 10) `shouldBe` return (iv 5 55)

    it "fib[100,110] with widening should be [0,∞]" $
       let ?strat = Strat (reuseExact . widenInput I.widening) in
       let ?widen = I.widening in
       run fib (iv 100 110) `shouldBe` return (iv 0 Infinity)

    it "fib[1,∞] should be [0,∞]" $
       let ?strat = Strat (reuseByMetric euclid) in
       let ?widen = I.widening in
       run fib (iv 0 Infinity) `shouldBe` return (iv 0 Infinity)

  describe "factorial" $ do
    it "fact[5,10] should be [5!,10!] = [12,3628800]" $
       let ?strat = Strat id in
       let ?widen = W.finite in
       run fact (iv 5 10) `shouldBe` return (iv 120 3628800)

    it "fact[10,15] with stack size 3 should be [10,15] * [9,14] * [8,13] * [1,∞] = [720,∞]" $
       let ?strat = Strat (maxSize 3 $ widenInput I.widening) in 
       let ?widen = I.widening in
       run fact (iv 10 15) `shouldBe` return (iv 720 Infinity)

    it "fact[0,∞] should be [1,∞]" $
       let ?strat = Strat id in
       let ?widen = I.widening in
       run fact (iv 0 Infinity) `shouldBe` return (iv 1 Infinity)

  describe "ackermann" $ do
    it "ack([0,3],[0,3]) should be [1,61] " $
       let ?strat = Strat reuseExact in
       let ?widen = W.finite in
       run ackermann (iv 0 3, iv 0 3) `shouldBe` return (iv 1 61)

    it "ack([0,∞],[0,∞]) with euclidian reuseByMetric should be [1,∞] " $
       let ?strat = Strat (reuseByMetric (euclid M.** euclid)) in
       let ?widen = I.widening in do
       pendingWith "FIXME: reuseByMetric is broken"
       run ackermann (iv 0 Infinity, iv 0 Infinity) `shouldBe` return (iv 1 Infinity)

  describe "even odd" $
    it "even([0,∞]) should be top" $
       let ?strat = Strat (reuseByMetric (M.unit M.** euclid)) in
       let ?widen = W.finite in
       run evenOdd (Even,iv 0 Infinity) `shouldBe` top

  describe "diverge" $
    it "should terminate with bottom" $
      let ?strat = Strat reuseExact in
      let ?widen = W.finite in
      run diverge 5 `shouldBe` bottom
{-# INLINE sharedSpec #-}

-- toParallel :: (Identifiable a, Complete b) => Arr a b -> Fix a b (TerminatingT (FixT a (Terminating b) (S.StackWideningT s a (S.ParallelT Cache a (Terminating b) ((->)))))) a b
-- toParallel x = x
-- {-# INLINE toParallel #-}

toChaotic :: (Identifiable a, Complete b)
          => Arr a b -> TerminatingT (FixT a (Terminating b) (ChaoticT a (StackT Stack a (CacheT Cache a (Terminating b) (->))))) a b
toChaotic x = x
{-# INLINE toChaotic #-}
