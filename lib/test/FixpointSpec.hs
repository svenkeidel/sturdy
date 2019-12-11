{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FixpointSpec where

import           Prelude hiding (id,(.),lookup,Bounded,Bool(..),fail)

import           TestPrograms

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Fix.Stack (ArrowStack,widenInput,maxSize,reuseByMetric)
import qualified Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Fix.Cache (ArrowCache)
import           Control.Arrow.Fix.Chaotic (ArrowChaotic,chaotic,iterateInner,iterateOuter)
import           Control.Arrow.Fix.Parallel (ArrowParallel,parallel)
import qualified Control.Arrow.Trans as Arrow
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Metrics
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

import           Text.Printf

import           Test.Hspec hiding (parallel)

main :: IO ()
main = hspec spec

metricFile :: String
metricFile = "fixpoint.csv"

spec :: Spec
spec = do
  describe "Parallel" $
    fixpointSpec "parallel" (runParallel parallel)
  describe "Chaotic" $ do
    describe "simple" $ fixpointSpec "Chaotic.simple" (runChaotic chaotic)
    describe "inner component" $ fixpointSpec "Chaotic.inner" (runChaotic iterateInner)
    describe "outer component" $ fixpointSpec "Chaotic.outer" (runChaotic iterateOuter)

fixpointSpec :: String -> (forall a b. (Show a, Show b, Identifiable a, Complete b, ?strat :: Strat a b, ?widen :: Widening b) => Arr a b -> a -> (Metrics a,Terminating b)) -> Spec
fixpointSpec algName eval = sharedSpec $ \name f a -> do
  let (metrics,res) = eval f a
  let csv = printf "\"%s%s\",%s,%s\n" name (show a) algName (toCSV metrics)
  appendFile metricFile csv
  return res
{-# INLINE fixpointSpec #-}

sharedSpec :: (forall a b. (Show a, Show b, Identifiable a, PreOrd a, Complete b, ?strat :: Strat a b, ?widen :: Widening b)
           => String -> Arr a b -> a -> IO (Terminating b)) -> Spec
sharedSpec run = do
  describe "fibonacci" $ do
    it "fib[5,10] should be [5,55]" $
       let ?strat = Strat id in
       let ?widen = W.finite in
       run "fib" fib (iv 5 10) `shouldBe'` return (iv 5 55)

    it "fib[100,110] with widening should be [0,∞]" $
       let ?strat = Strat (widenInput I.widening) in
       let ?widen = I.widening in
       run "fib" fib (iv 100 110) `shouldBe'` return (iv 0 Infinity)

    it "fib[1,∞] should be [0,∞]" $
       let ?strat = Strat (reuseByMetric euclid) in
       let ?widen = I.widening in
       run "fib" fib (iv 0 Infinity) `shouldBe'` return (iv 0 Infinity)

  describe "factorial" $ do
    it "fact[5,10] should be [5!,10!] = [12,3628800]" $
       let ?strat = Strat id in
       let ?widen = W.finite in
       run "fact" fact (iv 5 10) `shouldBe'` return (iv 120 3628800)

    it "fact[10,15] with stack size 3 should be [10,15] * [9,14] * [8,13] * [1,∞] = [720,∞]" $
       let ?strat = Strat (maxSize 3 (widenInput I.widening)) in
       let ?widen = I.widening in
       run "fact" fact (iv 10 15) `shouldBe'` return (iv 720 Infinity)

    it "fact[0,∞] should be [1,∞]" $
       let ?strat = Strat id in
       let ?widen = I.widening in
       run "fact" fact (iv 0 Infinity) `shouldBe'` return (iv 1 Infinity)

  describe "ackermann" $ do
    it "ack([0,3],[0,3]) should be [1,61] " $
       let ?strat = Strat id in
       let ?widen = W.finite in
       run "ackermann" ackermann (iv 0 3, iv 0 3) `shouldBe'` return (iv 1 61)

    it "ack([0,∞],[0,∞]) with euclidian reuseByMetric should be [1,∞] " $
       let ?strat = Strat (reuseByMetric (euclid M.** euclid)) in
       let ?widen = I.widening in
       run "ackermann" ackermann (iv 0 Infinity, iv 0 Infinity) `shouldBe'` return (iv 1 Infinity)

  describe "even odd" $
    it "even([0,∞]) should be top" $
       let ?strat = Strat (reuseByMetric (M.unit M.** euclid)) in
       let ?widen = W.finite in
       run "even" evenOdd (Even,iv 0 Infinity) `shouldBe'` top

  describe "diverge" $
    it "should terminate with bottom" $
      let ?strat = Strat id in
      let ?widen = W.finite in
      run "diverge" diverge 5 `shouldBe'` bottom
  where
    shouldBe' f g = do
      x <- f
      x `shouldBe` g
    {-# INLINE shouldBe' #-}
{-# INLINE sharedSpec #-}

toParallel :: Complete b => Arr a b -> TerminatingT (FixT a (Terminating b) (MetricsT a (StackT Stack a (CacheT (Parallel Cache) a (Terminating b) (->))))) a b
toParallel x = x
{-# INLINE toParallel #-}

runParallel :: (forall a b c. (Identifiable a, ArrowChoice c, ArrowParallel c, ArrowStack a c, ArrowCache a b c) => FixpointCombinator c a b)
           -> (forall a b. (Identifiable a, Complete b, ?strat :: Strat a b, ?widen :: Widening b) => Arr a b -> a -> (Metrics a,Terminating b))
runParallel algorithm f a = snd $ Arrow.run (toParallel f) (getStrat ?strat . algorithm) (T.widening ?widen) a
{-# INLINE runParallel #-}

toChaotic :: (Identifiable a, Complete b)
          => Arr a b -> TerminatingT (FixT a (Terminating b) (MetricsT a (ChaoticT a (StackT Stack a (CacheT Cache a (Terminating b) (->)))))) a b
toChaotic x = x
{-# INLINE toChaotic #-}

runChaotic :: (forall a b c. (Show a, Show b, Identifiable a, ArrowChoice c, ArrowStack a c, ArrowChaotic a c, ArrowCache a b c) => FixpointCombinator c a b)
           -> (forall a b. (Show a, Show b, Identifiable a, Complete b, ?strat :: Strat a b, ?widen :: Widening b) => Arr a b -> a -> (Metrics a,Terminating b))
runChaotic algorithm f a = snd $ Arrow.run (toChaotic f) (getStrat ?strat . algorithm) (T.widening ?widen) a
{-# INLINE runChaotic #-}
