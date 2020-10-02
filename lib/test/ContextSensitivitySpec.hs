{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=200 -fno-warn-redundant-constraints -fno-warn-partial-type-signatures #-}
module ContextSensitivitySpec where

import           Prelude hiding (lookup,Bounded,fail,Bool)

import           TestPrograms hiding (Fun(..))

import           Control.Monad(forM_)
import           Control.Arrow
import           Control.Arrow.Fix as F
import           Control.Arrow.Fix.Context(ArrowContext,callsiteSensitive)
import           Control.Arrow.Fix.Chaotic
import qualified Control.Arrow.Trans as Arrow
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Component
import           Control.Arrow.Transformer.Abstract.Fix.Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable as Cache hiding (Widening)
import           Control.Arrow.Transformer.Abstract.Fix.Context

import qualified Data.Abstract.Boolean as Abs
import           Data.Abstract.Terminating (Terminating)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.Widening (Widening)
import qualified Data.Abstract.Widening as W
import           Data.Abstract.CallString

import           Data.Boolean
import           Data.Identifiable
import           Data.Order
import           Data.Hashable
import           Data.Text.Prettyprint.Doc

import           GHC.Generics

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  -- do
  --describe "Parallel" (sharedSpec (\f -> snd . Arrow.run (toParallel f) (S.stackWidening ?stackWiden (S.parallel (T.widening ?widen)))))
  describe "Chaotic" $ do
    describe "inner component" $
      callsiteSpec $ \f a ->
        let ?contextWidening = ?widenA
            ?cacheWidening = T.widening ?widenB in
        let ?fixpointAlgorithm = fixpointAlgorithm (callsiteSensitive ?sensitivity fst . innermost) in
        snd $ Arrow.run (f :: ChaoticT _ _ _) a
    describe "outer component" $
      callsiteSpec $ \f a ->
        let ?contextWidening = ?widenA
            ?cacheWidening = T.widening ?widenB in
        let ?fixpointAlgorithm = fixpointAlgorithm (callsiteSensitive ?sensitivity fst . outermost) in
        snd $ Arrow.run (f :: ChaoticT _ _ _) a

data Val = Num IV | Unit | Top deriving (Show,Eq,Generic,Hashable)
instance Pretty Val where pretty = viaShow

type Line = Int

{-# INLINE callsiteSpec #-}
callsiteSpec :: (forall lab a b.
                 (Show lab, Show a, Show b, Identifiable lab, Identifiable a, PreOrd lab, PreOrd a, Complete b,
                  ?sensitivity :: Int, ?widenA :: Widening a, ?widenB :: Widening b)
                => Arr (lab,a) b -> (lab,a) -> Terminating b) -> Spec
callsiteSpec run = do
  describe "diamond" $ do
    let diamond :: Arr ((Line,Fun),Val) Val
        diamond = fix $ \call -> proc ((_,fun),x) -> case fun of
          Main -> do
            call -< ((1,Fun1),Num (iv 1 1))
            call -< ((2,Fun2),Num (iv 2 2))
            call -< ((3,Fun1),Num (iv 3 3))
          Fun1 ->
            call -< ((4,Id),x)
          Fun2 ->
            call -< ((5,Id),x)
          Id ->
            returnA -< x

    let ?widenA = W.finite
    let ?widenB = W.finite

    it "context insensitive" $
      let ?sensitivity = 0 in
      run diamond ((0,Main),Unit) `shouldBe` return Top

    it "1-callsite sensitive" $
      let ?sensitivity = 1 in
      run diamond ((0,Main),Unit) `shouldBe` return (Num (iv 1 3))

    it "2-callsite sensitive" $
      let ?sensitivity = 2 in
      run diamond ((0,Main),Unit) `shouldBe` return (Num (iv 3 3))

  describe "mutual recursive functions" $ do
    let ?widenA = W.finite
    let ?widenB = W.finite

    let runTests :: (?sensitivity :: Int) => [(EvenOdd,Int,Abs.Bool)] -> IO ()
        runTests l = forM_ l $ \(eo,arg,res) ->
          run evenOdd (eo,iv (fromIntegral arg) (fromIntegral arg)) `shouldBe` return res

    it "context insensitive" $ do
      let ?sensitivity = 0
      runTests [
        (Even,1,false), (Odd,1,true),
        (Even,2,top),  (Odd,2,top)
        ]

    it "1-callsite sensitive" $ do
      let ?sensitivity = 1
      runTests [
        (Even,1,false), (Odd,1,true),
        (Even,2,true), (Odd,2,false),
        (Even,3,top) , (Odd,3,top)
        ]

    it "2-callsite sensitive" $ do
      let ?sensitivity = 2
      runTests [
        (Even,1,false), (Odd,1,true),
        (Even,2,true),  (Odd,2,false),
        (Even,3,false), (Odd,3,true),
        (Even,4,top),   (Odd,4,top)
        ]

instance PreOrd Val where
  _ ⊑ Top = True
  Num x ⊑ Num y = x ⊑ y
  Unit ⊑ Unit = True
  _ ⊑ _ = False

instance Complete Val where
  Num x ⊔ Num y = Num (x ⊔ y)
  Unit ⊔ Unit = Unit
  _ ⊔ _ = Top

data Fun = Main | Fun1 | Fun2 | Id deriving (Show,Eq,Generic)
instance Hashable Fun
instance PreOrd Fun where
  e1 ⊑ e2 = e1 == e2

type ChaoticT lab a b =
  TerminatingT
    (FixT
      (ComponentT Component (lab,a)
        (StackT Stack (lab,a)
          (CacheT (Context (Cache.Proj2 (CtxCache (CallString lab))) Cache) (lab,a) (Terminating b)
            (ContextT (CallString lab)
              (->)))))) (lab,a) b
