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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module ContextSensitivitySpec where

import           Prelude hiding (lookup,Bounded,fail,Bool)

import           TestPrograms

import           Control.Monad(forM_)
import           Control.Arrow
import           Control.Arrow.Fix as F
import qualified Control.Arrow.Trans as Arrow
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Chaotic
import           Control.Arrow.Transformer.Abstract.Fix.ContextSensitive.Cache
import           Control.Arrow.Transformer.Abstract.Fix.ContextSensitive.CallSite
-- import           Control.Arrow.Transformer.Abstract.Fix.Trace

import qualified Data.Abstract.Boolean as Abs
import           Data.Abstract.Terminating (Terminating)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.Widening (Widening)
import qualified Data.Abstract.Widening as W

import           Data.Boolean
import           Data.Identifiable
import           Data.Order
import           Data.Hashable
import           Data.Proxy

import           GHC.Generics
import           GHC.TypeLits

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  --describe "Parallel" (sharedSpec (\f -> snd . Arrow.run (toParallel f) (S.stackWidening ?stackWiden (S.parallel (T.widening ?widen)))))
  describe "Chaotic" $ do
    describe "iterate inner component" $
      callsiteSpec (\f a -> snd $ Arrow.run (toChaotic f) ({-trace-} iterateInner) ?widenA (T.widening ?widenB) a)
    describe "iterate outer component" $
      callsiteSpec (\f a -> snd $ Arrow.run (toChaotic f) ({-trace-} iterateOuter) ?widenA (T.widening ?widenB) a)

data Val = Num IV | Unit | Top deriving (Show,Eq,Generic,Hashable)

{-# INLINE callsiteSpec #-}
callsiteSpec :: (forall k lab a b.
                 (KnownNat k, Show lab, Show a, Show b, Identifiable lab, Identifiable a, PreOrd a, Complete b,
                  ?widenA :: Widening a, ?widenB :: Widening b, ?sensitivity :: Proxy (k :: Nat))
                => Arr (lab,a) b -> (lab,a) -> Terminating b) -> Spec
callsiteSpec run = do
  describe "diamond" $ do
    let diamond :: Arr (Funs,Val) Val
        diamond = fix $ \call -> proc (e,x) -> case e of
          Main -> do
            call -< (Fun1,Unit)
            call -< (Fun2,Unit)
          Fun1 ->
            call -< (Id,Num (iv 1 1))
          Fun2 ->
            call -< (Id,Num (iv 2 2))
          Id ->
            returnA -< x

    let ?widenA = W.finite
    let ?widenB = W.finite

    it "context insensitive" $
      let ?sensitivity = Proxy @0 in
      run diamond (Main,Unit) `shouldBe` return Top

    it "1-callsite sensitive" $
      let ?sensitivity = Proxy @1 in
      run diamond (Main,Unit) `shouldBe` return (Num (iv 1 2))

    it "2-callsite sensitive" $
      let ?sensitivity = Proxy @2 in
      run diamond (Main,Unit) `shouldBe` return (Num (iv 2 2))

  describe "mutual recursive functions" $ do
    let ?widenA = W.finite
    let ?widenB = W.finite

    let runTests :: (KnownNat k, ?sensitivity :: Proxy (k :: Nat)) => [(EvenOdd,Int,Abs.Bool)] -> IO ()
        runTests l = forM_ l $ \(eo,arg,res) -> do
          run evenOdd (eo,iv (fromIntegral arg) (fromIntegral arg)) `shouldBe` return res

    it "context insensitive" $ do
      let ?sensitivity = Proxy @0
      runTests [
        (Even,1,false), (Odd,1,true),
        (Even,2,top),   (Odd,2,top)
        ]

    it "1-callsite sensitive" $ do
      let ?sensitivity = Proxy @1
      runTests [
        (Even,1,false), (Odd,1,true),
        (Even,2,true),  (Odd,2,false),
        (Even,3,top),   (Odd,3,top)
        ]

    it "2-callsite sensitive" $ do
      let ?sensitivity = Proxy @2
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

-- widening :: Widening Val
-- widening Top Top = (Stable,Top)
-- widening (Num x) (Num y) = Num <$> (I.widening x y)
-- widening Unit Unit = (Stable,Unit)
-- widening _ _ = (Instable,Top)

data Funs = Main | Fun1 | Fun2 | Id deriving (Show,Eq,Generic)
instance Hashable Funs
instance PreOrd Funs where
  e1 ⊑ e2 = e1 == e2

toChaotic :: (KnownNat k, ?sensitivity :: Proxy k, Show lab, Show a, Show b, Identifiable lab, Identifiable a, PreOrd a, Complete b)
          => Arr (lab,a) b -> TerminatingT (FixT (lab,a) (Terminating b)
                                              (ChaoticT (lab,a) (Terminating b)
                                                (CallSiteT k lab
                                                  (-- TraceT
                                                    (CacheT _ lab a (Terminating b) (->)))))) (lab,a) b
toChaotic x = x
{-# INLINE toChaotic #-}
