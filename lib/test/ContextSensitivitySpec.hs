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

import           Prelude hiding (lookup,Bounded,fail)

import           Control.Arrow
import           Control.Arrow.Fix as F
import qualified Control.Arrow.Trans as Arrow
import           Control.Arrow.Order hiding (bottom)
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Chaotic
import           Control.Arrow.Transformer.Abstract.Fix.ContextSensitive.Cache
import           Control.Arrow.Transformer.Abstract.Fix.ContextSensitive.CallSite
-- import           Control.Arrow.Transformer.Abstract.Fix.Trace

import           Data.Abstract.InfiniteNumbers
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import           Data.Abstract.Terminating (Terminating)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.Widening (Widening)
import qualified Data.Abstract.Widening as W

import           Data.Identifiable
import           Data.Order
import           Data.Hashable
import           Data.Profunctor
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

type Arr x y = forall c. (ArrowChoice c, Profunctor c, ArrowComplete y c, ArrowFix (c x y)) => c x y

callsiteSpec :: (forall k lab a b.
                 (KnownNat k, Show lab, Show a, Show b, Identifiable lab, Identifiable a, PreOrd a, Complete b,
                  ?widenA :: Widening a, ?widenB :: Widening b, ?sensitivity :: Proxy (k :: Nat))
                => Arr (lab,a) b -> (lab,a) -> Terminating b) -> Spec
callsiteSpec run = do
  let prog :: Arr (Funs,Val) Val
      prog = fix $ \call -> proc (e,x) -> case e of
        Main -> do
          call -< (Fun1,Unit)
          call -< (Fun2,Unit)
        Fun1 ->
          call -< (Id,Num (iv 1 1))
        Fun2 ->
          call -< (Id,Num (iv 2 2))
        Id ->
          returnA -< x

  it "context insensitive" $
    let ?widenA = W.finite in 
    let ?widenB = W.finite in
    let ?sensitivity = Proxy @0 in
    run prog (Main,Unit) `shouldBe` return Top

  it "1-callsite sensitive" $
    let ?widenA = W.finite in 
    let ?widenB = W.finite in
    let ?sensitivity = Proxy @1 in
    run prog (Main,Unit) `shouldBe` return (Num (iv 1 2))

  it "2-callsite sensitive" $
    let ?widenA = W.finite in 
    let ?widenB = W.finite in
    let ?sensitivity = Proxy @2 in
    run prog (Main,Unit) `shouldBe` return (Num (iv 2 2))
{-# INLINE callsiteSpec #-}

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

type IV = Interval (InfiniteNumber Int)

iv :: InfiniteNumber Int -> InfiniteNumber Int -> IV
iv n m = I.Interval n m

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
