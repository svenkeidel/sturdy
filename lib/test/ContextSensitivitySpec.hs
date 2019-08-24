{-# LANGUAGE Arrows #-}
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
import           Control.Arrow.Transformer.Abstract.Fix.Chaotic
import           Control.Arrow.Transformer.Abstract.Fix.Context

import           Data.Abstract.Context
import           Data.Abstract.Context.Insensitive
import           Data.Abstract.InfiniteNumbers
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import           Data.Abstract.Terminating (Terminating)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.Widening (Widening)
import qualified Data.Abstract.Widening as W

import           Data.Identifiable
import           Data.Empty
import           Data.Order
import           Data.Hashable
import           Data.Profunctor
import           Data.Proxy

import           GHC.Generics

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  --describe "Parallel" (sharedSpec (\f -> snd . Arrow.run (toParallel f) (S.stackWidening ?stackWiden (S.parallel (T.widening ?widen)))))
  describe "Chaotic"  (sharedSpec (\f -> snd . Arrow.run (toChaotic f) id ?widenA (T.widening ?widenB)))

data Val = Num IV | Unit | Top deriving (Show,Eq,Generic,Hashable)

type Arr ctx x y = forall c. (ArrowChoice c, Profunctor c, ArrowComplete y c, ArrowFix (c x y)) => c x y

sharedSpec :: (forall ctx lab a b.
                 (Show a, Show b, Identifiable lab, Identifiable a, PreOrd a, Complete b, IsContext ctx lab, IsEmpty ctx,
                  ?widenA :: Widening a, ?widenB :: Widening b, ?sensitivity :: Proxy ctx)
                => Arr ctx (lab,a) b -> (lab,a) -> Terminating b) -> Spec
sharedSpec run = do
  let prog :: Arr ctx (Funs,Val) Val
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

  it "context-insensitive" $
    let ?widenA = W.finite in 
    let ?widenB = W.finite in
    let ?sensitivity = Proxy @(ContextInsensitive Funs) in
    run prog (Main,Unit) `shouldBe` return (Num (iv 1 2))
{-# INLINE sharedSpec #-}

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

data Funs = Main | Fun1 | Fun2 | Id deriving (Eq,Generic)
instance Hashable Funs
instance PreOrd Funs where
  e1 ⊑ e2 = e1 == e2

toChaotic :: (?sensitivity :: Proxy ctx, IsContext ctx lab, Show a, Show b, Identifiable lab, Identifiable a, PreOrd a, Complete b)
          => Arr ctx (lab,a) b -> TerminatingT (ChaoticT (lab,a) (Terminating b) (ContextT ctx lab a (Terminating b) (->))) (lab,a) b
toChaotic x = x
{-# INLINE toChaotic #-}
