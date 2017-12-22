{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Soundness where

import WhileLanguage

import           Data.Error
import           Data.Order
import           Data.Text
import           Data.GaloisConnection

import           Test.Hspec
import           Test.QuickCheck
import           Test.Hspec.Core.QuickCheck (modifyMaxSuccess)

instance Arbitrary Text where
  arbitrary = fmap pack arbitrary

withSize :: Spec -> Spec
withSize = modifyMaxSuccess (const 1000)

sound :: (Arbitrary a, Galois rc ra, Galois pc pa, Show a, Show ra, Show pa) =>
  String ->
  (a -> Prog) ->
  (Prog -> Error String rc) ->
  (Prog -> Error String ra) ->
  (Prog -> Error String pc) ->
  (Prog -> Error String pa) ->
  Spec
sound desc genprog runConcrete runAbstract propConcrete propAbstract = do
  withSize $ it ("sound value approximation " ++ desc) $ property $ \a -> do
    let prog = genprog a
    let concreteVal = runConcrete prog
    let concreteValAbstracted = fmap alpha concreteVal
    let abstractVal = runAbstract prog
    abstractVal `shouldSatisfy` (concreteValAbstracted ⊑)

  withSize $ it ("sound property approximation " ++ desc) $ property $ \a -> do
    let prog = genprog a
    let concreteProp = propConcrete prog
    let concretePropAbstracted = fmap alpha concreteProp
    let abstractProp = propAbstract prog
    abstractProp `shouldSatisfy` (concretePropAbstracted ⊑)

soundProg :: (Galois rc ra, Galois pc pa, Show ra, Show pa) =>
  String ->
  [Statement] ->
  (Prog -> Error String rc) ->
  (Prog -> Error String ra) ->
  (Prog -> Error String pc) ->
  (Prog -> Error String pa) ->
  Spec
soundProg desc prog runConcrete runAbstract propConcrete propAbstract = do
  let concreteVal = runConcrete prog
  let concreteValAbstracted = fmap alpha concreteVal
  let abstractVal = runAbstract prog
  it ("sound value approximation " ++ desc) $ abstractVal `shouldSatisfy` (concreteValAbstracted ⊑)

  let concreteProp = propConcrete prog
  let concretePropAbstracted = fmap alpha concreteProp
  let abstractProp = propAbstract prog
  it ("sound property approximation " ++ desc) $ abstractProp `shouldSatisfy` (concretePropAbstracted ⊑)
