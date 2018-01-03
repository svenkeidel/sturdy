{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Soundness where

import WhileLanguage

import           Control.Monad (unless)

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

shouldBeApproximated :: (HasCallStack, Galois c a, Show a, Show c) => Error String c -> Error String a -> Expectation
c `shouldBeApproximated` a = unless (ca ⊑ a) (expectationFailure msg)
  where ca = fmap alpha c
        msg = "soundness check failed: " ++ show c ++ " ⊑ " ++ show a

sound :: (Arbitrary a, Galois rc ra, Galois pc pa, Show a, Show rc, Show ra, Show pc, Show pa) =>
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
    runConcrete prog `shouldBeApproximated` runAbstract prog

  withSize $ it ("sound property approximation " ++ desc) $ property $ \a -> do
    let prog = genprog a
    propConcrete prog `shouldBeApproximated` propAbstract prog

soundProg :: (Galois rc ra, Galois pc pa, Show rc, Show ra, Show pc, Show pa) =>
  String ->
  [Statement] ->
  (Prog -> Error String rc) ->
  (Prog -> Error String ra) ->
  (Prog -> Error String pc) ->
  (Prog -> Error String pa) ->
  Spec
soundProg desc prog runConcrete runAbstract propConcrete propAbstract = do
  it ("sound value approximation " ++ desc) $
    runConcrete prog `shouldBeApproximated` runAbstract prog

  it ("sound property approximation " ++ desc) $
    propConcrete prog `shouldBeApproximated` propAbstract prog
