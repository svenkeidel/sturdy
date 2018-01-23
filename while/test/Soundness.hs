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
        msg = "soundness check failed: " ++ show ca ++ " ⊑ " ++ show a

sound :: (Arbitrary a, Galois rc ra, Galois pc pa, Show a, Show rc, Show ra, Show pc, Show pa) =>
  String ->
  (a -> Prog) ->
  (Prog -> Error String (rc, pc)) ->
  (Prog -> Error String (ra, pa)) ->
  Spec
sound desc genprog runConcrete runAbstract =
  withSize $ it ("sound value and property approximation " ++ desc) $ property $ \a -> do
    let prog = genprog a
    let (rc, pc) = unzipError $ runConcrete prog
    let (ra, pa) = unzipError $ runAbstract prog
    rc `shouldBeApproximated` ra
    pc `shouldBeApproximated` pa

soundProg :: (Galois rc ra, Galois pc pa, Show rc, Show ra, Show pc, Show pa) =>
  String ->
  [Statement] ->
  (Prog -> Error String (rc, pc)) ->
  (Prog -> Error String (ra, pa)) ->
  Spec
soundProg desc prog runConcrete runAbstract =
  it ("sound value and property approximation " ++ desc) $ do
    let (rc, pc) = unzipError $ runConcrete prog
    let (ra, pa) = unzipError $ runAbstract prog
    rc `shouldBeApproximated` ra
    pc `shouldBeApproximated` pa
