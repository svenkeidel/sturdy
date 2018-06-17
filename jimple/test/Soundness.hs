{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Soundness where

import SharedSemantics

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

shouldBeApproximated :: (HasCallStack,Galois c a,Show a,Show c) => c -> a -> Expectation
c `shouldBeApproximated` a = unless (ca ⊑ a) (expectationFailure msg)
  where ca = fmap alpha c
        msg = "soundness check failed: " ++ show ca ++ " ⊑ " ++ show a

soundExpr :: (Galois vc va,Show vc, Show va) =>
  String ->
  [(String,vc)] -> x ->
  ([(String,vc)] -> x -> Error (Exception vc) vc) ->
  ([(String,va)] -> x -> Error (Exception va) va) ->
  Spec
soundExpr desc mem x runConcrete runAbstract =
  it ("sound value approximation " ++ desc) $ do
    let mema = map (\(l,vc) -> (l,alpha [vc])) mem
    let vc = runConcrete mem x
    let va = runAbstract mema x
    vc `shouldBeApproximated` va

soundStatements :: (Galois vc va,Show vc, Show va) =>
  String ->
  [(String,vc)] -> x ->
  ([(String,vc)] -> x -> Error (Exception vc) (Maybe vc)) ->
  ([(String,va)] -> x -> Error (Exception va) (Maybe va)) ->
  Spec
soundStatements desc mem x runConcrete runAbstract =
  it ("sound value approximation " ++ desc) $ do
    let mema = map (\(l,vc) -> (l,alpha [vc])) mem
    let vc = runConcrete mem x
    let va = runAbstract mema x
    vc `shouldBeApproximated` va

soundProgram :: (Galois vc va,Show vc, Show va) =>
  String ->
  [CompilationUnit] -> x ->
  ([CompilationUnit] -> x -> Error (Exception vc) (Maybe vc)) ->
  ([CompilationUnit] -> x -> Error (Exception va) (Maybe va)) ->
  Spec
soundProgram desc units x runConcrete runAbstract =
  it ("sound value approximation " ++ desc) $ do
    let vc = runConcrete units x
    let va = runAbstract units x
    vc `shouldBeApproximated` va
