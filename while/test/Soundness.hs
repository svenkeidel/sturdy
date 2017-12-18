module Soundness where

import WhileLanguage

import           Data.Error
import           Data.Order
import           Data.Text

import           Test.Hspec
import           Test.QuickCheck
import           Test.Hspec.Core.QuickCheck (modifyMaxSuccess)

instance Arbitrary Text where
  arbitrary = fmap pack arbitrary

withSize :: Spec -> Spec
withSize = modifyMaxSuccess (const 1000)

sound :: (Arbitrary a, PreOrd r, PreOrd p, Show a, Show r, Show p) =>
  String ->
  (a -> Prog) ->
  (Prog -> Error String r) ->
  (Prog -> Error String r) ->
  (Prog -> Error String p) ->
  (Prog -> Error String p) ->
  Spec
sound desc genprog runConcrete runAbstract propConcrete propAbstract = do
  withSize $ it ("sound value approximation " ++ desc) $ property $ \a -> do
    let prog = genprog a
    let concreteVal = runConcrete prog
    let abstractVal = runAbstract prog
    abstractVal `shouldSatisfy` (concreteVal ⊑)

  withSize $ it ("sound property approximation " ++ desc) $ property $ \a -> do
    let prog = genprog a
    let concreteProp = propConcrete prog
    let abstractProp = propAbstract prog
    abstractProp `shouldSatisfy` (concreteProp ⊑)

soundProg :: (PreOrd r, PreOrd p, Show r, Show p) =>
  String ->
  [Statement] ->
  (Prog -> Error String r) ->
  (Prog -> Error String r) ->
  (Prog -> Error String p) ->
  (Prog -> Error String p) ->
  Spec
soundProg desc prog runConcrete runAbstract propConcrete propAbstract = do
  let concreteVal = runConcrete prog
  let abstractVal = runAbstract prog
  it ("sound value approximation " ++ desc) $ abstractVal `shouldSatisfy` (concreteVal ⊑)

  let concreteProp = propConcrete prog
  let abstractProp = propAbstract prog
  it ("sound property approximation " ++ desc) $ abstractProp `shouldSatisfy` (concreteProp ⊑)
