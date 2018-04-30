module ConcreteSpec where

import Syntax
import Concrete

import Test.Hspec
import Data.Fixed (mod')

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "literals" $ do
    it "number literals" $ do
      let program = ENumber 2.0
      eval scope program `shouldBe` VNumber 2.0
    
    it "boolean literals" $ do
      let program = EBool True
      eval scope program `shouldBe` VBool True
    
    it "string literals" $ do
      let program = EString "test"
      eval scope  program `shouldBe` VString "test"
    
    it "undefined literal" $ do
      eval scope EUndefined `shouldBe` VUndefined
      
    it "null literal" $ do
      eval scope ENull `shouldBe` VNull

    it "lambda literal" $ do
      eval scope (ELambda [] (ENumber 1.0)) `shouldBe` (VLambda [] (ENumber 1.0))
  
  describe "objects" $ do
    it "object with numbers" $ do
      let program = EObject [("a", ENumber 2.0), ("b", ENumber 3.0)]
      eval scope program `shouldBe` VObject [("a", VNumber 2.0), ("b", VNumber 3.0)]
  
  describe "identifiers" $ do
    it "single identifier" $ do 
      let program = EId "a"
      let scopeWithId = Scope (insert "a" (VNumber 1.0) (values scope)) (parent scope)
      eval scopeWithId program `shouldBe` VNumber 1.0
    
  describe "operators" $ do
    it "number addition" $ do
      let program = EOp ONumPlus [(ENumber 1.0), (ENumber 2.0)]
      eval scope program `shouldBe` VNumber 3.0
    it "number multiplication" $ do
      let program = EOp OMul [(ENumber 3.0), (ENumber 2.0)]
      eval scope program `shouldBe` VNumber 6.0
    it "number division" $ do
      let program = EOp ODiv [(ENumber 3.0), (ENumber 2.0)]
      eval scope program `shouldBe` VNumber 1.5
    it "number modulus" $ do
      let program = EOp OMod [(ENumber 3.0), (ENumber 0.7)]
      eval scope program `shouldBe` VNumber (mod' (3.0 :: Double) (0.7 :: Double))
    it "number subtraction" $ do
      let program = EOp OSub [(ENumber 3.0), (ENumber 2.0)]
      eval scope program `shouldBe` VNumber 1.0
    
    it "string addition" $ do
      let program = EOp OStrPlus [(EString "a"), (EString "b")]
      eval scope program `shouldBe` VString "ab"
  
  where scope = emptyScope