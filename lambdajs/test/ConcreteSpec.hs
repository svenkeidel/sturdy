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
    it "number less than" $ do
      let program = EOp OLt [(ENumber 3.0), (ENumber 4.0)]
      eval scope program `shouldBe` VBool True

    it "string addition" $ do
      let program = EOp OStrPlus [(EString "a"), (EString "b")]
      eval scope program `shouldBe` VString "ab"
    it "string less than" $ do
      let program = EOp OStrLt [(EString "a"), (EString "b")]
      eval scope program `shouldBe` VBool True
    it "string length" $ do
      let program = EOp OStrLen [(EString "abc")]
      eval scope program `shouldBe` VNumber 3.0
    it "string starts with" $ do
      let program = EOp OStrStartsWith [(EString "abc"), (EString "a")]
      eval scope program `shouldBe` VBool True

    it "bool and" $ do
      let program = EOp OBAnd [(EBool True), (EBool False)]
      eval scope program `shouldBe` VBool False
    it "bool or" $ do
      let program = EOp OBOr [(EBool True), (EBool False)]
      eval scope program `shouldBe` VBool True
    it "bool xor" $ do
      let program = EOp OBXOr [(EBool True), (EBool False)]
      eval scope program `shouldBe` VBool True
    it "bool not" $ do
      let program = EOp OBNot [(EBool True)]
      eval scope program `shouldBe` VBool False

    it "typeof number" $ do
      let program = EOp OTypeof [(ENumber 1.0)]
      eval scope program `shouldBe` VString "number"
    it "typeof string" $ do
      let program = EOp OTypeof [(EString "a")]
      eval scope program `shouldBe` VString "string"
    it "typeof bool" $ do
      let program = EOp OTypeof [(EBool True)]
      eval scope program `shouldBe` VString "boolean"
    it "typeof undefined" $ do
      let program = EOp OTypeof [(EUndefined)]
      eval scope program `shouldBe` VString "undefined" 
    it "typeof function" $ do
      let program = EOp OTypeof [(ELambda [] (ENumber 1.0))]
      eval scope program `shouldBe` VString "function"  
    it "typeof object" $ do
      let program = EOp OTypeof [(EObject [])]
      eval scope program `shouldBe` VString "object"   
  
    it "is primitive number" $ do
      let program = EOp OIsPrim [(ENumber 1.0)]
      eval scope program `shouldBe` VBool True
    it "is primitive string" $ do
      let program = EOp OIsPrim [(EString "a")]
      eval scope program `shouldBe` VBool True
    it "is primitive object" $ do
      let program = EOp OIsPrim [(EObject [])]
      eval scope program `shouldBe` VBool False

  where scope = emptyScope