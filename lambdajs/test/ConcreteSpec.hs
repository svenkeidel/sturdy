module ConcreteSpec where

import Syntax
import ValueInterpreter (Store, LJSArrow, runArr, lookup, empty, insert)
import qualified ValueInterpreter as Interpreter (eval)

import Test.Hspec

import Data.Fixed (mod')
import Data.Either (isLeft)

main :: IO ()
main = hspec spec

eval :: Store -> Expr -> Either String Value
eval st e = case runArr (Interpreter.eval) e st of
  Left err -> Left err
  Right (_ , v) -> Right v

spec :: Spec
spec = do
  describe "literals" $ do
    it "number literals" $ do
      let program = ENumber 2.0
      eval scope program `shouldBe` Right (VNumber 2.0)
    it "boolean literals" $ do
      let program = EBool True
      eval scope program `shouldBe` Right (VBool True)
    it "string literals" $ do
      let program = EString "test"
      eval scope  program `shouldBe` Right (VString "test")
    it "undefined literal" $ do
      eval scope EUndefined `shouldBe` Right (VUndefined)
    it "null literal" $ do
      eval scope ENull `shouldBe` Right (VNull)
    it "lambda literal" $ do
      eval scope (ELambda [] (ENumber 1.0)) `shouldBe` Right ((VLambda [] (ENumber 1.0)))
  
  describe "objects" $ do
    it "object with numbers" $ do
      let program = EObject [("a", ENumber 2.0), ("b", ENumber 3.0)]
      eval scope program `shouldBe` Right (VObject [("a", VNumber 2.0), ("b", VNumber 3.0)])
  
  describe "identifiers" $ do
    it "single identifier" $ do 
      let program = EId "a"
      let scopeWithId = insert "a" (VNumber 1.0) empty
      eval scopeWithId program `shouldBe` Right (VNumber 1.0)
    
  describe "operators" $ do
    -- numer operators
    describe "numbers" $ do
      it "addition" $ do
        let program = EOp ONumPlus [(ENumber 1.0), (ENumber 2.0)]
        eval scope program `shouldBe` Right (VNumber 3.0)
      it "multiplication" $ do
        let program = EOp OMul [(ENumber 3.0), (ENumber 2.0)]
        eval scope program `shouldBe` Right (VNumber 6.0)
      it "division" $ do
        let program = EOp ODiv [(ENumber 3.0), (ENumber 2.0)]
        eval scope program `shouldBe` Right (VNumber 1.5)
      it "modulus" $ do
        let program = EOp OMod [(ENumber 3.0), (ENumber 0.7)]
        eval scope program `shouldBe` Right (VNumber (mod' (3.0 :: Double) (0.7 :: Double)))
      it "subtraction" $ do
        let program = EOp OSub [(ENumber 3.0), (ENumber 2.0)]
        eval scope program `shouldBe` Right (VNumber 1.0)
      it "less than" $ do
        let program = EOp OLt [(ENumber 3.0), (ENumber 4.0)]
        eval scope program `shouldBe` Right (VBool True)
      it "to integer" $ do
        let program = EOp OToInteger [(ENumber 3.5)]
        eval scope program `shouldBe` Right (VNumber 3.0)
      it "to int32" $ do
        let program = EOp OToInt32 [(ENumber ((2^31) + 1))]
        eval scope program `shouldBe` Right (VNumber (-(2^31) + 1))
      it "to uint32" $ do
        let program = EOp OToUInt32 [(ENumber (-(2^31)))]
        eval scope program `shouldBe` Right (VNumber (2^31))

    -- string operators
    describe "strings" $ do
      it "addition" $ do
        let program = EOp OStrPlus [(EString "a"), (EString "b")]
        eval scope program `shouldBe` Right (VString "ab")
      it "less than" $ do
        let program = EOp OStrLt [(EString "a"), (EString "b")]
        eval scope program `shouldBe` Right (VBool True)
      it "length" $ do
        let program = EOp OStrLen [(EString "abc")]
        eval scope program `shouldBe` Right (VNumber 3.0)
      it "starts with" $ do
        let program = EOp OStrStartsWith [(EString "abc"), (EString "a")]
        eval scope program `shouldBe` Right (VBool True)

    -- bool operators
    describe "bool" $ do
      it "and" $ do
        let program = EOp OBAnd [(EBool True), (EBool False)]
        eval scope program `shouldBe` Right (VBool False)
      it "or" $ do
        let program = EOp OBOr [(EBool True), (EBool False)]
        eval scope program `shouldBe` Right (VBool True)
      it "xor" $ do
        let program = EOp OBXOr [(EBool True), (EBool False)]
        eval scope program `shouldBe` Right (VBool True)
      it "not" $ do
        let program = EOp OBNot [(EBool True)]
        eval scope program `shouldBe` Right (VBool False)

    -- typeof
    describe "typeof" $ do
      it "number" $ do
        let program = EOp OTypeof [(ENumber 1.0)]
        eval scope program `shouldBe` Right (VString "number")
      it "string" $ do
        let program = EOp OTypeof [(EString "a")]
        eval scope program `shouldBe` Right (VString "string")
      it "bool" $ do
        let program = EOp OTypeof [(EBool True)]
        eval scope program `shouldBe` Right (VString "boolean")
      it "undefined" $ do
        let program = EOp OTypeof [(EUndefined)]
        eval scope program `shouldBe` Right (VString "undefined" )
      it "function" $ do
        let program = EOp OTypeof [(ELambda [] (ENumber 1.0))]
        eval scope program `shouldBe` Right (VString "function"  )
      it "object" $ do
        let program = EOp OTypeof [(EObject [])]
        eval scope program `shouldBe` Right (VString "object")
  
    -- is primitive
    describe "is primitive" $ do
      it "number" $ do
        let program = EOp OIsPrim [(ENumber 1.0)]
        eval scope program `shouldBe` Right (VBool True)
      it "string" $ do
        let program = EOp OIsPrim [(EString "a")]
        eval scope program `shouldBe` Right (VBool True)
      it "object" $ do
        let program = EOp OIsPrim [(EObject [])]
        eval scope program `shouldBe` Right (VBool False)

    -- primitive to number
    describe "to number conversions" $ do
      it "number" $ do
        let program = EOp OPrimToNum [(ENumber 1.0)]
        eval scope program `shouldBe` Right (VNumber 1.0)
      it "string" $ do
        let program = EOp OPrimToNum [(EString "1.0")]
        eval scope program `shouldBe` Right (VNumber 1.0)
      it "null" $ do
        let program = EOp OPrimToNum [(ENull)]
        eval scope program `shouldBe` Right (VNumber 0.0)
      it "undefined" $ do
        let program = EOp OPrimToNum [(EUndefined)]
        eval scope program `shouldSatisfy` (\a -> a /= a)
    
    -- primitive to string
    describe "to string conversions" $ do
      it "number" $ do
        let program = EOp OPrimToStr [(ENumber 1.0)]
        eval scope program `shouldBe` Right (VString "1.0")
      it "string" $ do
        let program = EOp OPrimToStr [(EString "1.0")]
        eval scope program `shouldBe` Right (VString "1.0")
      it "null" $ do
        let program = EOp OPrimToStr [(ENull)]
        eval scope program `shouldBe` Right (VString "null")
      it "undefined" $ do
        let program = EOp OPrimToStr [(EUndefined)]
        eval scope program `shouldBe` Right (VString "undefined")
      it "object" $ do
        let program = EOp OPrimToStr [(EObject [])]
        eval scope program `shouldBe` Right (VString "object")

    -- primitive to bool
    describe "to bool conversions" $ do
      it "number" $ do
        let program = EOp OPrimToBool [(ENumber 1.0)]
        eval scope program `shouldBe` Right (VBool True)
      it "nan" $ do
        let program = EOp OPrimToBool [(ENumber $ 0.0/0.0)]
        eval scope program `shouldBe` Right (VBool False)
      it "zero" $ do
        let program = EOp OPrimToBool [(ENumber 0.0)]
        eval scope program `shouldBe` Right (VBool False)
      it "string" $ do
        let program = EOp OPrimToBool [(EString "1.0")]
        eval scope program `shouldBe` Right (VBool True)
      it "empty string" $ do
        let program = EOp OPrimToBool [(EString "")]
        eval scope program `shouldBe` Right (VBool False)
      it "null" $ do
        let program = EOp OPrimToBool [(ENull)]
        eval scope program `shouldBe` Right (VBool False)
      it "undefined" $ do
        let program = EOp OPrimToBool [(EUndefined)]
        eval scope program `shouldBe` Right (VBool False)
      it "object" $ do
        let program = EOp OPrimToBool [(EObject [])]
        eval scope program `shouldBe` Right (VBool True)

    -- shifts
    describe "shifts" $ do
      it "left shift" $ do
        let program = EOp OLShift [(ENumber 2.0), (ENumber 2.0)]
        eval scope program `shouldBe` Right (VNumber 8.0)
      it "neg sign preserving right shift" $ do
        let program = EOp OSpRShift [(ENumber (-2.0)), (ENumber 3.0)]
        eval scope program `shouldBe` Right (VNumber (-1.0))
      it "sign preserving right shift" $ do
        let program = EOp OSpRShift [(ENumber 8.0), (ENumber 2.0)]
        eval scope program `shouldBe` Right (VNumber 2.0)
      it "zero filling right shift" $ do
        let program = EOp OZfRShift [(ENumber (-2.0)), (ENumber 3.0)]
        eval scope program `shouldBe` Right (VNumber 536870911)
    
    -- strict equality
    describe "strict equality" $ do
      it "number" $ do
        let program = EOp OStrictEq [(ENumber 1.0), (ENumber 1.0)]
        eval scope program `shouldBe` Right (VBool True)
      it "number string" $ do
        let program = EOp OStrictEq [(ENumber 1.0), (EString "1.0")]
        eval scope program `shouldBe` Right (VBool False)
      it "number nan" $ do
        let program = EOp OStrictEq [(ENumber (0.0/0.0)), (ENumber (0.0/0.0))]
        eval scope program `shouldBe` Right (VBool False)

    -- abstract equality
    describe "abstract equality" $ do
      it "number" $ do
        let program = EOp OAbstractEq [(ENumber 1.0), (ENumber 1.0)]
        eval scope program `shouldBe` Right (VBool True)
      it "number nan" $ do
        let program = EOp OAbstractEq [(ENumber (0.0/0.0)), (ENumber (0.0/0.0))]
        eval scope program `shouldBe` Right (VBool False)
      it "number string" $ do
        let program = EOp OAbstractEq [(ENumber 1.0), (EString "1.0")]
        eval scope program `shouldBe` Right (VBool True)
      it "undefined null" $ do
        let program = EOp OAbstractEq [(ENull), (EUndefined)]
        eval scope program `shouldBe` Right (VBool True)
      it "bool number" $ do
        let program = EOp OAbstractEq [(EBool True), (ENumber 1.0)]
        eval scope program `shouldBe` Right (VBool True)
      it "bool number false" $ do
        let program = EOp OAbstractEq [(EBool True), (ENumber 0.0)]
        eval scope program `shouldBe` Right (VBool False)

    -- math functions
    describe "math functions" $ do
      it "exp" $ do
        let program = EOp OMathExp [(ENumber 3.5)]
        eval scope program `shouldBe` Right (VNumber 33.11545195869231)
      it "log" $ do
        let program = EOp OMathLog [(ENumber 3.5)]
        eval scope program `shouldBe` Right (VNumber 1.252762968495368)
      it "cos" $ do
        let program = EOp OMathCos [(ENumber 3.5)]
        eval scope program `shouldBe` Right (VNumber (-0.9364566872907963))
      it "sin" $ do
        let program = EOp OMathSin [(ENumber 3.5)]
        eval scope program `shouldBe` Right (VNumber (-0.35078322768961984))
      it "abs pos" $ do
        let program = EOp OMathAbs [(ENumber 3.5)]
        eval scope program `shouldBe` Right (VNumber 3.5)
      it "abs neg" $ do
        let program = EOp OMathAbs [(ENumber (-3.5))]
        eval scope program `shouldBe` Right (VNumber 3.5)
      it "pow" $ do
        let program = EOp OMathPow [(ENumber 3.5), (ENumber 4.2)]
        eval scope program `shouldBe` Right (VNumber 192.79056951583615)

    -- object functions
    describe "object functions" $ do
      it "has own prop" $ do
        let program = EOp OHasOwnProp [(EObject [("a", ENumber 1.0)]), (EString "a")]
        eval scope program `shouldBe` Right (VBool True)
      it "has not own prop" $ do
        let program = EOp OHasOwnProp [(EObject [("a", ENumber 1.0)]), (EString "b")]
        eval scope program `shouldBe` Right (VBool False)
      it "get field" $ do
        let program = EGetField (EObject [("a", ENumber 1.0), ("b", ENumber 2.0)]) (EString "a")
        eval scope program `shouldBe` Right (VNumber 1.0)
      it "get field proto" $ do
        let program = EGetField (EObject [("a", ENumber 1.0), ("b", ENumber 2.0), ("__proto__", (EObject [("c", ENumber 3.0)]))]) (EString "c")
        eval scope program `shouldBe` Right (VNumber 3.0)
      it "update field" $ do
        let program = EGetField (EUpdateField (EObject [("a", ENumber 1.0), ("b", ENumber 2.0)]) (EString "a") (ENumber 3.0)) (EString "a")
        eval scope program `shouldBe` Right (VNumber 3.0)
      it "delete field" $ do
        let program = EGetField (EDeleteField (EObject [("a", ENumber 1.0), ("b", ENumber 2.0)]) (EString "a")) (EString "a")
        eval scope program `shouldBe` Right VUndefined
    
    -- lambda application
    describe "lambda application" $ do
      it "identity" $ do
        let program = EApp (ELambda ["a"] (EId "a")) [(ENumber 1.0)]
        eval scope program `shouldBe` Right (VNumber 1.0)
      it "double" $ do
        let program = EApp (ELambda ["a"] (EOp ONumPlus [(EId "a"), (EId "a")])) [(ENumber 1.0)]
        eval scope program `shouldBe` Right (VNumber 2.0)

    -- let
    describe "let expression" $ do
      it "let single var" $ do
        let program = ELet [("a", ENumber 1.0)] (EId "a")
        eval scope program `shouldBe` Right (VNumber 1.0)
      it "let double var" $ do
        let program = ELet [("a", ENumber 1.0), ("b", ENumber 2.0)] (EOp ONumPlus [(EId "a"), (EId "b")])
        eval scope program `shouldBe` Right (VNumber 3.0)

    -- if
    describe "if expression" $ do
      it "if then branch" $ do
        let program = EIf (EBool True) (ENumber 1.0) (ENumber 2.0)
        eval scope program `shouldBe` Right (VNumber 1.0)
      it "if else branch" $ do
        let program = EIf (EBool False) (ENumber 1.0) (ENumber 2.0)
        eval scope program `shouldBe` Right (VNumber 2.0)
   
    -- seq
    describe "seq" $ do
      it "environment carry-over" $ do
        let program = (ELet [("a", EObject [("b", ENumber 1.0)])] (ESeq (EUpdateField (EId "a") (EString "b") (ENumber 2.0)) (EGetField (EId "a") (EString "b"))))
        eval scope program `shouldBe` Right (VNumber 2.0)

    -- ref
    describe "references" $ do
      it "set ref" $ do
        let program = (ELet [("a", ENumber 1.0)] (ESeq (ESetRef (EId "a") (ENumber 2.0)) (EId "a")))
        eval scope program `shouldBe` Right (VNumber 2.0)
    
    -- eval
    describe "eval" $ do
      it "abort" $ do
        let program = EEval
        eval scope program `shouldSatisfy` isLeft

    -- labels
    describe "labels" $ do
      it "caught label" $ do
        let program = ELabel "label1" (ESeq (EBreak "label1" (ENumber 1.0)) (EUndefined))
        eval scope program `shouldBe` Right (VNumber 1.0)
      it "escape while" $ do
        let program = ELabel "label1" (EWhile (EOp OStrictEq [(ENumber 1.0), (ENumber 1.0)]) (EBreak "label1" (ENumber 2.0)))
        eval scope program `shouldBe` Right (VNumber 2.0)

    -- throws
    describe "throws" $ do
      it "caught throws" $ do
        let program = ECatch (EThrow (ENumber 1.0)) (ELambda ["x"] (EId "x"))
        eval scope program `shouldBe` Right (VNumber 1.0)
  where scope = empty