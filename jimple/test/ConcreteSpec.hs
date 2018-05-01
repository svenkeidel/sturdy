module ConcreteSpec where

import Concrete
import Syntax

import qualified Data.Map as Store

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Literals" $ do
    it "LocalName lookup" $ do
      let expr = EImmediate (ILocalName "x")
      let st = Store.insert "x" (VInt 2) store
      eval' st expr `shouldBe` Right (st, VInt 2)
    it "Integer literals" $ do
      let expr = EImmediate (IInt 7)
      eval' store expr `shouldBe` Right (store, VInt 7)
    it "Float literals" $ do
      let expr = EImmediate (IFloat 2.5)
      eval' store expr `shouldBe` Right (store, VFloat 2.5)
    it "String literals" $ do
      let expr = EImmediate (IString "Hello World")
      eval' store expr `shouldBe` Right (store, VString "Hello World")
    it "Class literals" $ do
      let expr = EImmediate (IClass "java.lang.Object")
      eval' store expr `shouldBe` Right (store, VClass "java.lang.Object")
    it "Null literals" $ do
      let expr = EImmediate (INull)
      eval' store expr `shouldBe` Right (store, VNull)

  describe "Simple Expressions" $ do
    it "Addition" $ do
      let expr = EBinop (IInt 8) Plus (IInt 2)
      eval' store expr `shouldBe` Right (store, VInt 10)
    it "Division by zero" $ do
      let expr = EBinop (IInt 8) Div (IInt 0)
      eval' store expr `shouldBe` Left "Cannot divide by zero"

  where store = Store.empty
