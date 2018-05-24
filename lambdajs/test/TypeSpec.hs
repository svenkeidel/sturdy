module TypeSpec where

import Syntax
import SharedInterpreter (LJSArrow)
import qualified SharedInterpreter as Interpreter (runAbstract)
import Data.Concrete.Environment
import Data.Concrete.Error
import Control.Arrow.Fail
import Test.Hspec

import Data.Fixed (mod')
import Data.Either (isLeft)

main :: IO ()
main = hspec spec

eval :: [(Ident, Location)] -> [(Location, Type)] -> Expr -> Either String Type
eval env st e = case Interpreter.runAbstract env st e of
  (st, Fail s) -> Left s
  (st, Success r) -> Right r

spec :: Spec
spec = do
   describe "literals" $ do
        it "number literals" $ do
            let program = ENumber 2.0
            eval scope store program `shouldBe` Right TNumber
        it "boolean literals" $ do
            let program = EBool True
            eval scope store program `shouldBe` Right TBool
        it "string literals" $ do
            let program = EString "test"
            eval scope store program `shouldBe` Right TString
        it "undefined literal" $ do
            eval scope store EUndefined `shouldBe` Right TUndefined
        it "null literal" $ do
            eval scope store ENull `shouldBe` Right TNull
        it "lambda literal" $ do
            eval scope store (ELambda [] (ENumber 1.0)) `shouldBe` Right (TLambda [] TNumber)
    where (scope, store) = ([], [])