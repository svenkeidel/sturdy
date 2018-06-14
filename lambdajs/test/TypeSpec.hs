module TypeSpec where

import Syntax
import SharedAbstract (TypeArr)
import qualified SharedAbstract as Interpreter (runAbstract)
import Data.Concrete.Environment
import Data.Abstract.HandleError
import Control.Arrow.Fail
import Test.Hspec

import Data.Fixed (mod')
import Data.Either (isLeft)
import Data.Set

main :: IO ()
main = hspec spec

eval :: [(Ident, Location)] -> [(Location, Type)] -> Expr -> Either String Type'
eval env st e = case Interpreter.runAbstract env st e of
  (st, Fail s) -> Left s
  (st, Success r) -> Right r

spec :: Spec
spec = do
    describe "literals" $ do
        it "number literals" $ do
            let program = ENumber 2.0
            eval scope store program `shouldBe` Right (Data.Set.fromList [TNumber])
        it "boolean literals" $ do
            let program = EBool True
            eval scope store program `shouldBe` Right (Data.Set.fromList [TBool])
        it "string literals" $ do
            let program = EString "test"
            eval scope store program `shouldBe` Right (Data.Set.fromList [TString])
        it "undefined literal" $ do
            eval scope store EUndefined `shouldBe` Right (Data.Set.fromList [TUndefined])
        it "null literal" $ do
            eval scope store ENull `shouldBe` Right (Data.Set.fromList [TNull])
        it "lambda literal" $ do
            eval scope store (ELambda [] (ENumber 1.0)) `shouldBe` Right (Data.Set.fromList [(TLambda [] (Data.Set.fromList [TNumber]))])
    describe "unions" $ do
        it "if" $ do
            let program = EIf (EBool True) (ENumber 1.0) (EString "a")
            eval scope store program `shouldBe` Right (Data.Set.fromList [TNumber, TString])
    where (scope, store) = ([], [])