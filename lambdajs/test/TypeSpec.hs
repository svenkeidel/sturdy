module TypeSpec where

import           Control.Arrow.Fail
import           Data.Abstract.Environment
import           Data.Abstract.HandleError
import           Data.Concrete.Environment
import           SharedAbstract            (TypeArr)
import qualified SharedAbstract            as Interpreter (runAbstract)
import           Syntax
import           Test.Hspec

import           Data.Either               (isLeft)
import           Data.Fixed                (mod')
import           Data.Set

main :: IO ()
main = hspec spec

eval :: [(Ident, Type)] -> [(Location, Type)] -> Expr -> Either String Type'
eval env st e = case Interpreter.runAbstract env st e of
  (st, Fail s)    -> Left s
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
            eval scope store (ELambda [] (ENumber 1.0)) `shouldBe` Right (Data.Set.fromList [(TLambda [] (ENumber 1.0) Data.Abstract.Environment.empty)])
    describe "unions" $ do
        it "if" $ do
            let program = EIf (EBool True) (ENumber 1.0) (EString "a")
            eval scope store program `shouldBe` Right (Data.Set.fromList [TNumber, TString])
    where (scope, store) = ([], [])
