module TypeSpec where

import           Control.Arrow.Fail
import           Data.Abstract.DiscretePowerset as P
import           Data.Abstract.Environment      as E
import           Data.Abstract.FiniteMap
import           Data.Abstract.HandleError
import           Data.Abstract.Terminating
import           Data.Either                    (isLeft)
import           Data.Fixed                     (mod')
import qualified SharedInterpreter              as SI (eval)

import           Syntax
import           Test.Hspec
import           TypeSemantics                  (TypeArr)
import qualified TypeSemantics                  as Interpreter (runType)

main :: IO ()
main = hspec spec

eval :: [(Ident, Type)] -> Expr -> Either String Type'
eval env e = case Interpreter.runType SI.eval env e of
    Terminating (loc, (st, Fail s))    -> Left s
    Terminating (loc, (st, Success s)) -> Right s
    NonTerminating                     -> Left "nonterm"

spec :: Spec
spec = do
    describe "literals" $ do
        it "number literals" $ do
            let program = ENumber 2.0
            eval store program `shouldBe` Right (P.singleton TNumber)
        it "boolean literals" $ do
            let program = EBool True
            eval store program `shouldBe` Right (P.singleton TBool)
        it "string literals" $ do
            let program = EString "test"
            eval store program `shouldBe` Right (P.singleton TString)
        it "undefined literal" $ do
            eval store EUndefined `shouldBe` Right (P.singleton TUndefined)
        it "null literal" $ do
            eval store ENull `shouldBe` Right (P.singleton TNull)
        it "lambda literal" $ do
            eval store (ELambda [] (ENumber 1.0)) `shouldBe` Right (P.singleton (TLambda [] (ENumber 1.0) Data.Abstract.FiniteMap.empty))
    describe "unions" $ do
        it "if" $ do
            let program = EIf (EBool True) (ENumber 1.0) (EString "a")
            eval store program `shouldBe` Right (P.insert TNumber (P.singleton TString))
    describe "loop" $ do
        it "while" $ do
            let program = (ESeq (EWhile (EBool True) (ENull)) (ENumber 2.0))
            eval store program `shouldBe` Left "nonterm"
    where store = []
