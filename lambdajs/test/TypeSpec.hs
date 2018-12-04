module TypeSpec where

import           Control.Arrow.Fail
import           Data.Abstract.Environment
import           Data.Abstract.HandleError
import           Data.Concrete.Environment
import           Syntax
import           Test.Hspec
import           TypeSemantics             (TypeArr)
import qualified TypeSemantics             as Interpreter (runType)
import qualified SharedInterpreter as SI (eval)
import           Data.Either               (isLeft)
import           Data.Fixed                (mod')
import           Data.Set
import Data.Abstract.Powerset
import           Data.Abstract.Terminating
import Data.Abstract.FiniteMap

main :: IO ()
main = hspec spec

eval :: [(Ident, Type)] -> Expr -> Either String Type'
eval env e = case Interpreter.runType SI.eval env e of
    Terminating (loc, (st, Fail s)) -> Left s
    Terminating (loc, (st, Success s)) -> Right s
    NonTerminating -> Left "..."

spec :: Spec
spec = do
    describe "literals" $ do
        it "number literals" $ do
            let program = ENumber 2.0
            eval store program `shouldBe` Right (Data.Abstract.Powerset.fromFoldable [TNumber])
        it "boolean literals" $ do
            let program = EBool True
            eval store program `shouldBe` Right (Data.Abstract.Powerset.fromFoldable [TBool])
        it "string literals" $ do
            let program = EString "test"
            eval store program `shouldBe` Right (Data.Abstract.Powerset.fromFoldable [TString])
        it "undefined literal" $ do
            eval store EUndefined `shouldBe` Right (Data.Abstract.Powerset.fromFoldable [TUndefined])
        it "null literal" $ do
            eval store ENull `shouldBe` Right (Data.Abstract.Powerset.fromFoldable [TNull])
        it "lambda literal" $ do
            eval store (ELambda [] (ENumber 1.0)) `shouldBe` Right (Data.Abstract.Powerset.fromFoldable [(TLambda [] (ENumber 1.0) Data.Abstract.FiniteMap.empty)])
    describe "unions" $ do
        it "if" $ do
            let program = EIf (EBool True) (ENumber 1.0) (EString "a")
            eval store program `shouldBe` Right (Data.Abstract.Powerset.fromFoldable [TNumber, TString])
    where store = []
