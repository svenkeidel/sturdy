module ConcreteSpec where

import           ConcreteInterpreter
import           GenericInterpreter(Exc(..))

import qualified Data.ByteString.Lazy as LBS
import           Data.Concrete.Error
import           Data.Vector(fromList,empty)

import           Language.Wasm
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "evalNumericInst" $ do
        let inst = I32Const 5
        (evalNumericInst inst []) `shouldBe` (Success $ Value $ Wasm.VI32 5)
        let inst = IBinOp BS32 IAdd
        (evalNumericInst inst [Value $ Wasm.VI32 10, Value $ Wasm.VI32 1]) `shouldBe`
            (Success $ Value $ Wasm.VI32 11)

    it "evalVariableInst" $ do
        let inst = GetLocal 1
        let fd = (0, Wasm.emptyModInstance) 
        (fst $ evalVariableInst inst [] fd (fromList $ map (Value . Wasm.VI32) [5,8,7]) empty) `shouldBe`
            [Value $ Wasm.VI32 8]

        --(length inst) `shouldBe` 0
--    let path = "test/samples/fact.wast"
--    it "parsing of webassembly module" $ do
--        content <- LBS.readFile path
--        let Right parsed = parse content
--        (length $ functions parsed) `shouldBe` 7
