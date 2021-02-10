module ConcreteSpec where

import           ConcreteInterpreter
import           GenericInterpreter(Exc(..))

import qualified Data.ByteString.Lazy as LBS
import           Data.Concrete.Error
import           Data.Text.Lazy (pack)
import           Data.Vector(fromList,empty)

import           Language.Wasm
import           Language.Wasm.Interpreter (ModuleInstance(..))
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

    it "evalVariableInst GetLocal" $ do
        let inst = GetLocal 1
        let fd = (0, Wasm.emptyModInstance) 
        let store = emptyWasmStore
        (fst $ evalVariableInst inst [] fd (fromList $ map (Value . Wasm.VI32) [5,8,7]) store 0) `shouldBe`
            [Value $ Wasm.VI32 8]
    it "evalVariableInst GetGlobal" $ do
        let inst = GetGlobal 1
        let store = emptyWasmStore{globalInstances = fromList $ map (Value . Wasm.VI32) [3,4,5]}
        let fd = (0, Wasm.emptyModInstance{globaladdrs = fromList [0,1,2]})
        (fst $ evalVariableInst inst [] fd empty store 0) `shouldBe` [Value $ Wasm.VI32 4]
    it "evalVariabelInst SetGlobal" $ do
        let inst = SetGlobal 1
        let store = emptyWasmStore{globalInstances = fromList $ map (Value . Wasm.VI32) [3,4,5]}
        let fd = (0, Wasm.emptyModInstance{globaladdrs = fromList [0,1,2]})
        let stack = [Value $ Wasm.VI32 6]
        (globalInstances $ fst $ snd $ snd $ evalVariableInst inst stack fd empty store 0) `shouldBe`
            (fromList $ map (Value . Wasm.VI32) [3,6,5])

    it "evalParametricInst" $ do
        let inst = Drop
        let stack = map (Value . Wasm.VI32) [1,2,3]
        (fst $ evalParametricInst inst stack) `shouldBe` (map (Value . Wasm.VI32) [2,3])
        let inst = Select
        (fst $ evalParametricInst inst stack) `shouldBe` [Value $ Wasm.VI32 3]
        let stack = map (Value . Wasm.VI32) [0,1,2]
        (fst $ evalParametricInst inst stack) `shouldBe` [Value $ Wasm.VI32 1]

    it "run noop" $ do
        let path = "test/samples/simple.wast"
        content <- LBS.readFile path
        let Right m = parse content
        let Right validMod = validate m
        Right (modInst, store) <- instantiate validMod
        let (Success (_,(_,(Success (_,result))))) = invokeExported store modInst (pack "noop") []
        result `shouldBe` [Value $ Wasm.VI32 0]

    it "run fact" $ do
        let path = "test/samples/fact.wast"
        content <- LBS.readFile path
        let Right m = parse content
        let Right validMod = validate m
        Right (modInst, store) <- instantiate validMod
        let (Success (_,(_,(Success (_,result))))) = invokeExported store modInst (pack "fac-rec") [Value $ Wasm.VI64 0]
        result `shouldBe` [Value $ Wasm.VI64 1]

        --(length inst) `shouldBe` 0
--    let path = "test/samples/fact.wast"
--    it "parsing of webassembly module" $ do
--        content <- LBS.readFile path
--        let Right parsed = parse content
--        (length $ functions parsed) `shouldBe` 7
