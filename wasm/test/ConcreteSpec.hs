module ConcreteSpec where

import           Concrete
import           ConcreteInterpreter
import           GenericInterpreter(Exc(..))

import           Control.Arrow.Transformer.Concrete.GlobalState

import qualified Data.ByteString.Lazy as LBS
import           Data.Concrete.Error
import           Data.List (isInfixOf)
import           Data.List.Singleton (singleton)
import           Data.Text.Lazy (pack)
import           Data.Vector(fromList,empty)

import           Language.Wasm
import           Language.Wasm.Interpreter (ModuleInstance(..))
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure
import           Language.Wasm.Validate

import           Test.Hspec

main :: IO ()
main = hspec spec

getFunctionBody (Function _ _ b) = b

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
        let store = emptyGlobalState
        (fst $ evalVariableInst inst [] fd (fromList $ map (Value . Wasm.VI32) [5,8,7]) store) `shouldBe`
            [Value $ Wasm.VI32 8]
    it "evalVariableInst GetGlobal" $ do
        let inst = GetGlobal 1
        let store = emptyGlobalState{globalInstances = fromList $ map (\x -> GlobInst Mutable (Value $ Wasm.VI32 x)) [3,4,5]}
        let fd = (0, Wasm.emptyModInstance{globaladdrs = fromList [0,1,2]})
        (fst $ evalVariableInst inst [] fd empty store) `shouldBe` [Value $ Wasm.VI32 4]
    it "evalVariabelInst SetGlobal" $ do
        let inst = SetGlobal 1
        let store = emptyGlobalState{globalInstances = fromList $ map (\x -> GlobInst Mutable (Value $ Wasm.VI32 x)) [3,4,5]}
        let fd = (0, Wasm.emptyModInstance{globaladdrs = fromList [0,1,2]})
        let stack = [Value $ Wasm.VI32 6]
        (globalInstances $ fst $ snd $ snd $ evalVariableInst inst stack fd empty store) `shouldBe`
            (fromList $ map (\x -> GlobInst Mutable (Value $ Wasm.VI32 x)) [3,6,5])

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
        let (_,(Success (_,(_,(Success (_,result)))))) = invokeExported store modInst (pack "noop") []
        result `shouldBe` [Value $ Wasm.VI32 0]

    it "run const" $ do
        let path = "test/samples/simple.wast"
        content <- LBS.readFile path
        let Right m = parse content
        let Right validMod = validate m
        Right (modInst, store) <- instantiate validMod
        let (_,(Success (_,(_,(Success (_,result)))))) = invokeExported store modInst (pack "const") [Value $ Wasm.VI32 5]
        result `shouldBe` [Value $ Wasm.VI32 5]

    it "run half-fact" $ do
        let path = "test/samples/simple.wast"
        content <- LBS.readFile path
        let Right m = parse content
        let Right validMod = validate m
        Right (modInst, store) <- instantiate validMod
        let (_, (Success (_,(_,(Success (_,result1)))))) = invokeExported store modInst (pack "half-fac") [Value $ Wasm.VI32 0]
        result1 `shouldBe` [Value $ Wasm.VI32 1]

    it "run half-fact-64" $ do
        let path = "test/samples/simple.wast"
        content <- LBS.readFile path
        let Right m = parse content
        let Right validMod = validate m
        --let halfFac64Code = (getFunctionBody . (!! 3) . functions . getModule) validMod
        --putStrLn $ show validMod
        Right (modInst, store) <- instantiate validMod
        let (_, (Success (_,(_,(Success (_,result1)))))) = invokeExported store modInst (pack "half-fac-64") [Value $ Wasm.VI64 0]
        result1 `shouldBe` [Value $ Wasm.VI64 1]
        --mapM_ (putStrLn . show) halfFac64Code
        --putStrLn ""
        --mapM_ putStrLn (reverse logs)

    it "run fac-rec" $ do
        let path = "test/samples/fact.wast"
        content <- LBS.readFile path
        let Right m = parse content
        let Right validMod = validate m
        --let facCode = (getFunctionBody . (!! 0) . functions . getModule) validMod
        --mapM_ (putStrLn . show) facCode
        --putStrLn ""
        Right (modInst, store) <- instantiate validMod
        let params = map (singleton . Value . Wasm.VI64) [0 .. 8]
        let results = map (invokeExported store modInst (pack "fac-rec")) params
        let rs = map (\(_,Success (_,(_,(Success (_,r))))) -> r) results
        --let logs = zip [0 ..] ls
        --mapM_ printLogs logs
        rs `shouldBe` (map (singleton . Value . Wasm.VI64) [1,1,2,6,24,120,720,5040,40320])

        --where
        --    printLogs (n,ls) = do
        --        putStrLn $ show n
        --        mapM_ putStrLn ((reverse . filter (\x -> isInfixOf "before returning" x ||
        --                                                 isInfixOf "readFunction" x ||
        --                                                 isInfixOf "invoke" x)) ls)
        --        putStrLn ""

    it "run test-mem" $ do
        let path = "test/samples/simple.wast"
        content <- LBS.readFile path
        let Right m = parse content
        let Right validMod = validate m
        Right (modInst, store) <- instantiate validMod
        let (_, (Success (_,(_,(Success (_,result)))))) = invokeExported store modInst (pack "test-mem") [Value $ Wasm.VI32 42]
        result `shouldBe` [Value $ Wasm.VI32 43]
