{-# LANGUAGE OverloadedLists #-}

module UnitSpec where

import           Abstract (BaseValue(..))
import           UnitAnalysis
import           GenericInterpreter(Exc(..))

import           Control.Arrow.Transformer.Abstract.WasmFrame (Vector(..))

import qualified Data.ByteString.Lazy as LBS
import           Data.Abstract.Error
import qualified Data.Abstract.Except as Exc
import           Data.Abstract.FreeCompletion
import           Data.List (isInfixOf)
import           Data.List.Singleton (singleton)
import qualified Data.Abstract.Powerset as Pow
import           Data.Text.Lazy (pack)
import           Data.Vector(fromList,empty)
import qualified Data.Vector as Vec

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
    it "run noop" $ do
        let path = "test/samples/simple.wast"
        content <- LBS.readFile path
        let Right m = parse content
        let Right validMod = validate m
        Right (modInst, store) <- instantiate validMod
        let (log, Success (locals,(Lower state,(Exc.Success (stack,result))))) = invokeExported store modInst (pack "noop") []
        locals `shouldBe` (Vector $ Vec.empty)
        stack `shouldBe` []
        result `shouldBe` [Value $ Lower $ VI32 ()]
        --result `shouldBe` [Value $ Wasm.VI32 0]

    it "run test-br3" $ do
        let path = "test/samples/simple.wast"
        content <- LBS.readFile path
        let Right m = parse content
        let Right validMod = validate m
        Right (modInst, store) <- instantiate validMod
        let (_, (Success (_,(_,(Exc.Success (_,result)))))) = invokeExported store modInst (pack "test-br3") [Value $ Lower $ VI32 ()]
        result `shouldBe` [Value $ Lower $ VI32 ()]
--    it "run const" $ do
--        let path = "test/samples/simple.wast"
--        content <- LBS.readFile path
--        let Right m = parse content
--        let Right validMod = validate m
--        Right (modInst, store) <- instantiate validMod
--        let (_,(Success (_,(_,(Success (_,result)))))) = invokeExported store modInst (pack "const") [Value $ Wasm.VI32 5]
--        result `shouldBe` [Value $ Wasm.VI32 5]
--
--    it "run half-fact" $ do
--        let path = "test/samples/simple.wast"
--        content <- LBS.readFile path
--        let Right m = parse content
--        let Right validMod = validate m
--        Right (modInst, store) <- instantiate validMod
--        let (_, (Success (_,(_,(Success (_,result1)))))) = invokeExported store modInst (pack "half-fac") [Value $ Wasm.VI32 0]
--        result1 `shouldBe` [Value $ Wasm.VI32 1]
--
--    it "run half-fact-64" $ do
--        let path = "test/samples/simple.wast"
--        content <- LBS.readFile path
--        let Right m = parse content
--        let Right validMod = validate m
--        --let halfFac64Code = (getFunctionBody . (!! 3) . functions . getModule) validMod
--        --putStrLn $ show validMod
--        Right (modInst, store) <- instantiate validMod
--        let (_, (Success (_,(_,(Success (_,result1)))))) = invokeExported store modInst (pack "half-fac-64") [Value $ Wasm.VI64 0]
--        result1 `shouldBe` [Value $ Wasm.VI64 1]
--        --mapM_ (putStrLn . show) halfFac64Code
--        --putStrLn ""
--        --mapM_ putStrLn (reverse logs)
--
--    it "run fac-rec" $ do
--        let path = "test/samples/fact.wast"
--        content <- LBS.readFile path
--        let Right m = parse content
--        let Right validMod = validate m
--        --let facCode = (getFunctionBody . (!! 0) . functions . getModule) validMod
--        --mapM_ (putStrLn . show) facCode
--        --putStrLn ""
--        Right (modInst, store) <- instantiate validMod
--        let params = map (singleton . Value . Wasm.VI64) [0 .. 8]
--        let results = map (invokeExported store modInst (pack "fac-rec")) params
--        let rs = map (\(_,Success (_,(_,(Success (_,r))))) -> r) results
--        --let logs = zip [0 ..] ls
--        --mapM_ printLogs logs
--        rs `shouldBe` (map (singleton . Value . Wasm.VI64) [1,1,2,6,24,120,720,5040,40320])
--
--        --where
--        --    printLogs (n,ls) = do
--        --        putStrLn $ show n
--        --        mapM_ putStrLn ((reverse . filter (\x -> isInfixOf "before returning" x ||
--        --                                                 isInfixOf "readFunction" x ||
--        --                                                 isInfixOf "invoke" x)) ls)
--        --        putStrLn ""
--
--    it "run test-mem" $ do
--        let path = "test/samples/simple.wast"
--        content <- LBS.readFile path
--        let Right m = parse content
--        let Right validMod = validate m
--        Right (modInst, store) <- instantiate validMod
--        let (_, (Success (_,(_,(Success (_,result)))))) = invokeExported store modInst (pack "test-mem") [Value $ Wasm.VI32 42]
--        result `shouldBe` [Value $ Wasm.VI32 43]
