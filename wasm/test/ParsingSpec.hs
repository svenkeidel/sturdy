module ParsingSpec where

import qualified Data.ByteString.Lazy as LBS
import           Data.Text.Lazy (pack)


import           Language.Wasm
import           Language.Wasm.Structure
import           Language.Wasm.Interpreter

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    let path = "test/samples/fact.wast"
    it "parsing of webassembly module" $ do
        content <- LBS.readFile path
        let Right parsed = parse content
        length (functions parsed) `shouldBe` 7

    it "run haskell wasm interpreter" $ do
        content <- LBS.readFile path
        let Right m = parse content
        let Right validMod = validate m
        (Right modInst, store) <- instantiate emptyStore emptyImports validMod
        Just result <- invokeExport store modInst (pack "fac-rec") [VI64 2]
        result `shouldBe` [VI64 2]

    it "run script of haskell wasm interpeter" $ do
        content <- LBS.readFile "test/samples/fac.wast"
        let Right script = parseScript content
        let onAssert msg ass = putStrLn $ "Failed assert: " ++ msg ++ ". Assert: " ++ show ass
        () <- runScript onAssert script
        () `shouldBe` ()
