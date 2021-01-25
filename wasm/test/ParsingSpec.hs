module ParsingSpec where

import qualified Data.ByteString.Lazy as LBS

import           Language.Wasm
import           Language.Wasm.Structure

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    let path = "test/samples/fact.wast"
    it "parsing of webassembly module" $ do
        content <- LBS.readFile path
        let Right parsed = parse content
        (length $ functions parsed) `shouldBe` 7
