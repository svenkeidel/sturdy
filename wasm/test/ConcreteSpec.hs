module ConcreteSpec where

import           Concrete
import           ConcreteInterpreter
import           Data

import qualified Data.ByteString.Lazy as LBS
import           Data.Concrete.Error
import           Data.List.Singleton (singleton)
import           Data.Text.Lazy (pack)

import           Language.Wasm
import qualified Language.Wasm.Interpreter as Wasm

import           Test.Hspec

main :: IO ()
main = hspec spec

getFunctionBody :: Function -> Expression
getFunctionBody (Function _ _ b) = b

runFunc :: String -> String -> [Value] -> IO Result
runFunc modName funcName args = do
    let path = "test/samples/" ++ modName ++ ".wast"
    content <- LBS.readFile path
    let Right m = parse content
    let Right validMod = validate m
    Right (modInst, staticS, mems, tabs) <- instantiateConcrete validMod
    return $ invokeExported staticS mems tabs modInst (pack funcName) args

succResult :: Result -> [Value]
succResult (Success (_,(_,(_,(_,Success (_,result)))))) = result
succResult _ = error "not defined"

spec :: Spec
spec = do
    it "run noop" $ do
        result <- runFunc "simple" "noop" []
        succResult result `shouldBe` [Value $ Wasm.VI32 0]

    it "run const" $ do
        result <- runFunc "simple" "const" [Value $ Wasm.VI32 5]
        succResult result `shouldBe` [Value $ Wasm.VI32 5]

    it "run fac-rec" $ do
        let params = map (singleton . Value . Wasm.VI64) [0 .. 8]
        results <- mapM (runFunc "fact" "fac-rec") params
        let rs = map succResult results
        rs `shouldBe` map (singleton . Value . Wasm.VI64) [1,1,2,6,24,120,720,5040,40320]

    it "run test-mem" $ do
        result <- runFunc "simple" "test-mem" [Value $ Wasm.VI32 42]
        succResult result `shouldBe` [Value $ Wasm.VI32 43]

    it "run test-call-indirect" $ do
        result <- runFunc "simple" "test-call-indirect" []
        succResult result `shouldBe` [Value $ Wasm.VI32 0]
