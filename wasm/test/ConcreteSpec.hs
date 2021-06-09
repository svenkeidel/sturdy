module ConcreteSpec where

import           Concrete
import           ConcreteInterpreter
import           Data

import qualified Data.ByteString.Lazy as LBS
import           Data.Concrete.Error
import           Data.Either(isRight)
import           Data.List.Singleton (singleton)
import           Data.Text.Lazy (pack)

import           Language.Wasm
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Script (runScript')

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

runScriptInterp :: String -> IO ()
runScriptInterp scriptName = do
    let path = "test/samples/" ++ scriptName ++ ".wast"
    content <- LBS.readFile path
    let s = parseScript content
    s `shouldSatisfy` isRight
    let (Right script) = s
    let onAssert msg ass = putStrLn $ "Failed assert: " ++ msg ++ ". Assert: " ++ show ass
    () <- runScript' invokeExported' onAssert script
    return ()

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

    it "run fac-rec2" $ do
        result <- runFunc "fact" "fac-rec" [Value $ Wasm.VI64 25]
        succResult result `shouldBe` [Value $ Wasm.VI64 7034535277573963776]

    it "run fac-iter" $ do
        result <- runFunc "fact" "fac-iter" [Value $ Wasm.VI64 25]
        succResult result `shouldBe` [Value $ Wasm.VI64 7034535277573963776]

    it "run fac-rec-named" $ do
        result <- runFunc "fact" "fac-rec-named" [Value $ Wasm.VI64 25]
        succResult result `shouldBe` [Value $ Wasm.VI64 7034535277573963776]

    it "run fac-iter-named" $ do
        result <- runFunc "fact" "fac-iter-named" [Value $ Wasm.VI64 25]
        succResult result `shouldBe` [Value $ Wasm.VI64 7034535277573963776]

    it "run fac-opt" $ do
        result <- runFunc "fact" "fac-opt" [Value $ Wasm.VI64 25]
        succResult result `shouldBe` [Value $ Wasm.VI64 7034535277573963776]

--    it "run fac-ssa" $ do
--        result <- runFunc "fact" "fac-ssa" [Value $ Wasm.VI64 2]
--        --print result
--        succResult result `shouldBe` [Value $ Wasm.VI64 2]

    it "run test-mem" $ do
        result <- runFunc "simple" "test-mem" [Value $ Wasm.VI32 42]
        succResult result `shouldBe` [Value $ Wasm.VI32 43]
        result <- runFunc "simple" "test-mem" [Value $ Wasm.VI32 (2^32-2)]
        succResult result `shouldBe` [Value $ Wasm.VI32 (2^32-1)]

    it "run test-size" $ do
        result <- runFunc "simple" "test-size" []
        succResult result `shouldBe` [Value $ Wasm.VI32 1]

    it "run test-memgrow" $ do
        result <- runFunc "simple" "test-memgrow" []
        succResult result `shouldBe` [Value $ Wasm.VI32 1, Value $ Wasm.VI32 2]

    it "run test-call-indirect" $ do
        result <- runFunc "simple" "test-call-indirect" []
        succResult result `shouldBe` [Value $ Wasm.VI32 0]

--    it "run block script" $ do
--        () <- runScriptInterp "block"
--        () `shouldBe` ()
--
    it "run first" $ do
        result <- runFunc "simple" "first" [Value $ Wasm.VI32 0, Value $ Wasm.VI32 1]
        succResult result `shouldBe` [Value $ Wasm.VI32 0]

    it "run call-first" $ do
        result <- runFunc "simple" "call-first" []
        succResult result `shouldBe` [Value $ Wasm.VI32 0]

    it "run nesting" $ do
        result <- runFunc "simple" "nesting" [Value $ Wasm.VF32 1, Value $ Wasm.VF32 2]
        succResult result `shouldBe` [Value $ Wasm.VF32 2]

--    it "run params-break" $ do
--        result <- runFunc "simple" "params-break" []
--        succResult result `shouldBe` [Value $ Wasm.VI32 12]
--
--    it "run loop script" $ do
--        () <- runScriptInterp "loop"
--        () `shouldBe` ()
--
--    it "run fac script" $ do
--        () <- runScriptInterp "fac"
--        () `shouldBe` ()
