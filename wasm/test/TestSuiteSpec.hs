module TestSuiteSpec where

import           ConcreteInterpreter

import           Test.Hspec

import qualified Data.ByteString.Lazy as LBS
import           Data.Either(isRight)
import qualified Data.List as List
import           Data.IORef

import           Language.Wasm
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Script (runScript')

import qualified System.Directory as Directory

initTests :: IO [String]
initTests = List.sort . filter p . filter (List.isSuffixOf ".wast") <$> Directory.listDirectory "test/spec"
    --return ["block.wast","br_if.wast","br_table.wast"]
    where p :: String -> Bool
          p s = not (any (\e -> List.isInfixOf e s) exclude)
exclude :: [String]
exclude = ["stack-guard","memory_grow", "float_exprs", "memory_trap", "imports"]

runTest :: String -> Spec
runTest filename =
    it filename $ do
        putStrLn $ "Running " ++ filename
        let path = "test/spec/" ++ filename
        content <- LBS.readFile path
        let s = parseScript content
        s `shouldSatisfy` isRight
        let (Right script) = s
        errors <- newIORef []
        let onAssert msg ass = modifyIORef' errors (("Failed assert: " ++ msg ++ ". Assert: " ++ show ass) :)
        () <- runScript' invokeExported' onAssert script
        readIORef errors `shouldReturn` []

spec :: Spec
spec = do
    fs <- runIO $ initTests
    describe "run testsuite" $ do
        mapM_ runTest fs

--    before initTests $ do
--        describe "run" $ do
--            it "test" $ \(fs,c) -> do
--                idx <- readIORef c
--                writeIORef c (idx + 1)
--                runTest (fs !! 5)
