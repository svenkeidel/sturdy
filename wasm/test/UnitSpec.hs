{-# LANGUAGE OverloadedLists #-}

module UnitSpec where

import           Abstract (BaseValue(..))
import qualified Concrete as Concrete
import qualified ConcreteInterpreter as Concrete
import           UnitAnalysis as U
import           UnitAnalysisValue
import           Soundness
import           GenericInterpreter(Exc(..))

--import           Control.Arrow.Transformer.Abstract.WasmFrame (Vector(..))
--import           Control.Arrow.Transformer.Abstract.Stack (AbsList(..))

import qualified Data.ByteString.Lazy as LBS
import           Data.Abstract.Error
import qualified Data.Abstract.Except as Exc
import           Data.Abstract.FreeCompletion
import           Data.Abstract.Terminating
import qualified Data.HashSet as HashSet
import           Data.List (isInfixOf)
import           Data.List.Singleton (singleton)
import qualified Data.Abstract.Powerset as Pow
import           Data.Order
import           Data.Text.Lazy (pack)
import           Data.Text.Prettyprint.Doc
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

term :: Terminating a -> a
term (Terminating a) = a

getFunctionBody (Function _ _ b) = b

readModule :: String -> IO ValidModule
readModule path = do
    content <- LBS.readFile path
    let Right m = parse content
    let Right validMod = validate m
    return validMod

runFunc :: String -> String -> [Value] -> IO Result
runFunc modName funcName args = do
    mod <- readModule ("test/samples/" ++ modName ++ ".wast")
    Right (modInst, staticS, tabs) <- instantiateAbstract mod
    return $ invokeExported staticS tabs modInst (pack funcName) args

succResult :: Result -> [Value]
succResult (Terminating (Success (_,(_,(Exc.Success (_,result)))))) = result

excResult :: Result -> Exc.Except (U.Exc Value) [Value]
excResult (Terminating (Success (_,(_,(Exc.Success (_,result)))))) = Exc.Success result
excResult (Terminating (Success (_,(_,(Exc.SuccessOrFail e (_,result)))))) = Exc.SuccessOrFail e result
excResult (Terminating (Success (_,(_,(Exc.Fail e))))) = Exc.Fail e


spec :: Spec
spec = do
    it "run fact" $ do
        result <- runFunc "fact" "fac-rec" [Value $ VI64 ()]
        (succResult result) `shouldBe` [Value $ VI64 ()]
--        validMod <- readModule "test/samples/fact.wast"
--        Right (modInst, store) <- instantiate validMod
--        let (Success (_,(_,(Exc.Success (_,result))))) = term $ invokeExported store modInst (pack "fac-rec") [Value $ Lower $ VI64 ()]
--        result `shouldBe` [Value $ Lower $ VI64 ()]
--        let args = [[Concrete.Value $ Wasm.VI64 1],[Concrete.Value $ Wasm.VI64 10]]
----        Right (concModInst, concStore) <- Concrete.instantiate validMod
----        Right (absModInst, absStore) <- instantiate validMod
----        let (AbsList absArgs) = foldr1 (⊔) $ (map (AbsList . map alphaVal1)) $ args
----        let concResults = map (snd . Concrete.invokeExported concStore concModInst (pack "fac-rec")) args
----        let (Terminating temp) = invokeExported absStore absModInst (pack "fac-rec") absArgs
----        --let absResult = resultToAbsList $ temp
----        putStrLn (show concResults)
----        putStrLn (show temp)
--        sound <- isSoundlyAbstracted validMod "fac-rec" args
--        sound `shouldBe` True

    it "run test2" $ do
        result <- runFunc "simple" "test2" []
        (succResult result) `shouldBe` [Value $ VI32 ()]


    it "run test-br3" $ do
        result <- runFunc "simple" "test-br3" [Value $ VI32 ()]
        (succResult result) `shouldBe` [Value $ VI32 ()]
--        let args = [[Concrete.Value $ Wasm.VI32 10]]
--        sound <- isSoundlyAbstracted validMod "test-br3" args
--        sound `shouldBe` True

    it "run test-call-indirect" $ do
        result <- runFunc "simple" "test-call-indirect" []
        (excResult result) `shouldSatisfy` (\x -> case x of
                                              (Exc.SuccessOrFail _ [Value (VI32 ())]) -> True
                                              _ -> False)

--    it "run non-terminating" $ do
--        validMod <- readModule "test/samples/simple.wast"
--        Right (modInst, store) <- instantiate validMod
--        let result = invokeExported store modInst (pack "non-terminating") []
--        result `shouldBe` NonTerminating
--
--    it "run maybe-non-terminating" $ do
--        validMod <- readModule "test/samples/simple.wast"
----        Right (modInst, store) <- instantiate validMod
----        let result = term $ invokeExported store modInst (pack "maybe-non-terminating") [Value $ Lower $ VI32 ()]
----        --result `shouldBe` [Value $ Lower $ VI32 ()]
----        result `shouldSatisfy` (const False)
--        let args = [[Concrete.Value $ Wasm.VI32 42]]
--        sound <- isSoundlyAbstracted validMod "maybe-non-terminating" args
--        sound `shouldBe` True
--
--    it "run noop" $ do
--        validMod <- readModule "test/samples/simple.wast"
--        Right (modInst, store) <- instantiate validMod
--        let (Success (locals,(Lower state,(Exc.Success (stack,result))))) = term $ invokeExported store modInst (pack "noop") []
--        locals `shouldBe` (Vector $ Vec.empty)
--        stack `shouldBe` []
--        result `shouldBe` [Value $ Lower $ VI32 ()]
--
--        let args = [[]]
--        sound <- isSoundlyAbstracted validMod "noop" args
--        sound `shouldBe` True
--
--    it "run test-br-and-return" $ do
--        let path = "test/samples/simple.wast"
--        content <- LBS.readFile path
--        let Right m = parse content
--        let Right validMod = validate m
--        Right (modInst, store) <- instantiate validMod
--        let ((Success (_,(_,(Exc.Success (_,result)))))) = term $ invokeExported store modInst (pack "test-br-and-return") [Value $ Lower $ VI32 ()]
--        result `shouldBe` [Value $ Lower $ VI32 ()]
--        let args = [[Concrete.Value $ Wasm.VI32 10]]
--        sound <- isSoundlyAbstracted validMod "test-br-and-return" args
--        sound `shouldBe` True
--
--    it "run test-unreachable" $ do
--        let path = "test/samples/simple.wast"
--        content <- LBS.readFile path
--        let Right m = parse content
--        let Right validMod = validate m
--        Right (modInst, store) <- instantiate validMod
--        let ((Success (_,(_,(Exc.Success (_, res)))))) = term $ invokeExported store modInst (pack "test-unreachable") []
--        res `shouldBe` [Value $ Lower $ VI32 ()]
--        let args = [[]]
--        sound <- isSoundlyAbstracted validMod "test-unreachable" args
--        sound `shouldBe` True
--
--    it "run test-unreachable2" $ do
--        let path = "test/samples/simple.wast"
--        content <- LBS.readFile path
--        let Right m = parse content
--        let Right validMod = validate m
--        Right (modInst, store) <- instantiate validMod
--        let ((Success (_,(_,(Exc.Success (_, res)))))) = term $ invokeExported store modInst (pack "test-unreachable2") []
--        res `shouldBe` [Value $ Lower $ VI32 ()]
--        let args = [[]]
--        sound <- isSoundlyAbstracted validMod "test-unreachable2" args
--        sound `shouldBe` True
--
--    it "run test-unreachable3" $ do
--        let path = "test/samples/simple.wast"
--        content <- LBS.readFile path
--        let Right m = parse content
--        let Right validMod = validate m
--        Right (modInst, store) <- instantiate validMod
--        let ((Success (_,(_,(Exc.Success (_, res)))))) = term $ invokeExported store modInst (pack "test-unreachable3") []
--        res `shouldBe` [Value $ Lower $ VI32 ()]
--        let args = [[]]
--        sound <- isSoundlyAbstracted validMod "test-unreachable3" args
--        sound `shouldBe` True
--
--    it "run test-unreachable4" $ do
--        let path = "test/samples/simple.wast"
--        content <- LBS.readFile path
--        let Right m = parse content
--        let Right validMod = validate m
--        Right (modInst, store) <- instantiate validMod
--        let ((Success (_,(_,(Exc.Fail (Exc e)))))) = term $ invokeExported store modInst (pack "test-unreachable4") []
--        (HashSet.toList e) `shouldBe` [Trap "Execution of unreachable instruction"]
--        let args = [[]]
--        sound <- isSoundlyAbstracted validMod "test-unreachable4" args
--        sound `shouldBe` True
--
--    it "run test-unreachable5" $ do
--        let path = "test/samples/simple.wast"
--        content <- LBS.readFile path
--        let Right m = parse content
--        let Right validMod = validate m
--        Right (modInst, store) <- instantiate validMod
--        let ((Success (_,(_,(Exc.Success (_, res)))))) = term $ invokeExported store modInst (pack "test-unreachable5") [Value $ Lower $ VI32 ()]
--        res `shouldBe` [Value $ Lower $ VI32 ()]
--        let args = [[Concrete.Value $ Wasm.VI32 10]]
--        sound <- isSoundlyAbstracted validMod "test-unreachable5" args
--        sound `shouldBe` True
--        --(HashSet.toList e) `shouldBe` [Trap "Execution of unreachable instruction"]
--
--    it "run test-br-and-return3" $ do
--        let path = "test/samples/simple.wast"
--        content <- LBS.readFile path
--        let Right m = parse content
--        let Right validMod = validate m
--        Right (modInst, store) <- instantiate validMod
--        let ((Success (_,(_,(Exc.Success (_, res)))))) = term $ invokeExported store modInst (pack "test-br-and-return3") [Value $ Lower $ VI32 ()]
--        res `shouldBe` [Value $ Lower $ VI32 ()]
--        let args = [[Concrete.Value $ Wasm.VI32 10]]
--        sound <- isSoundlyAbstracted validMod "test-br-and-return3" args
--        sound `shouldBe` True
--
--    it "run test-br-and-return2" $ do
--        let path = "test/samples/simple.wast"
--        content <- LBS.readFile path
--        let Right m = parse content
--        let Right validMod = validate m
--        Right (modInst, store) <- instantiate validMod
--        let ((Success (_,(_,(Exc.Success (_, res))))))= term $ invokeExported store modInst (pack "test-br-and-return2") [Value $ Lower $ VI32 ()]
--        res `shouldBe` [Value $ Lower $ VI32 ()]
--        let args = [[Concrete.Value $ Wasm.VI32 10]]
--        sound <- isSoundlyAbstracted validMod "test-br-and-return2" args
--        sound `shouldBe` True
--        --result `shouldBe` [Value $ Lower $ VI32 ()]
----    it "run const" $ do
----        let path = "test/samples/simple.wast"
----        content <- LBS.readFile path
----        let Right m = parse content
----        let Right validMod = validate m
----        Right (modInst, store) <- instantiate validMod
----        let (_,(Success (_,(_,(Success (_,result)))))) = term $ invokeExported store modInst (pack "const") [Value $ Wasm.VI32 5]
----        result `shouldBe` [Value $ Wasm.VI32 5]
----
----    it "run half-fact" $ do
----        let path = "test/samples/simple.wast"
----        content <- LBS.readFile path
----        let Right m = parse content
----        let Right validMod = validate m
----        Right (modInst, store) <- instantiate validMod
----        let (_, (Success (_,(_,(Success (_,result1)))))) = term $ invokeExported store modInst (pack "half-fac") [Value $ Wasm.VI32 0]
----        result1 `shouldBe` [Value $ Wasm.VI32 1]
----
----    it "run half-fact-64" $ do
----        let path = "test/samples/simple.wast"
----        content <- LBS.readFile path
----        let Right m = parse content
----        let Right validMod = validate m
----        --let halfFac64Code = (getFunctionBody . (!! 3) . functions . getModule) validMod
----        --putStrLn $ show validMod
----        Right (modInst, store) <- instantiate validMod
----        let (_, (Success (_,(_,(Success (_,result1)))))) = term $ invokeExported store modInst (pack "half-fac-64") [Value $ Wasm.VI64 0]
----        result1 `shouldBe` [Value $ Wasm.VI64 1]
----        --mapM_ (putStrLn . show) halfFac64Code
----        --putStrLn ""
----        --mapM_ putStrLn (reverse logs)
----
----    it "run fac-rec" $ do
----        let path = "test/samples/fact.wast"
----        content <- LBS.readFile path
----        let Right m = parse content
----        let Right validMod = validate m
----        --let facCode = (getFunctionBody . (!! 0) . functions . getModule) validMod
----        --mapM_ (putStrLn . show) facCode
----        --putStrLn ""
----        Right (modInst, store) <- instantiate validMod
----        let params = map (singleton . Value . Wasm.VI64) [0 .. 8]
----        let results = map (term $ invokeExported store modInst (pack "fac-rec")) params
----        let rs = map (\(_,Success (_,(_,(Success (_,r))))) -> r) results
----        --let logs = zip [0 ..] ls
----        --mapM_ printLogs logs
----        rs `shouldBe` (map (singleton . Value . Wasm.VI64) [1,1,2,6,24,120,720,5040,40320])
----
----        --where
----        --    printLogs (n,ls) = do
----        --        putStrLn $ show n
----        --        mapM_ putStrLn ((reverse . filter (\x -> isInfixOf "before returning" x ||
----        --                                                 isInfixOf "readFunction" x ||
----        --                                                 isInfixOf "invoke" x)) ls)
----        --        putStrLn ""
----
----    it "run test-mem" $ do
----        let path = "test/samples/simple.wast"
----        content <- LBS.readFile path
----        let Right m = parse content
----        let Right validMod = validate m
----        Right (modInst, store) <- instantiate validMod
----        let (_, (Success (_,(_,(Success (_,result)))))) = term $ invokeExported store modInst (pack "test-mem") [Value $ Wasm.VI32 42]
----        result `shouldBe` [Value $ Wasm.VI32 43]
