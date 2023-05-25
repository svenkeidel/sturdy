{-# LANGUAGE OverloadedLists #-}

module UnitSpec where

import           Abstract (BaseValue(..))
import qualified Concrete as Concrete
--import qualified Data as D
import           UnitAnalysis as U
import           UnitAnalysisValue as U
import           UnitSoundness
--import           GraphToDot

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Abstract.Except as Exc
import           Data.Abstract.MonotoneErrors(toSet)
import           Data.Abstract.Terminating
import qualified Data.HashSet as HashSet
import           Data.Text.Lazy (pack)

import           Language.Wasm
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure

--import           Numeric.Natural

import           Test.Hspec

main :: IO ()
main = hspec spec

term :: Terminating a -> a
term (Terminating a) = a
term _ = error "not defined"

getFunctionBody :: Function -> Expression
getFunctionBody (Function _ _ b) = b

readModule :: String -> IO ValidModule
readModule path = do
    content <- LBS.readFile path
    let Right m = parse content
    let Right validMod = validate m
    return validMod

runFunc :: String -> String -> [Value] -> IO Result
runFunc modName funcName args = do
    m <- readModule ("test/samples/" ++ modName ++ ".wast")
    Right (modInst, staticS, tabs) <- instantiateAbstract m
    return $ invokeExported staticS tabs modInst (pack funcName) args

checkSoundness :: String -> String -> [[Concrete.Value]] -> IO Bool
checkSoundness m f args = do
    validMod <- readModule ("test/samples/" ++ m ++ ".wast")
    isSoundlyAbstracted validMod f args

succResult :: Result -> [Value]
succResult (_,(_,(Terminating (_,(_,(Exc.Success (_,result))))))) = result
succResult _ = error "not defined"

terminatedSucc :: Result -> Bool
terminatedSucc (_, (errs, Terminating (_,(_,(Exc.Success _))))) = HashSet.null (toSet errs)
terminatedSucc (_, (errs, NonTerminating)) = HashSet.null (toSet errs)
terminatedSucc _ = False

terminatedErr :: Result -> Bool
terminatedErr (_,(errs, NonTerminating)) = not $ HashSet.null (toSet errs)
terminatedErr _ = False

terminatedMaybeErr :: Result -> Bool
terminatedMaybeErr (_,(errs, Terminating (_,(_,(Exc.Success _))))) = not $ HashSet.null (toSet errs)
terminatedMaybeErr _ = False

isNonTerminating :: Result -> Bool
isNonTerminating (_,(_, NonTerminating)) = True
isNonTerminating _ = False

spec :: Spec
spec = do
    it "run fact" $ do
        result <- runFunc "fact" "fac-rec" [Value $ VI64 ()]
        result `shouldSatisfy` terminatedSucc
        (succResult result) `shouldBe` [Value $ VI64 ()]
        let args = [[Concrete.Value $ Wasm.VI64 1],[Concrete.Value $ Wasm.VI64 10]]
        checkSoundness "fact" "fac-rec" args `shouldReturn` True

    it "run test2" $ do
        result <- runFunc "simple" "test2" []
        result `shouldSatisfy` terminatedSucc
        (succResult result) `shouldBe` [Value $ VI32 ()]
        checkSoundness "simple" "test2" [[]] `shouldReturn` True


    it "run test-br3" $ do
        result <- runFunc "simple" "test-br3" [Value $ VI32 ()]
        result `shouldSatisfy` terminatedSucc
        (succResult result) `shouldBe` [Value $ VI32 ()]
        let args = [[Concrete.Value $ Wasm.VI32 10]]
        checkSoundness "simple" "test-br3" args `shouldReturn` True

    it "run test-call-indirect" $ do
        result <- runFunc "simple" "test-call-indirect" []
        result `shouldSatisfy` terminatedMaybeErr
        (succResult result) `shouldBe` [Value $ VI32 ()]
        checkSoundness "simple" "test-call-indirect" [[]] `shouldReturn` True

    it "run non-terminating" $ do
        result <- runFunc "simple" "non-terminating" []
        result `shouldSatisfy` terminatedSucc
        result `shouldSatisfy` isNonTerminating

    it "run maybe-non-terminating" $ do
        let args = [[Concrete.Value $ Wasm.VI32 42]]
        checkSoundness "simple" "maybe-non-terminating" args `shouldReturn` True

    it "run test-unreachable" $ do
        result <- runFunc "simple" "test-unreachable" []
        (succResult result) `shouldBe` [Value $ VI32 ()]
        checkSoundness "simple" "test-unreachable" [[]] `shouldReturn` True

    it "run test-unreachable2" $ do
        checkSoundness "simple" "test-unreachable2" [[]] `shouldReturn` True

    it "run test-unreachable3" $ do
        checkSoundness "simple" "test-unreachable3" [[]] `shouldReturn` True

    it "run test-unreachable4" $ do
        checkSoundness "simple" "test-unreachable4" [[]] `shouldReturn` True

    it "run test-unreachable5" $ do
        checkSoundness "simple" "test-unreachable4" [[]] `shouldReturn` True

    it "run test-br-and-return" $ do
        let args = [[Concrete.Value $ Wasm.VI32 10]]
        checkSoundness "simple" "test-br-and-return" args `shouldReturn` True

    it "run test-br-and-return2" $ do
        let args = [[Concrete.Value $ Wasm.VI32 10]]
        checkSoundness "simple" "test-br-and-return2" args `shouldReturn` True

    it "run test-br-and-return3" $ do
        let args = [[Concrete.Value $ Wasm.VI32 10]]
        checkSoundness "simple" "test-br-and-return3" args `shouldReturn` True

--    it "print cfg" $ do
--        result <- runFunc "fact" "fac-rec" [Value $ VI64 ()]
--        let cfg = fst result
--        putStrLn (show cfg)
--        putStrLn $ graphToDot showForGraph cfg
--        pending
--
--
--showForGraph :: D.Instruction Natural -> String
--showForGraph (D.I32Const i _) = "I32Const " ++ (show i)
--showForGraph (D.I64Const i _) = "I64Const " ++ (show i)
--showForGraph (D.GetLocal n _) = "GetLocal " ++ (show n)
--showForGraph (D.IRelOp bs op _) = "IRelOp " ++ (show bs) ++ " " ++ (show op)
--showForGraph (D.IBinOp bs op _) = "IBinOp " ++ (show bs) ++ " " ++ (show op)
--showForGraph (D.Call i _) = "Call " ++ (show i)
--showForGraph (D.If t _ _ _) = "If " ++ (show t)
--showForGraph _ = error "todo"
