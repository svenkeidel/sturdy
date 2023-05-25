{-# LANGUAGE OverloadedLists #-}

module ConstantPropagationUnitMemorySpec where

import qualified Concrete as Concrete
--import qualified Data as D
import           ConstantPropagationUnitMemory as A
import           ConstantPropagationValue as A
import           UnitAnalysisValue as U
import           ConstantPropagationSoundness
--import           GraphToDot

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Abstract.Except as Exc
import           Data.Abstract.MonotoneErrors(toSet)
import           Data.Abstract.Terminating
import qualified Data.HashSet as HashSet
import           Data.Text.Lazy (pack)

import           Language.Wasm
import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Structure (Function(..), Expression)

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

runFunc :: String -> String -> [A.Value] -> IO Result
runFunc modName funcName args = do
    m <- readModule ("test/samples/" ++ modName ++ ".wast")
    Right (modInst, staticS, tabs) <- instantiateAbstract m
    return $ invokeExported staticS tabs modInst (pack funcName) args

checkSoundness :: String -> String -> [[Concrete.Value]] -> IO Bool
checkSoundness m f args = do
    validMod <- readModule ("test/samples/" ++ m ++ ".wast")
    isSoundlyAbstracted validMod f args

succResult :: Result -> [A.Value]
succResult (_,(_,(Terminating (_,(_,(Exc.Success (_,result))))))) = result
succResult res = error $ "unsuccessful result " ++ show res

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

minialpha :: A.Value -> A.Value
minialpha = A.NotConstant . A.asNotConstant

spec :: Spec
spec = do

    -- CONSTANT VALUES

    it "run noop" $ do
        result <- runFunc "simple" "noop" []
        succResult result `shouldBe` [A.constantValue $ Wasm.VI32 0]

    it "run const" $ do
        result <- runFunc "simple" "const" [A.constantValue $ Wasm.VI32 5]
        succResult result `shouldBe` [A.constantValue $ Wasm.VI32 5]

    it "run fac-rec" $ do
        let params = map (singleton . A.constantValue . Wasm.VI64) [0 .. 8]
        results <- mapM (runFunc "fact" "fac-rec") params
        let rs = map succResult results
        rs `shouldBe` map (singleton . A.constantValue . Wasm.VI64) [1,1,2,6,24,120,720,5040,40320]

    it "run fac-rec2" $ do
        result <- runFunc "fact" "fac-rec" [A.constantValue $ Wasm.VI64 25]
        succResult result `shouldBe` [A.constantValue $ Wasm.VI64 7034535277573963776]

    it "run fac-iter" $ do
        result <- runFunc "fact" "fac-iter" [A.constantValue $ Wasm.VI64 25]
        succResult result `shouldBe` [A.constantValue $ Wasm.VI64 7034535277573963776]

    it "run fac-rec-named" $ do
        result <- runFunc "fact" "fac-rec-named" [A.constantValue $ Wasm.VI64 25]
        succResult result `shouldBe` [A.constantValue $ Wasm.VI64 7034535277573963776]

    it "run fac-iter-named" $ do
        result <- runFunc "fact" "fac-iter-named" [A.constantValue $ Wasm.VI64 25]
        succResult result `shouldBe` [A.constantValue $ Wasm.VI64 7034535277573963776]

    it "run fac-opt" $ do
        result <- runFunc "fact" "fac-opt" [A.constantValue $ Wasm.VI64 25]
        succResult result `shouldBe` [A.constantValue $ Wasm.VI64 7034535277573963776]

    it "run test-mem" $ do
        -- result <- runFunc "simple" "test-mem" [A.constantValue $ Wasm.VI32 42]
        -- succResult result `shouldBe` [minialpha $ A.constantValue $ Wasm.VI32 43]
        result <- runFunc "simple" "test-mem" [A.constantValue $ Wasm.VI32 (2^(32::Int)-2)]
        succResult result `shouldBe` [minialpha $ A.constantValue $ Wasm.VI32 (2^(32::Int)-1)]

    it "run test-size" $ do
        result <- runFunc "simple" "test-size" []
        succResult result `shouldBe` [minialpha $ A.constantValue $ Wasm.VI32 1]

    it "run test-memgrow" $ do
        result <- runFunc "simple" "test-memgrow" []
        succResult result `shouldBe` [minialpha $ A.constantValue $ Wasm.VI32 1, minialpha $ A.constantValue $ Wasm.VI32 2]

    it "run test-call-indirect" $ do
        result <- runFunc "simple" "test-call-indirect" []
        succResult result `shouldBe` [A.constantValue $ Wasm.VI32 0]

    it "run first" $ do
        result <- runFunc "simple" "first" [A.constantValue $ Wasm.VI32 0, A.constantValue $ Wasm.VI32 1]
        succResult result `shouldBe` [A.constantValue $ Wasm.VI32 0]

    it "run call-first" $ do
        result <- runFunc "simple" "call-first" []
        succResult result `shouldBe` [A.constantValue $ Wasm.VI32 0]

    it "run nesting" $ do
        result <- runFunc "simple" "nesting" [A.constantValue $ Wasm.VF32 1, A.constantValue $ Wasm.VF32 2]
        succResult result `shouldBe` [A.constantValue $ Wasm.VF32 2]


    -- UNIT VALUES

    -- it "const: run fact" $ do
    --     result <- runFunc "fact" "fac-rec" [minialpha $ A.constantValue $ Wasm.VI64 1]
    --     result `shouldSatisfy` terminatedSucc
    --     (succResult result) `shouldBe` [minialpha $ A.constantValue $ Wasm.VI64 1]
    --     let args = [[Concrete.Value $ Wasm.VI64 1],[Concrete.Value $ Wasm.VI64 10]]
    --     checkSoundness "fact" "fac-rec" args `shouldReturn` True

    it "const: run test2" $ do
        checkSoundness "simple" "test2" [[]] `shouldReturn` True

    it "const: run test-br3" $ do
        let args = [[Concrete.Value $ Wasm.VI32 10]]
        checkSoundness "simple" "test-br3" args `shouldReturn` True

    it "const: run test-call-indirect" $ do
        checkSoundness "simple" "test-call-indirect" [[]] `shouldReturn` True

    it "const: run maybe-non-terminating" $ do
        let args = [[Concrete.Value $ Wasm.VI32 42]]
        checkSoundness "simple" "maybe-non-terminating" args `shouldReturn` True

    it "const: run test-unreachable" $ do
        checkSoundness "simple" "test-unreachable" [[]] `shouldReturn` True

    it "const: run test-unreachable2" $ do
        checkSoundness "simple" "test-unreachable2" [[]] `shouldReturn` True

    it "const: run test-unreachable3" $ do
        checkSoundness "simple" "test-unreachable3" [[]] `shouldReturn` True

    it "const: run test-unreachable4" $ do
        checkSoundness "simple" "test-unreachable4" [[]] `shouldReturn` True

    it "const: run test-unreachable5" $ do
        checkSoundness "simple" "test-unreachable4" [[]] `shouldReturn` True

    it "const: run test-br-and-return" $ do
        let args = [[Concrete.Value $ Wasm.VI32 10]]
        checkSoundness "simple" "test-br-and-return" args `shouldReturn` True

    it "const: run test-br-and-return2" $ do
        let args = [[Concrete.Value $ Wasm.VI32 10]]
        checkSoundness "simple" "test-br-and-return2" args `shouldReturn` True

    it "const: run test-br-and-return3" $ do
        let args = [[Concrete.Value $ Wasm.VI32 10]]
        checkSoundness "simple" "test-br-and-return3" args `shouldReturn` True


  where
    singleton :: a -> [a]
    singleton x = [x]



--    it "print cfg" $ do
--        result <- runFunc "fact" "fac-rec" [A.constantValue $ VI64 ()]
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
