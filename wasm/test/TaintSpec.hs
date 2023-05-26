{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module TaintSpec where

import           Abstract
import           TaintAnalysis
import           TaintAnalysisValue (Taint(..))
import qualified TaintAnalysisValue as Taint
import qualified UnitAnalysisValue as U

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Abstract.Except as Exc
import           Data.Abstract.MonotoneErrors(toSet)
import           Data.Abstract.Terminating
import qualified Data.HashSet as HashSet
import           Data.Text.Lazy (pack)

import           Language.Wasm
import           Language.Wasm.Structure

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

runFunc :: String -> String -> [U.Value] -> IO Result
runFunc modName funcName args = do
    m <- readModule ("test/samples/" ++ modName ++ ".wast")
    Right (modInst, staticS, tabs) <- instantiateTaint m
    return $ invokeExported staticS tabs modInst (pack funcName) args

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

taintValue :: Taint -> BaseValue () () () () -> Value
taintValue t bv = Taint.Value t $ U.Value bv

spec :: Spec
spec = do
    it "run const" $ do
        result <- runFunc "simple" "const" [U.Value $ VI32 ()]
        result `shouldSatisfy` terminatedSucc
        (succResult result) `shouldBe` [taintValue Tainted (VI32 ())]

    it "run noop" $ do
        result <- runFunc "simple" "noop" []
        result `shouldSatisfy` terminatedSucc
        (succResult result) `shouldBe` [taintValue Untainted (VI32 ())]

    it "run test2" $ do
        result <- runFunc "simple" "test2" []
        result `shouldSatisfy` terminatedSucc
        (succResult result) `shouldBe` [taintValue Untainted (VI32 ())]

    it "test-mem" $ do
        result <- runFunc "simple" "test-mem" [U.Value $ VI32 ()]
        result `shouldSatisfy` terminatedMaybeErr
        (succResult result) `shouldBe` [taintValue Taint.Top (VI32 ())]

    it "test-mem2" $ do
        result <- runFunc "simple" "test-mem2" []
        result `shouldSatisfy` terminatedMaybeErr
        (succResult result) `shouldBe` [taintValue Taint.Top (VI32 ())]

    it "run fact" $ do
        result <- runFunc "fact" "fac-rec" [U.Value $ VI64 ()]
        result `shouldSatisfy` terminatedSucc
        (succResult result) `shouldBe` [taintValue Taint.Top (VI64 ())]

    it "run test-call-indirect" $ do
        result <- runFunc "simple" "test-call-indirect" []
        result `shouldSatisfy` terminatedMaybeErr
        (succResult result) `shouldBe` [taintValue Untainted (VI32 ())]
