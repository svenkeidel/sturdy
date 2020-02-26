{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module TypedAnalysisSpec where

import           Prelude hiding (succ,pred,id)

import           Data.GraphViz hiding (diamond)
import           Data.Graph.Inductive(Gr)
import           Data.HashSet (HashSet)
import           Data.Order (bottom)
import           Data.Abstract.Terminating hiding (toEither)
import           Data.Text(Text)

import           Test.Hspec

import           System.Process -- deprecated
import           System.Directory

import           LispTypes as LT hiding (Bool)
import           LispParser
import           LispToHask

import           TypedAnalysis

import           Syntax as S hiding (Nil)
import qualified Data.Abstract.Boolean as B
import           Control.Arrow.Transformer.Abstract.Fix.Metrics
import           Text.Printf

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "behavior specific to interval analysis" $ do
    it "test lits" $ do
      -- let ?sensitivity = 0 in evalInterval' [("x", singleton IntVal), ("y", singleton IntVal)] ["x"] `shouldBe` Terminating (Success $ singleton IntVal)
      let ?sensitivity = 0 in evalInterval' [] [define "x" (lit $ S.Number 2), "x"] `shouldBe` ([], Terminating IntVal)
      let ?sensitivity = 0 in evalInterval' [] [define "x" (lit $ S.Number 2),
                                                set "x" (lit $ S.Bool True),
                                                "x"] `shouldBe` success Top

    it "test closures" $ do
      let ?sensitivity = 0 in evalInterval' [] [app (lam ["x"] ["x"]) [lit $ S.Number 2]] `shouldBe` ([], Terminating IntVal)
      let ?sensitivity = 0 in evalInterval' [] [define "id" (lam ["x"] ["x"]),
                                                app "id" [lit $ S.Number 2]] `shouldBe` ([], Terminating IntVal)
      -- let ?sensitivity = 0 in evalInterval' [] [define "id" (lit $ S.Bool True),
      --                                           set "id" (lam ["x"] ["x"]),
      --                                           app "id" [lit $ S.Number 2]] `shouldBe` Terminating (Success IntVal)
      -- let ?sensitivity = 0 in evalInterval' [] [define "id" (lam ["x"] ["x"]),
      --                                           set "id" (lit $ S.Bool True),
      --                                           app "id" [lit $ S.Number 2]] `shouldBe` Terminating (Success IntVal)

    it "should analyze let expression" $
      let expr = [let_ [("x", lit $ S.Number 1)] ["x"]] in do
      let ?sensitivity = 0 in evalInterval' [] expr `shouldBe` success IntVal
      let ?sensitivity = 1 in evalInterval' [] expr `shouldBe` success IntVal

    it "should analyze define" $
      let exprs = [define "x" (lit $ S.Number 1),
                   set "x" (lit $ S.Number 2),
                   set "x" (lit $ S.Number 3),
                   "x"] in do
      let ?sensitivity = 0 in evalInterval' [] exprs `shouldBe` success IntVal
      let ?sensitivity = 2 in evalInterval' [] exprs `shouldBe` success IntVal

    it "should return unify two different types" $
      let exprs = [define "x" (lit $ S.Number 1),
                   set "x" (lit $ S.Number 2),
                   set "x" (lit $ S.Bool True),
                   "x"] in do
      let ?sensitivity = 0 in evalInterval' [] exprs `shouldBe` success Top
      let ?sensitivity = 2 in evalInterval' [] exprs `shouldBe` success Top


    it "should terminate for the non-terminating program LetRec" $
      let ?sensitivity = 0
      in evalInterval' [] [let_rec [("id", lam ["x"] ["x"]),
                                ("fix",lam ["x"] [app "fix" ["x"]])]
                         [app "fix" ["id"]]]
           `shouldBe` ([], NonTerminating)

    it "should terminate for the non-terminating program Define" $
      pending

  describe "Benchmarks" $ testFixpointAlgorithms benchmarks
  describe "Tests" $ testFixpointAlgorithms customTests

testFixpointAlgorithms :: (Runner -> Spec) -> Spec
testFixpointAlgorithms tests = do
  describe "Chaotic Iteration" $ do
    describe "Innermost" $ tests innermostRunner
    describe "Outermost" $ tests outermostRunner
  describe "Parallel Iteration" $ do
    describe "ADI" $ tests parallelADIRunner
    describe "Stack" $ tests parallelRunner

benchmarks :: Runner -> Spec
benchmarks run = do
  gabrielBenchmarks run
  scalaAM run

-----------------GABRIEL BENCHMARKS---------------------------------------------
gabrielBenchmarks :: Runner -> Spec
gabrielBenchmarks run = describe "Gabriel-Benchmarks" $ do
-- TIMEOUT = 30s

    it "boyer" $ do
      pendingWith "out of memory"
      let inFile = "gabriel//boyer"
      let expRes = success (BoolVal B.True)
      run inFile expRes

    it "cpstak" $ do
--     => Final Values: Set(Int)
--     => TIME: 105 | STATES: 120
      pendingWith "out of memory"
      let inFile = "gabriel//cpstak"
      let expRes = success IntVal
      run inFile expRes


    it "dderiv" $ do
      -- pendingWith "out of memory"
      let inFile = "gabriel//dderiv"
      let expRes = success (BoolVal B.True)
      run inFile expRes

    it "deriv" $ do
--     => TIMEOUT | STATES: 1645737
      pendingWith "out of memory"
      -- most likely wrong ?
      let inFile = "gabriel//deriv"
      let expRes = success (BoolVal B.True)
      run inFile expRes

    it "diviter" $ do
--     => Final Values: Set(#f, {#f,#t})
--     => TIME: 163 | STATES: 175
      -- pendingWith "out of memory"
      let inFile = "gabriel//diviter"
      -- let expRes = Terminating (Success $ fromList [Bottom, BoolVal B.Top])
      let expRes = success (BoolVal B.Top)
      run inFile expRes

    it "divrec" $ do
--       => Final Values: Set(#f, {#f,#t})
--       => TIME: 59 | STATES: 219
      -- pendingWith "out of memory"
      let inFile = "gabriel//divrec"
      -- let expRes = Terminating (Success $ fromList [Bottom, BoolVal B.Top])
      let expRes = success (BoolVal B.Top)
      run inFile expRes

    it "takl" $ do
-- => TIMEOUT | STATES: 1959438
      -- pendingWith "out of memory"
      let inFile = "gabriel//takl"
      -- let expRes = Terminating (Success $ fromList [BoolVal B.False, BoolVal B.Top, Bottom])
      let expRes = success (BoolVal B.Top)
      run inFile expRes

-- -------------------SCALA-AM BENCHMARKS------------------------------------------
scalaAM :: Runner -> Spec
scalaAM run = describe "Scala-AM-Benchmarks" $ do
    it "collatz" $ do
-- => Final Values: Set(Int)
-- => TIME: 8 | STATES: 431
      let inFile = "scala-am//collatz"
      let expRes = success IntVal
      run inFile expRes

    it "gcipd" $ do
--       => Final Values: Set(Int)
--       => TIME: 14 | STATES: 1098
      let inFile = "scala-am//gcipd"
      let expRes = success IntVal
      run inFile expRes

    it "nqueens" $ do
-- => TIMEOUT | STATES: 1781142
      let inFile = "scala-am//nqueens"
      let expRes = success IntVal
      run inFile expRes

    it "primtest" $ do
-- => Final Values: Set(Int)
-- => TIME: 8 | STATES: 431
      pendingWith "not getting parsed yet"
      let inFile = "scala-am//primtest"
      let expRes = success IntVal
      run inFile expRes

    it "rsa" $ do
-- => Final Values: Set({#f,#t})
-- => TIME: 2831 | STATES: 247915
      pendingWith "only works for parallel, but parallel broken?"
      let inFile = "scala-am//rsa"
      let expRes = successOrFail bottom ["Expected elements of type num for op| [List [Num],Num]" <> "Scheme-Error"]
      run inFile expRes

-- -------------------Custom Tests------------------------------------------
customTests :: Runner -> Spec
customTests run = do
  it "recursion_union_empty_list" $ do
    let inFile = "test_rec_empty"
    let expRes = success (ListVal Nil)
    run inFile expRes

  it "recursion and union with non-empty list" $ do
    let inFile = "test_rec_nonempty"
    let expRes = success IntVal
    run inFile expRes

  it "should return NV for (car (cdr '(2 3 4)))" $ do
    let inFile = "test_cdr"
    let expRes = success IntVal
    run inFile expRes

  it "should return correct val for car" $ do
    let inFile = "test_car"
    let expRes = success IntVal
    run inFile expRes

  it "test_null_cdr" $ do
    let inFile = "test_null"
    let expRes = success (BoolVal B.True)
    run inFile expRes

  it "unifying two list of nums of different size should result in list of nums" $ do
    let inFile = "test_faulty_list"
    let expRes = success Top
    run inFile expRes

  it "test_if" $ do
    let inFile = "test_if"
    let expRes = success (BoolVal B.False)
    run inFile expRes

  it "test_opvars" $ do
    let inFile = "test_opvars"
    let expRes = success IntVal
    run inFile expRes

  it "test_equal" $ do
    let inFile = "test_equal"
    let expRes = success (BoolVal B.True)
    run inFile expRes

  it "test_cons" $ do
    let inFile = "test_cons"
    let expRes = success (BoolVal B.True)
    run inFile expRes

  it "test_closure_gc" $ do
    let inFile = "test_closure_gc"
    let expRes = success IntVal
    run inFile expRes

  it "lang_scheme_test" $ do
    let inFile = "lang_scheme_test"
    let expRes = success IntVal
    run inFile expRes

  it "test_inner_define" $ do
    let inFile = "test_inner_define"
    let expRes = success IntVal
    run inFile expRes

  it "test_unops" $ do
    let inFile = "test_unops"
    let expRes = success (BoolVal B.Top)
    run inFile expRes

  it "test_eq" $ do
    let inFile = "test_eq"
    let expRes = success (BoolVal B.Top)
    run inFile expRes

  it "test_binops" $ do
    let inFile = "test_binops"
    let expRes = successOrFail NonTerminating ["expected a two ints as arguments for quotient , but got [Top,Int]","expected a two ints as arguments for quotient , but got [True,String]"]
    run inFile expRes

  it "test_opvar_numbool" $ do
    let inFile = "test_opvar_numbool"
    let expRes = success Bottom
    run inFile expRes

  it "test_opvar_numnum" $ do
    let inFile = "test_opvar_numnum"
    let expRes = success IntVal
    run inFile expRes

  it "test_opvar_boolbool" $ do
    let inFile = "test_opvar_boolbool"
    let expRes = success Top
    run inFile expRes

  it "test_list" $ do
    let inFile = "test_list"
    let expRes = success QuoteVal
    run inFile expRes

  it "test_factorial" $ do
    let inFile = "test_factorial"
    let expRes = success IntVal
    run inFile expRes

  it "test_random" $ do
    -- pending
    let inFile = "test_random"
    let expRes = success IntVal
    run inFile expRes

success :: Val -> (HashSet Text, Terminating Val)
success v = ([],Terminating v)

successOrFail :: Terminating Val -> HashSet Text -> (HashSet Text, Terminating Val)
successOrFail v errs = (errs, v)

-------------------HELPER------------------------------------------------------


type Runner = (String -> (HashSet Text, Terminating Val) -> IO ())

metricFile :: String
metricFile = "TypedAnalysis.csv"

innermostRunner :: Runner
innermostRunner inFile expRes = do
  file_str <- helper_import inFile
  case readExprList file_str of
    Right a ->
      case match a of
        Right b -> do
          let ?sensitivity = 0
          let (metric,res) = evalIntervalChaoticInner' [let_rec (getTopDefinesLam b) (getBody b)]
          let csv = printf "\"%s\",chaotic inner,%s\n" inFile (toCSV metric)
          appendFile metricFile csv
          res`shouldBe` expRes
        Left b -> print b
    Left a -> print $ showError a

outermostRunner :: Runner
outermostRunner inFile expRes = do
  file_str <- helper_import inFile
  case readExprList file_str of
    Right a ->
      case match a of
        Right b -> do
          let ?sensitivity = 0
          let (metric,res) = evalIntervalChaoticOuter' [let_rec (getTopDefinesLam b) (getBody b)]
          let csv = printf "\"%s\",chaotic outer,%s\n" inFile (toCSV metric)
          appendFile metricFile csv
          res`shouldBe` expRes
        Left b -> print b
    Left a -> print $ showError a

parallelRunner :: Runner
parallelRunner inFile expRes = do
  file_str <- helper_import inFile
  case readExprList file_str of
    Right a ->
      case match a of
        Right b -> do
          let ?sensitivity = 0
          let (metric,res) = evalIntervalParallel' [let_rec (getTopDefinesLam b) (getBody b)]
          let csv = printf "\"%s\",parallel,%s\n" inFile (toCSV metric)
          appendFile metricFile csv
          res`shouldBe` expRes
        Left b -> print b
    Left a -> print $ showError a

parallelADIRunner :: Runner
parallelADIRunner inFile expRes = do
  file_str <- helper_import inFile
  case readExprList file_str of
    Right a ->
      case match a of
        Right b -> do
          let ?sensitivity = 0
          let (metric,res) = evalIntervalParallelADI' [let_rec (getTopDefinesLam b) (getBody b)]
          let csv = printf "\"%s\",parallel,%s\n" inFile (toCSV metric)
          appendFile metricFile csv
          res`shouldBe` expRes
        Left b -> print b
    Left a -> print $ showError a


helper_import :: String -> IO String
helper_import inFile = do
  root <- getCurrentDirectory
  let root' = root ++ "//scheme_files//" ++  inFile ++ ".scm"
  readCreateProcess (shell $ "raco expand " ++ root') ""

draw_graph :: String -> Gr Expr () -> IO FilePath
draw_graph inFile graph = do 
  let dotGraph = graphToDot fileGraphParams graph
  root <- getCurrentDirectory 
  let outPath = root ++ "//graph_files//" ++ inFile ++ ".png"
  runGraphvizCommand Dot dotGraph Png outPath

-- visualize node labels (expr) as actual node labels
fileGraphParams :: GraphvizParams Int Expr () () Expr
fileGraphParams = defaultParams {fmtNode = \(_, vl) -> [toLabel vl]}