{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module TypedAnalysisSpec where

import           Prelude hiding (succ,pred,id)

import           Control.Monad(when,forM_)
import           Control.Arrow.Transformer.Abstract.Fix.Metrics
import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow

import           Data.GraphViz hiding (diamond)
import           Data.HashSet (HashSet)
import           Data.Order (bottom)
import           Data.Text(Text)

import           Data.Abstract.Terminating hiding (toEither)
import qualified Data.Abstract.Boolean as B

import           System.Directory

import           Syntax as S hiding (Nil)
import           Parser(loadSchemeFile)
import           TypedAnalysis
import           TypedAnalysis.Chaotic(evalInner',evalOuter',eval')
import           TypedAnalysis.Parallel(evalParallel',evalADI')

import           Text.Printf

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  beforeAll (writeFile metricFile (printf "Function,Algorithm,%s\n" csvHeader)) $ do
    describe "Benchmarks" $ testFixpointAlgorithms benchmarks
    describe "Tests" $ testFixpointAlgorithms customTests

testFixpointAlgorithms :: ((?algorithm :: Algorithm) => Runner -> Spec) -> Spec
testFixpointAlgorithms tests = do
  describe "Chaotic" $ do
    describe "Innermost" $ let ?algorithm = ChaoticInner in tests (runner evalInner')
    describe "Outermost" $ let ?algorithm = ChaoticOuter in tests (runner evalOuter')
  describe "Parallel" $ let ?algorithm = Parallel in tests (runner evalParallel')
  describe "ADI" $ let ?algorithm = ADI in tests (runner evalADI')

benchmarks :: (?algorithm :: Algorithm) => Runner -> Spec
benchmarks run = do
  gabrielBenchmarks run
  scalaAM run

data Algorithm = ChaoticInner | ChaoticOuter | Parallel | ADI deriving (Show,Eq)

-----------------GABRIEL BENCHMARKS---------------------------------------------
gabrielBenchmarks :: (?algorithm :: Algorithm) => Runner -> Spec
gabrielBenchmarks run = describe "Gabriel" $ do
-- TIMEOUT = 30s

    it "boyer" $ do
      -- pendingWith "out of memory"
      let inFile = "gabriel/boyer.scm"
      let expRes = successOrFail (return (BoolVal B.Top))
                                 [ "Excpeted list as argument for cdr, but got Top"
                                 , "Excpeted list as argument for car, but got Top"
                                 ]
      run inFile expRes

    it "browse" $ do
      let inFile = "gabriel/browse.scm"
      let expRes = successOrFail (return (BoolVal B.Top))
                                 [ "Excpeted list as argument for cdr, but got Top"
                                 , "Excpeted list as argument for car, but got Top"
                                 ]
      run inFile expRes


    it "cpstak" $ do
--     => Final Values: Set(Int)
--     => TIME: 105 | STATES: 120
      let inFile = "gabriel/cpstak.scm"
      let expRes = success (NumVal IntVal)
      run inFile expRes


    it "dderiv" $ do
      let inFile = "gabriel/dderiv.scm"
      let expRes = success (BoolVal B.True)

      pendingWith "too imprecise"
      run inFile expRes

    it "deriv" $ do
--     => TIMEOUT | STATES: 1645737
      when (?algorithm == Parallel) $
        pendingWith "out of memory"

      let inFile = "gabriel/deriv.scm"
      let expRes = successOrFail (return (BoolVal B.Top))
                                 -- because (equals? (list 1 2) (list 1 2)) recursively calls (equals? 1 1)
                                 [ "error: String"
                                 , "Excpeted list as argument for cdr, but got Top"
                                 , "Excpeted list as argument for car, but got Top"
                                 ]
      run inFile expRes

    it "diviter" $ do
--     => Final Values: Set(#f, {#f,#t})
--     => TIME: 163 | STATES: 175
      -- pendingWith "out of memory"
      let inFile = "gabriel/diviter.scm"
      -- let expRes = Terminating (Success $ fromList [Bottom, BoolVal B.Top])
      let expRes = successOrFail (Terminating (BoolVal B.Top)) []
      run inFile expRes

    it "destruc" $ do
      let inFile = "gabriel/destruc.scm"
      let expRes = successOrFail (Terminating (BoolVal B.Top))
                                 [ "Excpeted list as argument for cdr, but got Top",
                                   "Excpeted list as argument for car, but got Top",
                                   "Empty program"
                                 ]
      run inFile expRes

    it "divrec" $ do
--       => Final Values: Set(#f, {#f,#t})
--       => TIME: 59 | STATES: 219
      -- pendingWith "out of memory"
      let inFile = "gabriel/divrec.scm"
      -- let expRes = Terminating (Success $ fromList [Bottom, BoolVal B.Top])
      let expRes = success (BoolVal B.Top)
      run inFile expRes

    it "takl" $ do
-- => TIMEOUT | STATES: 1959438
      let inFile = "gabriel/takl.scm"
      let expRes = successOrFail (return (BoolVal B.Top))
                                 -- because (equals? (list 1 2) (list 1 2)) recursively calls (equals? 1 1)
                                 [ "Excpeted list as argument for cdr, but got Top"
                                 , "Excpeted list as argument for car, but got Top"
                                 ]
      run inFile expRes

-- -------------------SCALA-AM BENCHMARKS------------------------------------------
scalaAM :: (?algorithm :: Algorithm) => Runner -> Spec
scalaAM run = describe "Scala-AM" $ do
    it "collatz" $ do
-- => Final Values: Set(Int)
-- => TIME: 8 | STATES: 431
      let inFile = "scala-am/collatz.scm"
      let expRes = success $ NumVal IntVal
      run inFile expRes

    it "gcipd" $ do
--       => Final Values: Set(Int)
--       => TIME: 14 | STATES: 1098
      let inFile = "scala-am/gcipd.scm"
      let expRes = success $ NumVal IntVal
      run inFile expRes

    it "nqueens" $ do
-- => TIMEOUT | STATES: 1781142
      let inFile = "scala-am/nqueens.scm"
      let expRes = success $ NumVal IntVal
      run inFile expRes

    it "primtest" $ do
-- => Final Values: Set(Int)
-- => TIME: 8 | STATES: 431
      -- pendingWith "not getting parsed yet"
      let inFile = "scala-am/primtest.scm"
      let expRes = successOrFail (return (NumVal IntVal))
                                 ["expected an integer as argument for random, but got NumTop"]
      run inFile expRes

    it "rsa" $ do
-- => Final Values: Set({#f,#t})
-- => TIME: 2831 | STATES: 247915
      -- pendingWith "only works for parallel, but parallel broken?"
      let inFile = "scala-am/rsa.scm"
      let expRes = successOrFail (return (BoolVal B.Top)) ["error: String"]
      run inFile expRes

-- -------------------Custom Tests------------------------------------------
customTests :: (?algorithm :: Algorithm) => Runner -> Spec
customTests run = do
    it "recursion_union_empty_list" $ do
      let inFile = "test_rec_empty.scm"
      let expRes = success (ListVal Nil)
      run inFile expRes

    it "recursion and union with non-empty list" $ do
      let inFile = "test_rec_nonempty.scm"
      let expRes = success $ NumVal IntVal
      run inFile expRes

    it "should return NV for (car (cdr '(2 3 4)))" $ do
      let inFile = "test_cdr.scm"
      let expRes = success $ NumVal IntVal
      run inFile expRes

    it "should return correct val for car" $ do
      let inFile = "test_car.scm"
      let expRes = success $ NumVal IntVal
      run inFile expRes

    it "test_null_cdr" $ do
      let inFile = "test_null.scm"
      let expRes = success (BoolVal B.True)
      run inFile expRes

    it "unifying two list of nums of different size should result in list of nums" $ do
      let inFile = "test_faulty_list.scm"
      let expRes = success Top
      run inFile expRes

    it "test_if" $ do
      let inFile = "test_if.scm"
      let expRes = success (BoolVal B.False)
      run inFile expRes

    it "test_opvars" $ do
      let inFile = "test_opvars.scm"
      let expRes = success $ NumVal IntVal
      run inFile expRes

    it "test_equal" $ do
      let inFile = "test_equal.scm"
      -- Higher sensitivity leads to BoolVal B.True
      let expRes = successOrFail (return (BoolVal B.Top)) ["Excpeted list as argument for cdr, but got Top","Excpeted list as argument for car, but got Top"]
      run inFile expRes

    it "test_cons" $ do
      let inFile = "test_cons.scm"
      let expRes = success (BoolVal B.True)
      run inFile expRes

    it "test_closure_gc" $ do
      let inFile = "test_closure_gc.scm"
      let expRes = success $ NumVal IntVal
      run inFile expRes

    it "lang_scheme_test" $ do
      let inFile = "lang_scheme_test.scm"
      let expRes = success $ NumVal IntVal
      run inFile expRes

    it "test_inner_define" $ do
      let inFile = "test_inner_define.scm"
      let expRes = success $ NumVal IntVal
      run inFile expRes

    it "test_unops" $ do
      let inFile = "test_unops.scm"
      let expRes = success (BoolVal B.Top)
      run inFile expRes

    it "test_eq" $ do
      let inFile = "test_eq.scm"
      let expRes = success (BoolVal B.Top)
      run inFile expRes

    it "test_binops" $ do
      let inFile = "test_binops.scm"
      let expRes = successOrFail NonTerminating ["expected a two ints as arguments for quotient , but got [Top,Int]","expected a two ints as arguments for quotient , but got [True,String]"]
      run inFile expRes

    it "test_opvar_numbool" $ do
      let inFile = "test_opvar_numbool.scm"
      let expRes = successOrFail NonTerminating ["expected a numbers as argument for <, but got [String,String]"]
      run inFile expRes

    it "test_opvar_numnum" $ do
      let inFile = "test_opvar_numnum.scm"
      let expRes = successOrFail (return Top) ["expected a numbers as argument for +, but got [Int,Top]"]
      run inFile expRes

    it "test_opvar_boolbool" $ do
      let inFile = "test_opvar_boolbool.scm"
      let expRes = success $ BoolVal B.True
      run inFile expRes

    it "test_list" $ do
      let inFile = "test_list.scm"
      let expRes = success $ QuoteVal ["+"]
      run inFile expRes

    it "test_factorial" $ do
      let inFile = "test_factorial.scm"
      let expRes = success $ NumVal IntVal
      run inFile expRes

    it "test_symbols" $ do
      let inFile = "test_symbols.scm"
      let expRes = success $ QuoteVal ["sym1", "sym2", "sym3"]
      run inFile expRes

    it "test_random" $ do
      let inFile = "test_random.scm"
      let expRes = success $ NumVal IntVal
      run inFile expRes

    it "test lits" $ do
      -- let ?sensitivity = 0 in evalInterval' [("x", singleton IntVal), ("y", singleton IntVal)] ["x"] `shouldBe` Terminating (Success $ singleton IntVal)
      let ?sensitivity = 0 in eval' [] [define "x" (lit $ S.Number 2), "x"] `shouldBe` ([], Terminating $ NumVal IntVal)
      let ?sensitivity = 0 in eval' [] [define "x" (lit $ S.Number 2),
                                        set "x" (lit $ S.Bool True),
                                        "x"] `shouldBe` success Top

    it "test closures" $ do
      let ?sensitivity = 0 in eval' [] [app (lam ["x"] ["x"]) [lit $ S.Number 2]] `shouldBe` ([], Terminating $ NumVal IntVal)
      let ?sensitivity = 0 in eval' [] [define "id" (lam ["x"] ["x"]),
                                        app "id" [lit $ S.Number 2]] `shouldBe` ([], Terminating $ NumVal IntVal)
      -- let ?sensitivity = 0 in evalInterval' [] [define "id" (lit $ S.Bool True),
      --                                           set "id" (lam ["x"] ["x"]),
      --                                           app "id" [lit $ S.Number 2]] `shouldBe` Terminating (Success IntVal)
      -- let ?sensitivity = 0 in evalInterval' [] [define "id" (lam ["x"] ["x"]),
      --                                           set "id" (lit $ S.Bool True),
      --                                           app "id" [lit $ S.Number 2]] `shouldBe` Terminating (Success IntVal)

    it "should analyze let expression" $
      let expr = [let_ [("x", lit $ S.Number 1)] ["x"]] in do
      let ?sensitivity = 0 in eval' [] expr `shouldBe` success (NumVal IntVal)
      let ?sensitivity = 1 in eval' [] expr `shouldBe` success (NumVal IntVal)

    it "should analyze define" $
      let exprs = [define "x" (lit $ S.Number 1),
                   set "x" (lit $ S.Number 2),
                   set "x" (lit $ S.Number 3),
                   "x"] in do
      let ?sensitivity = 0 in eval' [] exprs `shouldBe` success (NumVal IntVal)
      let ?sensitivity = 2 in eval' [] exprs `shouldBe` success (NumVal IntVal)

    it "should return unify two different types" $
      let exprs = [define "x" (lit $ S.Number 1),
                   set "x" (lit $ S.Number 2),
                   set "x" (lit $ S.Bool True),
                   "x"] in do
      let ?sensitivity = 0 in eval' [] exprs `shouldBe` success Top
      let ?sensitivity = 2 in eval' [] exprs `shouldBe` success Top


    it "should terminate for the non-terminating program LetRec" $
      let ?sensitivity = 0
      in eval' [] [let_rec [("id", lam ["x"] ["x"]),
                            ("fix",lam ["x"] [app "fix" ["x"]])]
                           [app "fix" ["id"]]]
           `shouldBe` ([], NonTerminating)
success :: Val -> (HashSet Text, Terminating Val)
success v = ([],Terminating v)

successOrFail :: Terminating Val -> HashSet Text -> (HashSet Text, Terminating Val)
successOrFail v errs = (errs, v)

type Runner = (String -> (HashSet Text, Terminating Val) -> IO ())

metricFile :: String
metricFile = "TypedAnalysis.csv"

runner :: (?algorithm :: Algorithm) => Eval' -> Runner
runner eval inFile expRes = do
  prog <- loadSchemeFile inFile
  let ?sensitivity = 0
  let (cfg,(metric,res)) = eval [prog]
  let csv = printf "\"%s\",%s,%s\n" inFile (show ?algorithm) (toCSV metric)
  appendFile metricFile csv
  renderCFG inFile cfg
  res`shouldBe` expRes

renderCFG :: String -> CFG Expr -> IO ()
renderCFG inFile (CFG graph) = do
  let dotGraph = graphToDot fileGraphParams graph
  root <- getCurrentDirectory
  forM_ (["gabriel", "scala-am"] :: [FilePath]) $ \dir ->
    createDirectoryIfMissing True (root ++ "/graph_files/" ++ dir)
  let outPath = root ++ "/graph_files/" ++ inFile ++ ".png"
  _ <- runGraphvizCommand Dot dotGraph Png outPath
  return ()

-- visualize node labels (expr) as actual node labels
fileGraphParams :: GraphvizParams Int Expr () () Expr
fileGraphParams = defaultParams {fmtNode = \(_, vl) -> [toLabel vl]}
