{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module TypedAnalysisSpec where

import           Prelude hiding (succ,pred,id)

import           Control.Monad(forM_)
import           Control.Arrow.Transformer.Abstract.Fix.Metrics
import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow

import           Data.GraphViz hiding (diamond)
import           Data.Text(Text)
import           Data.HashSet(HashSet)

import           Data.Abstract.MonotoneErrors (toSet)
import qualified Data.Abstract.Boolean as B
import qualified Data.Abstract.Powerset as Pow

import           GHC.Exts

import           System.Directory

import           Syntax as S hiding (Nil)
import           Parser(loadSchemeFile)
import           TypedAnalysis
import           TypedAnalysis.Chaotic(evalInner',evalOuter')
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
      let inFile = "gabriel/boyer.scm"
      let expRes = successOrFail (return (BoolVal B.Top))
                                 [ "Excpeted list as argument for cdr, but got Top"
                                 , "Excpeted list as argument for car, but got Top"
                                 ]
      run inFile expRes

    it "browse" $ do
      let inFile = "gabriel/browse.scm"
      let expRes = successOrFail (return (NumVal IntVal))
                                 [ "error: (length): contract violation, expected list"
                                 , "Excpeted list as argument for cdr, but got Top"
                                 , "Excpeted list as argument for car, but got Top"
                                 , "expected a quote as argument for symbol->string, but got Top"
                                 ]
      run inFile expRes

    it "cpstak" $ do
      -- TIME: 105 | STATES: 120
      let inFile = "gabriel/cpstak.scm"
      let expRes = success (NumVal IntVal)
      run inFile expRes

    it "destruc" $ do
      let inFile = "gabriel/destruc.scm"
      let expRes = successOrFail (Pow.singleton $ (BoolVal B.Top))
                                 [ "Excpeted list as argument for cdr, but got Top"
                                 , "Excpeted list as argument for car, but got Top"
                                 ]
      run inFile expRes

    it "dderiv" $ do
      pendingWith "The analysis is too imprecise to typecheck. \
                  \The analysis tries to call a function, whose closure is top. \
                  \Continuing at this point would be unsound because the analysis\
                  \would not soundly approximate the control-flow of the program."
      let inFile = "gabriel/dderiv.scm"
      let expRes = success (BoolVal B.True)
      run inFile expRes

    it "deriv" $ do
--     => TIMEOUT | STATES: 1645737
      -- when (?algorithm == Parallel || ?algorithm == ADI) $
      --   pendingWith "out of memory"

      let inFile = "gabriel/deriv.scm"
      let expRes = successOrFail (return (BoolVal B.Top))
                                 -- because (equals? (list 1 2) (list 1 2)) recursively calls (equals? 1 1)
                                 [ "error: No derivation method available"
                                 , "error: Cannot map over a non-list"
                                 , "Excpeted list as argument for cdr, but got Top"
                                 , "Excpeted list as argument for car, but got Top"
                                 ]
      run inFile expRes

    it "diviter" $ do
--     => Final Values: Set(#f, {#f,#t})
--     => TIME: 163 | STATES: 175
      -- pendingWith "out of memory"
      let inFile = "gabriel/diviter.scm"
      -- let expRes = Pow.singleton $ (Success $ fromList [Bottom, BoolVal B.Top])
      let expRes = successOrFail (Pow.singleton $ (BoolVal B.Top)) []
      run inFile expRes

    it "divrec" $ do
--       => Final Values: Set(#f, {#f,#t})
--       => TIME: 59 | STATES: 219
      -- pendingWith "out of memory"
      let inFile = "gabriel/divrec.scm"
      -- let expRes = Pow.singleton $ (Success $ fromList [Bottom, BoolVal B.Top])
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
      let expRes = success $ NumVal IntVal
      run inFile expRes

    it "rsa" $ do
-- => Final Values: Set({#f,#t})
-- => TIME: 2831 | STATES: 247915
      -- pendingWith "only works for parallel, but parallel broken?"
      let inFile = "scala-am/rsa.scm"
      let expRes = successOrFail (return (BoolVal B.Top))
                                 [ "error: Not a legal public exponent for that modulus."
                                 , "error: The modulus is too small to encrypt the message."
                                 ]
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
      let expRes = successOrFail Pow.empty ["expected a two ints as arguments for quotient , but got [Top,Int]","expected a two ints as arguments for quotient , but got [True,string]"]
      run inFile expRes

    it "test_opvar_numbool" $ do
      let inFile = "test_opvar_numbool.scm"
      let expRes = successOrFail Pow.empty ["expected a numbers as argument for <, but got [string,string]"]
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
      let expRes = success $ NumVal FloatVal
      run inFile expRes

success :: Val -> (HashSet Text, Pow.Pow Val)
success v = ([],Pow.singleton v)

successOrFail :: Pow.Pow Val -> HashSet Text -> (HashSet Text, Pow.Pow Val)
successOrFail v errs = (errs, v)

type Runner = (String -> (HashSet Text, Pow.Pow Val) -> IO ())

metricFile :: String
metricFile = "metrics.csv"

runner :: (?algorithm :: Algorithm) => Eval' -> Runner
runner eval inFile expected = do
  prog <- loadSchemeFile inFile
  let ?sensitivity = 0
  let (cfg,(Monotone metric,(errs,res))) = eval [prog]
  let csv = printf "\"%s\",%s,%s\n" inFile (show ?algorithm) (toCSV metric)
  appendFile metricFile csv
  renderCFG inFile cfg
  (toSet errs, res) `shouldBe` expected

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

when :: IsList l => Bool -> l -> l
when True l = l
when False _ = fromList []
