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

import           GHC.Exts

import           System.Directory

import           Syntax as S hiding (Nil)
import           Parser(loadSchemeFile)
import           TypedAnalysis
import           TypedAnalysis.Chaotic(evalInner',evalOuter')
import           TypedAnalysis.Parallel(evalParallel',evalADI')

import           Text.Printf

import           Test.Hspec

import           Data.Abstract.Powerset(Pow) 
import qualified Data.Abstract.Powerset as Pow 

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
    describe "Innermost" $ let ?algorithm = ChaoticInner in tests (runner_nm evalInner')
    describe "Outermost" $ let ?algorithm = ChaoticOuter in tests (runner_nm evalOuter')
  describe "Parallel" $ let ?algorithm = Parallel in tests (runner_nm evalParallel')
  describe "ADI" $ let ?algorithm = ADI in tests (runner_nm evalADI')

benchmarks :: (?algorithm :: Algorithm) => Runner -> Spec
benchmarks run = do
  gabrielBenchmarks run
  scalaAM run
  larceny run 

data Algorithm = ChaoticInner | ChaoticOuter | Parallel | ADI deriving (Show,Eq)

-----------------GABRIEL BENCHMARKS---------------------------------------------
gabrielBenchmarks :: (?algorithm :: Algorithm) => Runner -> Spec
gabrielBenchmarks run = describe "Gabriel" $ do

    it "boyer" $ do
      pendingWith "not terminating within 10 minutes, program might be too large"
      let inFile = "gabriel/boyer.scm"
      let expRes = successOrFail [BoolVal B.True, BoolVal B.False]
                                 [ "Expected list as argument for car, but got ['p]"
                                 , "Expected list as argument for cdr, but got ['u]"
                                 , "cannot cdr an empty list"
                                 , "Expected list as argument for cdr, but got ['e]"
                                 , "Expected list as argument for car, but got ['c]"
                                 , "Expected list as argument for car, but got ['x3]"
                                 , "Expected list as argument for car, but got ['val]"
                                 , "Expected list as argument for cdr, but got ['form]"
                                 , "Expected list as argument for car, but got ['pred]"
                                 , "Expected list as argument for cdr, but got ['f]"
                                 , "Expected list as argument for car, but got ['f]"
                                 , "Expected list as argument for car, but got ['x6]"
                                 , "Expected list as argument for cdr, but got ['val]"
                                 , "Expected list as argument for cdr, but got ['c]"
                                 , "Expected list as argument for cdr, but got ['l]"
                                 , "Expected list as argument for car, but got ['i]"
                                 , "Expected list as argument for cdr, but got ['x4]"
                                 , "Expected list as argument for car, but got ['y]"
                                 , "Expected list as argument for car, but got ['lessp]"
                                 , "Expected list as argument for cdr, but got ['equal]"
                                 , "Expected list as argument for car, but got ['l]"
                                 , "Expected list as argument for car, but got False"
                                 , "Expected list as argument for cdr, but got ['lessp]"
                                 , "Expected list as argument for car, but got ['equal]"
                                 , "Expected list as argument for cdr, but got ['x1]"
                                 , "Expected list as argument for cdr, but got ['y]"
                                 , "Expected list as argument for cdr, but got ['i]"
                                 , "Expected list as argument for cdr, but got ['x2]"
                                 , "Expected list as argument for cdr, but got ['z]"
                                 , "Expected list as argument for cdr, but got Int"
                                 , "Expected list as argument for cdr, but got ['j]"
                                 , "Expected list as argument for car, but got Int"
                                 , "Expected list as argument for car, but got ['x2]"
                                 , "Expected list as argument for cdr, but got ['w]"
                                 , "Expected list as argument for car, but got ['b]"
                                 , "Expected list as argument for cdr, but got ['pds]"
                                 , "Expected list as argument for cdr, but got ['implies]"
                                 , "Expected list as argument for cdr, but got ['p]"
                                 , "Expected list as argument for car, but got ['e]"
                                 , "Expected list as argument for car, but got ['x5]"
                                 , "cannot car an empty list"
                                 , "Expected list as argument for car, but got ['u]"
                                 , "Expected list as argument for car, but got ['base]"
                                 , "Expected list as argument for cdr, but got ['base]"
                                 , "Expected list as argument for cdr, but got ['x5]"
                                 , "Expected list as argument for car, but got ['x]"
                                 , "Expected list as argument for cdr, but got ['-]"
                                 , "Expected list as argument for cdr, but got ['var]"
                                 , "Expected list as argument for cdr, but got ['mem]"
                                 , "Expected list as argument for car, but got ['alist]"
                                 , "Expected list as argument for car, but got ['k]"
                                 , "Expected list as argument for cdr, but got ['x6]"
                                 , "Expected list as argument for car, but got ['and]"
                                 , "Expected list as argument for car, but got ['form]"
                                 , "Expected list as argument for cdr, but got ['x3]"
                                 , "Expected list as argument for cdr, but got ['k]"
                                 , "Expected list as argument for cdr, but got ['and]"
                                 , "Expected list as argument for cdr, but got ['pred]"
                                 , "Expected list as argument for cdr, but got ['alist]"
                                 , "Expected list as argument for car, but got ['q]"
                                 , "Expected list as argument for cdr, but got ['t]"
                                 , "Expected list as argument for cdr, but got ['d]"
                                 , "Expected list as argument for car, but got ['envrn]"
                                 , "Expected list as argument for car, but got ['a]"
                                 , "Expected list as argument for car, but got ['x1]"
                                 , "Expected list as argument for cdr, but got False"
                                 , "Expected list as argument for cdr, but got ['envrn]"
                                 , "Expected list as argument for cdr, but got ['a]"
                                 , "Expected list as argument for car, but got ['d]"
                                 , "Expected list as argument for car, but got ['x4]"
                                 , "Expected list as argument for car, but got ['t]"
                                 , "Expected list as argument for cdr, but got ['q]"
                                 , "Expected list as argument for car, but got ['pds]"
                                 , "Expected list as argument for car, but got ['implies]"
                                 , "Expected list as argument for cdr, but got ['b]"
                                 , "Expected list as argument for car, but got ['x7]"
                                 , "Expected list as argument for car, but got ['w]"
                                 , "Expected list as argument for car, but got ['z]"
                                 , "Expected list as argument for car, but got ['j]"
                                 , "Expected list as argument for cdr, but got ['x7]"
                                 , "Expected list as argument for car, but got ['mem]"
                                 , "Expected list as argument for car, but got ['var]"
                                 , "Expected list as argument for car, but got ['-]"
                                 , "Expected list as argument for cdr, but got ['x]" 
                                 ]
      run inFile expRes

    it "browse" $ do
      let inFile = "gabriel/browse.scm"
      let expRes = successOrFail [NumVal IntVal]
                                 [ "cannot cdr an empty list"
                                 , "Expected list as argument for car, but got False"
                                 , "Expected list as argument for car, but got ['b]"
                                 , "error: (length): contract violation, expected list"
                                 , "cannot car an empty list"
                                 , "Expected list as argument for car, but got ['a]"
                                 , "Expected list as argument for cdr, but got False"
                                 , "Expected list as argument for cdr, but got ['a]"
                                 , "Expected list as argument for cdr, but got ['b]"
                                 ]
      run inFile expRes

    it "cpstak" $ do
      let inFile = "gabriel/cpstak.scm"
      let expRes = success [NumVal IntVal]
      run inFile expRes

    it "destruc" $ do
      let inFile = "gabriel/destruc.scm"
      let expRes = successOrFail [BoolVal B.True, BoolVal B.False]
                                 $
                                 [ "cannot cdr an empty list"
                                 , "Expected list as argument for cdr, but got Int"
                                 , "Expected list as argument for car, but got Int"
                                 , "cannot car an empty list"
                                 ]
                                --  <>
                                --  when (?algorithm == Parallel || ?algorithm == ADI)
                                --  [ "cannot car an empty list" ]

      run inFile expRes

    it "dderiv" $ do
      pendingWith "The analysis is too imprecise to typecheck. \
                  \The analysis tries to call a function, whose closure is top. \
                  \Continuing at this point would be unsound because the analysis\
                  \would not soundly approximate the control-flow of the program."
      let inFile = "gabriel/dderiv.scm"
      let expRes = success [BoolVal B.True]
      run inFile expRes

    it "deriv" $ do
      -- when (?algorithm == Parallel || ?algorithm == ADI) $ 
      pendingWith "not terminating within 10 minutes, program might be too large"
      let inFile = "gabriel/deriv.scm"
      let expRes = successOrFail [BoolVal B.True, BoolVal B.False]
                                 [ "error: No derivation method available"
                                 , "cannot cdr an empty list"
                                 , "Expected list as argument for car, but got ['/]"
                                 , "Expected list as argument for cdr, but got ['*]"
                                 , "Expected list as argument for cdr, but got Int"
                                 , "Expected list as argument for car, but got Int"
                                 , "Expected list as argument for car, but got ['b]"
                                 , "cannot car an empty list"
                                 , "Expected list as argument for car, but got ['x]"
                                 , "Expected list as argument for car, but got ['+]"
                                 , "Expected list as argument for cdr, but got ['+]"
                                 , "Expected list as argument for car, but got ['a]"
                                 , "Expected list as argument for cdr, but got ['a]"
                                 , "error: Cannot map over a non-list"
                                 , "Expected list as argument for cdr, but got ['b]"
                                 , "Expected list as argument for cdr, but got ['/]"
                                 , "Expected list as argument for car, but got ['*]"
                                 , "Expected list as argument for cdr, but got ['x]"
                                 ]
      run inFile expRes

    it "diviter" $ do
      let inFile = "gabriel/diviter.scm"
      let expRes = successOrFail [BoolVal B.True, BoolVal B.False]
                                 [ "cannot cdr an empty list"
                                 , "cannot car an empty list"
                                 ]
      run inFile expRes

    it "divrec" $ do
      let inFile = "gabriel/divrec.scm"
      let expRes = successOrFail [BoolVal B.True, BoolVal B.False]
                                 [ "cannot cdr an empty list"
                                 , "cannot car an empty list"
                                 ]
      run inFile expRes

    it "takl" $ do
      let inFile = "gabriel/takl.scm"
      let expRes = successOrFail [BoolVal B.True, BoolVal B.False]
                                 [ "cannot cdr an empty list"
                                 , "Expected list as argument for cdr, but got Int"
                                 , "Expected list as argument for car, but got Int"
                                 , "cannot car an empty list"
                                 ]
      run inFile expRes

-- -------------------SCALA-AM BENCHMARKS------------------------------------------
scalaAM :: (?algorithm :: Algorithm) => Runner -> Spec
scalaAM run = describe "Scala-AM" $ do
    it "collatz" $ do
      let inFile = "scala-am/collatz.scm"
      let expRes = success [NumVal IntVal]
      run inFile expRes

    it "gcipd" $ do
      let inFile = "scala-am/gcipd.scm"
      let expRes = success [NumVal IntVal]
      run inFile expRes

    it "nqueens" $ do
      let inFile = "scala-am/nqueens.scm"
      let expRes = successOrFail [NumVal IntVal]
                                 [ "cannot cdr an empty list"
                                 , "cannot car an empty list"
                                 ]
      run inFile expRes

    it "primtest" $ do
      let inFile = "scala-am/primtest.scm"
      let expRes = success [NumVal IntVal]
      run inFile expRes

    it "rsa" $ do
      let inFile = "scala-am/rsa.scm"
      let expRes = successOrFail [BoolVal B.Top]
                                 [ "error: Not a legal public exponent for that modulus." 
                                 , "error: The modulus is too small to encrypt the message."
                                 ]
      run inFile expRes
----------------------LARCENY BENCHMAKRS------------------------------------
larceny :: (?algorithm :: Algorithm) => Runner -> Spec
larceny run = describe "Larceny" $ do
    it "ack" $ do
      let inFile = "larceny/ack.scm"
      let expRes = success [NumVal IntVal]
      run inFile expRes

    it "array1" $ do 
      pendingWith "missing implementations"
      let inFile = "larceny/array1.scm"
      let expRes = success [NumVal IntVal]
      run inFile expRes

    it "browse" $ do 
      pendingWith "missing implementations"
      let inFile = "larceny/browse.scm"
      let expRes = success [NumVal IntVal]
      run inFile expRes      

-- -------------------Custom Tests------------------------------------------
customTests :: (?algorithm :: Algorithm) => Runner -> Spec
customTests run = do
    it "test_rec_empty" $ do
      let inFile = "test_rec_empty.scm"
      let expRes = successOrFail [ListVal Nil]
                                 ["cannot cdr an empty list"]
      run inFile expRes

    it "test_factorial_letrec" $ do
      let inFile = "test_factorial_letrec.scm"
      let expRes = success [NumVal IntVal]
      run inFile expRes      

    it "test_rec_nonempty" $ do
      let inFile = "test_rec_nonempty.scm"
      let expRes = successOrFail [NumVal IntVal]
                                 ["cannot cdr an empty list"]
      run inFile expRes

    it "test_rec_defines" $ do
      let inFile = "test_rec_defines.scm"
      let expRes = success [NumVal IntVal]
      run inFile expRes      

    it "test_simple_floats" $ do
      let inFile = "test_simple_floats.scm"
      let expRes = success [BoolVal B.Top]
      run inFile expRes      

    it "test_simple_list" $ do
      let inFile = "test_simple_list.scm"
      let expRes = success [NumVal IntVal]
      run inFile expRes    

    it "test_subtraction" $ do
      let inFile = "test_subtraction.scm"
      let expRes = success [NumVal IntVal]
      run inFile expRes    

    it "test_cdr" $ do
      let inFile = "test_cdr.scm"
      let expRes = success $ [NumVal IntVal]
      run inFile expRes

    it "test_endless_nums" $ do
      let inFile = "test_endless_nums.scm"
      let expRes = success $ [NumVal IntVal]
      run inFile expRes

    it "test_endless_recursion" $ do
      let inFile = "test_endless_recursion.scm"
      let expRes = success []
      run inFile expRes
  
    it "test_car" $ do
      let inFile = "test_car.scm"
      let expRes = success [NumVal IntVal]
      run inFile expRes

    it "test_null_cdr" $ do
      let inFile = "test_null.scm"
      let expRes = success [BoolVal B.True]
      run inFile expRes

    it "test_faulty_list " $ do
      let inFile = "test_faulty_list.scm"
      let expRes = success [BoolVal B.False, NumVal IntVal]
      run inFile expRes

    it "test_if" $ do
      let inFile = "test_if.scm"
      let expRes = success [BoolVal B.False]
      run inFile expRes

    it "test_opvars" $ do
      let inFile = "test_opvars.scm"
      let expRes = success [NumVal IntVal]
      run inFile expRes

    it "test_equal" $ do
      let inFile = "test_equal.scm"
      -- Higher sensitivity leads to BoolVal B.True
      let expRes = successOrFail [BoolVal B.True, BoolVal B.False] 
                                 [ "cannot cdr an empty list"
                                 , "Expected list as argument for cdr, but got False"
                                 , "Expected list as argument for cdr, but got True"
                                 , "cannot car an empty list"
                                 , "Expected list as argument for car, but got False"
                                 , "Expected list as argument for car, but got True"
                                 ]
      run inFile expRes

    it "test_cons" $ do
      let inFile = "test_cons.scm"
      let expRes = success [BoolVal B.True]
      run inFile expRes

    it "test_closure_gc" $ do
      let inFile = "test_closure_gc.scm"
      let expRes = success $ [BoolVal B.False]
      run inFile expRes

    -- it "lang_scheme_test" $ do
    --   let inFile = "lang_scheme_test.scm"
    --   let expRes = success $ NumVal IntVal
    --   run inFile expRes

    it "test_inner_define" $ do
      let inFile = "test_inner_define.scm"
      let expRes = success [NumVal IntVal]
      run inFile expRes

    it "test_unops" $ do
      let inFile = "test_unops.scm"
      let expRes = success [BoolVal B.False, BoolVal B.True]
      run inFile expRes 

    it "test_eq" $ do
      let inFile = "test_eq.scm"
      let expRes = success [BoolVal B.True, BoolVal B.False]
      run inFile expRes

    it "test_binops" $ do
      let inFile = "test_binops.scm"
      let expRes = successOrFail [Bottom] ["expected a two ints as arguments for quotient , but got [string,Int]","expected a two ints as arguments for quotient , but got [True,string]"]
      run inFile expRes

    it "test_opvar_numbool" $ do
      let inFile = "test_opvar_numbool.scm"
      let expRes = successOrFail [Bottom] ["expected a numbers as argument for <, but got [string,string]"]
      run inFile expRes

    it "test_opvar_numnum" $ do
      let inFile = "test_opvar_numnum.scm"
      let expRes = successOrFail [Bottom, NumVal IntVal] ["expected a numbers as argument for +, but got [Int,string]"]
      run inFile expRes

    -- more precise than scalaAM's result ~ [StringVal, BoolVal B.False, BoolVal B.True]
    it "test_opvar_boolbool" $ do
      let inFile = "test_opvar_boolbool.scm"
      let expRes = success [BoolVal B.True]
      run inFile expRes

    it "test_list" $ do
      let inFile = "test_list.scm"
      let expRes = success [QuoteVal ["+"]]
      run inFile expRes

    it "test_factorial" $ do
      let inFile = "test_factorial.scm"
      let expRes = success [NumVal IntVal]
      run inFile expRes

    it "test_symbols" $ do
      let inFile = "test_symbols.scm"
      let expRes = success [QuoteVal ["sym1"], QuoteVal ["sym2"], QuoteVal ["sym3"]]
      run inFile expRes
      
    it "test_nonterminating_binding" $ do
      let expRes = success [Bottom]
      let inFile = "test_nonterminating_binding.scm"
      run inFile expRes

    it "test_let_lists" $ do 
      let expRes = success [BoolVal B.True]
      let inFile = "test_let_lists.scm"
      run inFile expRes

    it "test_letrec_lists" $ do 
      let expRes = success [BoolVal B.True]
      let inFile = "test_letrec_lists.scm"
      run inFile expRes

    it "test_app_lists" $ do 
      let expRes = success [BoolVal B.True]
      let inFile = "test_app_lists.scm"
      run inFile expRes

    it "test_random" $ do
      let inFile = "test_random.scm"
      let expRes = success [NumVal IntVal]
      run inFile expRes

success :: [Val] -> (HashSet Text, (Pow Val))
success v = ([],Pow.fromList v)

successOrFail :: [Val] -> HashSet Text -> (HashSet Text, (Pow Val))
successOrFail v errs = (errs, Pow.fromList v)

type Runner = (String -> (HashSet Text, (Pow Val)) -> IO ())

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

runner_nm :: (?algorithm :: Algorithm) => Eval_nm' -> Runner
runner_nm eval inFile expected = do
  prog <- loadSchemeFile inFile
  let ?sensitivity = 0
  let (cfg,(metric,(errs,res))) = eval [prog]
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
