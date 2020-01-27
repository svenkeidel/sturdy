{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
module IntervalAnalysisSpec where

import           Prelude hiding (succ,pred,id)

import           Data.Abstract.Error hiding (toEither)
import           Data.Abstract.Terminating hiding (toEither)
import qualified Data.Abstract.DiscretePowerset as Pow

import           Test.Hspec
-- import           SharedSpecs

import System.Process -- deprecated
import System.Directory

import LispTypes as LT hiding (Bool)
import LispParser
import LispToHask

import           IntervalAnalysis

import           Syntax as S
import qualified Data.Abstract.Boolean as B
import           Data.Abstract.DiscretePowerset (Pow)


import           Data.GraphViz hiding (diamond)
import           Data.Graph.Inductive(Gr)


main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "behavior specific to interval analysis" $ do
    it "context sensitivity" $
      let diamond = [if_ (lit $ Bool True) "x" "y"] in do
      let ?sensitivity = 0 in evalInterval' [("x", NumVal), ("y", NumVal)] diamond `shouldBe` Terminating (Success NumVal)
      let ?sensitivity = 1 in evalInterval' [("x", NumVal), ("y", NumVal)] diamond `shouldBe` Terminating (Success NumVal)

    it "should analyze let expression" $ 
      let expr = [let_ [("x", lit $ S.Number 1)] ["x"]] in do
      let ?sensitivity = 0 in evalInterval' [] expr `shouldBe` Terminating (Success NumVal)
      let ?sensitivity = 1 in evalInterval' [] expr `shouldBe` Terminating (Success NumVal)

    it "should analyze define" $ 
      let exprs = [define "x" (lit $ S.Number 1),                   
                   set "x" (lit $ S.Number 2), 
                   set "x" (lit $ S.Number 3),
                   "x"] in do
      let ?sensitivity = 0 in evalInterval' [] exprs `shouldBe` Terminating (Success NumVal)
      let ?sensitivity = 2 in evalInterval' [] exprs `shouldBe` Terminating (Success NumVal)


    it "should return top for unifying two different types" $  
      let exprs = [define "x" (lit $ S.Number 1),                   
                   set "x" (lit $ S.Number 2), 
                   set "x" (lit $ S.Bool True),
                   "x"] in do
      let ?sensitivity = 0 in evalInterval' [] exprs `shouldBe` Terminating (Success $ TypeError "cannot unify True and Num")
      let ?sensitivity = 2 in evalInterval' [] exprs `shouldBe` Terminating (Success $ TypeError "cannot unify True and Num")


    it "should terminate for the non-terminating program LetRec" $
      let ?sensitivity = 2
      in evalInterval' [] [let_rec [("id", lam ["x"] ["x"]),
                                ("fix",lam ["x"] [app "fix" ["x"]])]
                         [app "fix" ["id"]]]
           `shouldBe` NonTerminating

    it "should terminate for the non-terminating program Define" $
      pending

-----------------GABRIEL BENCHMARKS---------------------------------------------
  describe "Gabriel-Benchmarks" $ do

    it "cpstak" $ do 
      let inFile = "gabriel//cpstak"
      let expRes = Terminating (Success NumVal)
      helper_test inFile expRes


    it "deriv" $ do
      let inFile = "gabriel//deriv"
      let expRes = Terminating (Fail "{\"cannot unify Quote and List [TypeError: {\\\"cannot unify Quote and Num\\\"}]\"}")
      helper_test inFile expRes

    it "diviter" $ do
      let inFile = "gabriel//diviter"
      let expRes = Terminating (Success $ BoolVal B.True)              
      helper_test inFile expRes

    it "divrec" $ do
      pendingWith "returns False instead of true bc of: equal? (LV Bottom) (LV (LV Bottom))"
      let inFile = "gabriel//divrec"
      let expRes = Terminating (Success $ BoolVal B.Top)              
      helper_test inFile expRes      

    it "takl" $ do
      let inFile = "gabriel//takl"
      let expRes = Terminating (Success $ BoolVal B.Top)              
      helper_test inFile expRes

-------------------SCALA-AM BENCHMARKS------------------------------------------
  describe "Scala-AM-Benchmarks" $ do
    it "collatz" $ do
      let inFile = "scala-am//collatz"
      let expRes = Terminating (Success NumVal)
      helper_test inFile expRes

    it "gcipd" $ do
      let inFile = "scala-am//gcipd"
      let expRes = Terminating (Success NumVal)
      helper_test inFile expRes 

    it "nqueens" $ do
      let inFile = "scala-am//nqueens"
      let expRes = Terminating (Success NumVal)
      helper_test inFile expRes      

    it "rsa" $ do
      let inFile = "scala-am//rsa"
      let expRes = Terminating (Fail $ Pow.singleton "Expected elements of type num for op| [List [Num],Num]" <> "Scheme-Error")              
      helper_test inFile expRes      

-------------------Custom Tests------------------------------------------
  describe "Custom_Tests" $ do
    it "recursion and union with empty list" $ do
      let inFile = "test_rec_empty"
      let expRes = Terminating (Success $ ListVal Bottom)
      helper_test inFile expRes      

    it "recursion and union with non-empty list" $ do
      let inFile = "test_rec_nonempty"
      let expRes = Terminating (Success $ ListVal NumVal)          
      helper_test inFile expRes               

    it "should return listVal for (cdr '(2 3 4))" $ do
      let inFile = "test_cdr"
      let expRes = Terminating (Success $ ListVal NumVal)
      helper_test inFile expRes  

    it "should return correct val for car" $ do
      let inFile = "test_car"
      let expRes = Terminating (Success $ NumVal)
      helper_test inFile expRes      

    it "should return true for null? cdr cdr '(1 2)" $ do
      let inFile = "test_null"
      let expRes = Terminating (Success $ BoolVal B.Top)
      helper_test inFile expRes       

    it "unifying two list of nums of different size should result in list of nums" $ do
      let inFile = "test_faulty_list"
      let expRes = Terminating (Success $ ListVal NumVal)           
      helper_test inFile expRes  

    it "test_if" $ do
      let inFile = "test_if"
      let expRes = Terminating (Success $ ListVal NumVal)          
      helper_test inFile expRes            

    it "test_opvars" $ do
      let inFile = "test_opvars"
      let expRes = Terminating (Success $ NumVal)         
      helper_test inFile expRes         

    it "test_equal" $ do
      let inFile = "test_equal"
      let expRes = Terminating (Success $ BoolVal B.True)         
      helper_test inFile expRes   

    it "test_cons" $ do
      let inFile = "test_cons"
      let expRes = Terminating (Success $ BoolVal B.True)         
      helper_test inFile expRes 

    it "random_test" $ do
      let inFile = "random_test"
      let expRes = Terminating (Success NumVal)         
      helper_test inFile expRes 

    it "lang_scheme_test" $ do
      let inFile = "lang_scheme_test"
      let expRes = Terminating (Success NumVal)         
      helper_test inFile expRes 

    it "test_inner_define" $ do
      let inFile = "test_inner_define"
      let expRes = Terminating (Success NumVal)         
      helper_test inFile expRes 



-------------------HELPER------------------------------------------------------
helper_test :: String -> Terminating (Error (Pow String) Val) -> IO ()
helper_test inFile expRes = do
  file_str <- helper_import inFile
  case readExprList file_str of
    Right a ->
      case match a of
        Right b -> do
          let ?sensitivity = 0 
          let (graph, res) = evalInterval'' [let_rec (getTopDefinesLam b) (getBody b)]
          _ <- draw_graph inFile graph
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