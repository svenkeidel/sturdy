{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
module IntervalAnalysisSpec where

import           Prelude hiding (succ,pred,id)

import           Data.Abstract.Error hiding (toEither)
import           Data.Abstract.Terminating hiding (toEither)

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

-- import           Data.Label
-- import           Control.Monad.State
-- import           Data.HashMap.Lazy (HashMap)
-- import qualified Data.HashMap.Lazy as M
-- import           Data.Text.Lazy

import           Data.GraphViz
-- import           Data.GraphViz.Printing
import           Data.Graph.Inductive(Gr)
-- import qualified Data.Graph.Inductive as G
-- import           Data.Graph.Inductive.Example as Ex


-- import           Data.Label

-- import           GHC.Exts(toList)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- fileGraphsRun(\file -> (evalInterval'' file))
  -- let ?bound = I.Interval (-100) 100; ?sensitivity = 3 in sharedSpec (toEither . evalInterval' []) (NumVal . fromIntegral)

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
      let ?sensitivity = 0 in evalInterval' [] exprs `shouldBe` Terminating (Success $ TypeError "cannot unify BoolVals and NumVals")
      let ?sensitivity = 2 in evalInterval' [] exprs `shouldBe` Terminating (Success $ TypeError "cannot unify BoolVals and NumVals")


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
      let expRes = Terminating (Fail "message")
      helper_test inFile expRes

    it "diviter" $ do
      let inFile = "gabriel//diviter"
      let expRes = Terminating (Success $ BoolVal B.Top)              
      helper_test inFile expRes

    it "divrec" $ do
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
      let expRes = Terminating (Success $ BoolVal B.Top)              
      helper_test inFile expRes      

-------------------Custom Tests------------------------------------------
  describe "Custom_Tests" $ do
    it "recursion and union with empty list" $ do
      let inFile = "test_rec_empty"
      let expRes = Terminating (Fail "{\"empty list\"}")
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

    it "test simple num lists" $ do
      let inFile = "test_simple_list"
      let expRes = Terminating (Success $ ListVal NumVal)          
      helper_test inFile expRes            

    it "test_empty_lists" $ do
      let inFile = "test_empty_lists"
      let expRes = Terminating (Fail "{\"empty list\"}")
      helper_test inFile expRes  

    it "test_opvars" $ do
      let inFile = "test_opvars"
      let expRes = Terminating (Success $ NumVal)         
      helper_test inFile expRes         



  describe "graph-test" $ 
    it "test graph" $ do 
      let inFile = "test_opvars"
      file_str <- helper_import inFile
      -- file_str <- helper_import "//scheme_files//scala-am//nqueens.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> do 
              let ?sensitivity = 0
              let (graph, res) = evalInterval'' [let_rec (getTopDefinesLam b) (getBody b)]
              _ <- draw_graph inFile graph
              res `shouldBe` Terminating (Success $ NumVal)

              -- G.prettyPrint graph
              -- G.prettyPrint Ex.g3 
              -- putStrLn $ unpack $ renderDot $ toDot $ graphToDot nonClusteredParams clr479
              -- let dotGraph = graphToDot nonClusteredParams graph
              -- root <- getCurrentDirectory 
              -- let root' = root ++ "//scheme_files//scala-am//nqueens" ++ ".png"
              -- runGraphvizCommand :: PrintDotRepr dg n => GraphvizCommand -> dg n -> GraphvizOutput -> FilePath -> IO FilePath
              -- _ <- runGraphvizCommand Dot dotGraph Png root'
              -- readCreateProcess (shell) 
              -- putStrLn $ unpack $ renderDot $ toDot $ graphToDot nonClusteredParams graph

 
            Left b -> print b
        Left a -> print $ showError a

-------------------HELPER-------------------------------------------------------


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