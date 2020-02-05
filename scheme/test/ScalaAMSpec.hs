{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
module ScalaAMSpec where

import           Prelude hiding (succ,pred,id)

import           Data.Abstract.Error hiding (toEither)
import           Data.Abstract.Terminating hiding (toEither)
import qualified Data.Abstract.DiscretePowerset as Pow

import           Test.Hspec
-- import           SharedSpecs

import System.Process hiding (env) -- deprecated
import System.Directory

import LispTypes as LT hiding (Bool)
import LispParser
import LispToHask

import           ScalaAM

import           Syntax as S
import qualified Data.Abstract.Boolean as B
import           Data.Abstract.DiscretePowerset (Pow)


import           Data.GraphViz hiding (diamond)
import           Data.Graph.Inductive(Gr)

import           GHC.Exts (fromList)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "behavior specific to interval analysis" $ do
    let ?bound = 100  
    it "context sensitivity" $
      let diamond = [if_ "w" "x" "y"] in do
      let env =  [("w", BoolVal B.Top), ("x", IntVal (Pow.singleton 1)), ("y", IntVal(Pow.singleton 2))]
      let ?sensitivity = 0 in evalInterval' env diamond `shouldBe` Terminating (Success $ IntVal $ fromList [1,2])
      let ?sensitivity = 1 in evalInterval' env diamond `shouldBe` Terminating (Success $ IntVal $ fromList [1,2])

    it "should analyze let expression" $ 
      let expr = [let_ [("x", lit $ S.Number 1)] ["x"]] in do
      let ?sensitivity = 0 in evalInterval' [] expr `shouldBe` Terminating (Success $ IntVal $ fromList [1])
      let ?sensitivity = 1 in evalInterval' [] expr `shouldBe` Terminating (Success $ IntVal $ fromList [1])

    it "should analyze define" $ 
      let exprs = [define "x" (lit $ S.Number 1),                   
                   set "x" (lit $ S.Number 2), 
                   set "x" (lit $ S.Number 3),
                   "x"] in do
      let ?sensitivity = 0 in evalInterval' [] exprs `shouldBe` Terminating (Success $ IntVal $ fromList [1,2,3])
      let ?sensitivity = 2 in evalInterval' [] exprs `shouldBe` Terminating (Success $ IntVal $ fromList [1,2,3])


    it "should return top for unifying two different types" $  
      let exprs = [define "x" (lit $ S.Number 1),                   
                   set "x" (lit $ S.Number 2), 
                   set "x" (lit $ S.Bool True),
                   "x"] in do
      let ?sensitivity = 0 in evalInterval' [] exprs `shouldBe` Terminating (Success $ TypeError "cannot unify True and Int: {1, 2}")
      let ?sensitivity = 2 in evalInterval' [] exprs `shouldBe` Terminating (Success $ TypeError "cannot unify True and Int: {1, 2}")


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
    let ?bound = 1000
    let ?sensitivity = 0
    it "cpstak" $ do 
      let inFile = "gabriel//cpstak"
      let expRes = Terminating (Success $ IntVal $ fromList [6])
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
      -- pendingWith "returns False instead of true bc of: equal? (LV Bottom) (LV (LV Bottom))"
      let inFile = "gabriel//divrec"
      let expRes = Terminating (Success $ BoolVal B.Top)              
      helper_test inFile expRes      

    it "takl" $ do
      let inFile = "gabriel//takl"
      -- pendingWith "returns False instead of top/true because NV dont compare yet"
      let expRes = Terminating (Success $ BoolVal B.Top)              
      helper_test inFile expRes
      
-------------------SCALA-AM BENCHMARKS------------------------------------------
  describe "Scala-AM-Benchmarks" $ do
    let ?bound = 1000
    let ?sensitivity = 0
    it "collatz" $ do
      let inFile = "scala-am//collatz"
      let expRes = Terminating (Success $ IntVal $ fromList [6])
      helper_test inFile expRes

    it "gcipd" $ do
      let inFile = "scala-am//gcipd"
      let expRes = Terminating (Success $ IntVal $ fromList [36])
      helper_test inFile expRes 

    it "nqueens" $ do
      let inFile = "scala-am//nqueens"
      let expRes = Terminating (Success $ IntVal $ fromList [92])
      helper_test inFile expRes      

    it "rsa" $ do
      let inFile = "scala-am//rsa"
      let expRes = Terminating (Fail $ Pow.singleton "Expected elements of type num for op| [List [Num],Num]" <> "Scheme-Error")              
      helper_test inFile expRes      

-------------------Custom Tests------------------------------------------
  describe "Custom_Tests" $ do
    let ?bound = 100
    let ?sensitivity = 1
    it "recursion and union with empty list" $ do
      let inFile = "test_rec_empty"
      let expRes = Terminating (Success $ ListVal [Bottom])
      helper_test inFile expRes      

    it "recursion and union with non-empty list" $ do
      let inFile = "test_rec_nonempty"
      let expRes = Terminating (Success $ ListVal $ [IntVal $ fromList [1]])          
      helper_test inFile expRes               

    it "should return listVal for (cdr '(2 3 4))" $ do
      let inFile = "test_cdr"
      let expRes = Terminating (Success $ ListVal $ [IntVal $ fromList [3], IntVal $ fromList [4]])
      helper_test inFile expRes  

    it "should return correct val for car" $ do
      let inFile = "test_car"
      let expRes = Terminating (Success $ IntVal $ fromList [1])
      helper_test inFile expRes      

    it "should return true for null? cdr cdr '(1 2)" $ do
      let inFile = "test_null"
      let expRes = Terminating (Success $ BoolVal B.True)
      helper_test inFile expRes       

    it "unifying two list of nums of different size should result in an error" $ do
      let inFile = "test_faulty_list"
      let expRes = Terminating (Fail "{\"cannot unify lists of differing lengths| List [Int: {1}], List [Int: {1},Int: {2}]\"}")           
      helper_test inFile expRes  

    it "test_if" $ do
      let inFile = "test_if"
      let expRes = Terminating (Success $ ListVal [IntVal $ fromList [1], IntVal $ fromList [2]])          
      helper_test inFile expRes            

    it "test_opvars" $ do
      let inFile = "test_opvars"
      let expRes = Terminating (Success $ IntVal $ fromList [10])         
      helper_test inFile expRes         

    it "test_equal" $ do
      let inFile = "test_equal"
      let expRes = Terminating (Success $ BoolVal B.True)         
      helper_test inFile expRes   

    it "test_cons" $ do
      let inFile = "test_cons"
      let expRes = Terminating (Success $ BoolVal B.True)         
      helper_test inFile expRes 

    it "test_closures_gc" $ do
      let inFile = "test_closure_gc"
      let expRes = Terminating (Success $ IntVal $ fromList [6,7,8,9,12,16])         
      helper_test inFile expRes 

    it "lang_scheme_test" $ do
      let inFile = "lang_scheme_test"
      let expRes = Terminating (Success $ IntVal $ fromList [8])         
      helper_test inFile expRes 

    it "test_inner_define" $ do
      let inFile = "test_inner_define"
      let expRes = Terminating (Success $ IntVal $ fromList [8,10])         
      helper_test inFile expRes 

    it "test_subtraction" $ do
      let inFile = "test_subtraction"
      let expRes = Terminating (Success $ IntVal $ fromList [-4])         
      helper_test inFile expRes     

    it "test_endless_recursion" $ do
      let inFile = "test_endless_recursion"
      let expRes = NonTerminating
      helper_test inFile expRes     

    it "test_lits" $ do
      let inFile = "test_lits"
      let expRes = Terminating (Success $ IntVal $ fromList [3])
      helper_test inFile expRes     

    it "test_simple_floats" $ do
      let inFile = "test_simple_floats"
      let expRes = Terminating (Success $ BoolVal B.False)
      helper_test inFile expRes     

-------------------HELPER------------------------------------------------------
helper_test :: (?sensitivity :: Int, ?bound :: Int) => String -> Terminating (Error (Pow String) Val) -> IO ()
helper_test inFile expRes = do
  file_str <- helper_import inFile
  case readExprList file_str of
    Right a ->
      case match a of
        Right b -> do
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