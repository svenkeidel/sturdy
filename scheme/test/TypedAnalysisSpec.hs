{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
module TypedAnalysisSpec where

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

import           TypedAnalysis

import           Syntax as S
import qualified Data.Abstract.Boolean as B
import           Data.Abstract.DiscretePowerset (Pow, singleton)


import           Data.GraphViz hiding (diamond)
import           Data.Graph.Inductive(Gr)
import           GHC.Exts(toList, fromList)
import qualified Data.HashSet as Set 
import qualified Data.HashMap.Lazy as Map

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "behavior specific to interval analysis" $ do
    it "test lits" $ do 
      -- let ?sensitivity = 0 in evalInterval' [("x", singleton NumVal), ("y", singleton NumVal)] ["x"] `shouldBe` Terminating (Success $ singleton NumVal)
      let ?sensitivity = 0 in evalInterval' [] [define "x" (lit $ S.Number 2), "x"] `shouldBe` Terminating (Success $ singleton NumVal)
      let ?sensitivity = 0 in evalInterval' [] [define "x" (lit $ S.Number 2), 
                                                set "x" (lit $ S.Bool True),
                                                "x"] `shouldBe` Terminating (Success $ fromList [NumVal, BoolVal B.True])

    it "test closures" $ do
      let ?sensitivity = 0 in evalInterval' [] [app (lam ["x"] ["x"]) [lit $ S.Number 2]] `shouldBe` Terminating (Success $ singleton NumVal)
      let ?sensitivity = 0 in evalInterval' [] [define "id" (lam ["x"] ["x"]),
                                                app "id" [lit $ S.Number 2]] `shouldBe` Terminating (Success $ singleton NumVal)
      let ?sensitivity = 0 in evalInterval' [] [define "id" (lit $ S.Bool True),
                                                set "id" (lam ["x"] ["x"]),
                                                app "id" [lit $ S.Number 2]] `shouldBe` Terminating (Success $ singleton NumVal)
      let ?sensitivity = 0 in evalInterval' [] [define "id" (lam ["x"] ["x"]),
                                                set "id" (lit $ S.Bool True),
                                                app "id" [lit $ S.Number 2]] `shouldBe` Terminating (Success $ singleton NumVal)
    -- it "context sensitivity" $
    --   let diamond = [if_ (lit $ Bool True) "x" "y"] in do
      -- let ?sensitivity = 0 in evalInterval' [("x", singleton NumVal), ("y", singleton NumVal)] diamond `shouldBe` Terminating (Success $ singleton NumVal)
      -- let ?sensitivity = 1 in evalInterval' [("x", singleton NumVal), ("y", singleton NumVal)] diamond `shouldBe` Terminating (Success $ singleton NumVal)

    it "should analyze let expression" $ 
      let expr = [let_ [("x", lit $ S.Number 1)] ["x"]] in do
      let ?sensitivity = 0 in evalInterval' [] expr `shouldBe` Terminating (Success $ singleton NumVal)
      let ?sensitivity = 1 in evalInterval' [] expr `shouldBe` Terminating (Success $ singleton NumVal)

    it "should analyze define" $ 
      let exprs = [define "x" (lit $ S.Number 1),                   
                   set "x" (lit $ S.Number 2), 
                   set "x" (lit $ S.Number 3),
                   "x"] in do
      let ?sensitivity = 0 in evalInterval' [] exprs `shouldBe` Terminating (Success $ singleton NumVal)
      let ?sensitivity = 2 in evalInterval' [] exprs `shouldBe` Terminating (Success $ singleton NumVal)


    it "should return unify two different types" $  
      let exprs = [define "x" (lit $ S.Number 1),                   
                   set "x" (lit $ S.Number 2), 
                   set "x" (lit $ S.Bool True),
                   "x"] in do
      let ?sensitivity = 0 in evalInterval' [] exprs `shouldBe` Terminating (Success $ fromList [NumVal, BoolVal B.True])
      let ?sensitivity = 2 in evalInterval' [] exprs `shouldBe` Terminating (Success $ fromList [NumVal, BoolVal B.True])


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
-- TIMEOUT = 30s

    it "cpstak" $ do 
--     => Final Values: Set(Int)
--     => TIME: 105 | STATES: 120
      let inFile = "gabriel//cpstak"
      let expRes = Terminating (Success $ singleton NumVal)
      helper_test inFile expRes



    it "deriv" $ do
-- => TIMEOUT | STATES: 1645737
      pendingWith "!!!!!!!! some variable not bound !!!!!!!"
      let inFile = "gabriel//deriv"
      let expRes = Terminating (Fail "{\"cannot unify Quote and List [TypeError: {\\\"cannot unify Quote and Num\\\"}]\"}")
      helper_test inFile expRes

    it "diviter" $ do
--     => Final Values: Set(#f, {#f,#t})
--     => TIME: 163 | STATES: 175
      pendingWith "out of memory"
      let inFile = "gabriel//diviter"
      let expRes = Terminating (Success $ fromList [BoolVal B.False, BoolVal B.Top])              
      helper_test inFile expRes

    it "divrec" $ do
--       => Final Values: Set(#f, {#f,#t})
--       => TIME: 59 | STATES: 219
      pendingWith "out of memory"
      let inFile = "gabriel//divrec"
      let expRes = Terminating (Success $ fromList [BoolVal B.False, BoolVal B.Top])              
      helper_test inFile expRes      

    it "takl" $ do
-- => TIMEOUT | STATES: 1959438
      pendingWith "out of memory"
      let inFile = "gabriel//takl"
      let expRes = Terminating (Success $ singleton $ BoolVal B.Top)              
      helper_test inFile expRes

-- -------------------SCALA-AM BENCHMARKS------------------------------------------
  describe "Scala-AM-Benchmarks" $ do
    it "collatz" $ do
-- => Final Values: Set(Int)
-- => TIME: 8 | STATES: 431
      let inFile = "scala-am//collatz"
      let expRes = Terminating (Success $ singleton NumVal)
      helper_test inFile expRes

    it "gcipd" $ do
--       => Final Values: Set(Int)
--       => TIME: 14 | STATES: 1098
      let inFile = "scala-am//gcipd"
      let expRes = Terminating (Success $ singleton NumVal)
      helper_test inFile expRes 

    it "nqueens" $ do
-- => TIMEOUT | STATES: 1781142
      let inFile = "scala-am//nqueens"
      let expRes = Terminating (Success $ singleton NumVal)
      helper_test inFile expRes      

    it "rsa" $ do
-- => Final Values: Set({#f,#t})
-- => TIME: 2831 | STATES: 247915
      pendingWith "!!!!!!!! some variable not bound !!!!!!!"
      let inFile = "scala-am//rsa"
      let expRes = Terminating (Fail $ Pow.singleton "Expected elements of type num for op| [List [Num],Num]" <> "Scheme-Error")              
      helper_test inFile expRes      

-- -------------------Custom Tests------------------------------------------
  describe "Custom_Tests" $ do
    it "recursion_union_empty_list" $ do
      let inFile = "test_rec_empty"
      let expRes = Terminating (Success $ singleton Bottom)
      helper_test inFile expRes      

    it "recursion and union with non-empty list" $ do
      let inFile = "test_rec_nonempty"
      let expRes = Terminating (Success $ singleton NumVal)          
      helper_test inFile expRes               

    it "should return listVal for (cdr '(2 3 4))" $ do
      pendingWith "passes"
      let inFile = "test_cdr"
      let expRes = Terminating (Success $ singleton NumVal)
      helper_test inFile expRes  

    it "should return correct val for car" $ do
      let inFile = "test_car"
      let expRes = Terminating (Success $ singleton NumVal)
      helper_test inFile expRes      

    it "test_null_cdr" $ do
      let inFile = "test_null"
      let expRes = Terminating (Success $ singleton $ BoolVal B.True)
      helper_test inFile expRes       

    it "unifying two list of nums of different size should result in list of nums" $ do
      pendingWith "passes"
      let inFile = "test_faulty_list"
      let expRes = Terminating (Success $ singleton NumVal)           
      helper_test inFile expRes  

    it "test_if" $ do
      pendingWith "passes"
      let inFile = "test_if"
      let expRes = Terminating (Success $ singleton NumVal)          
      helper_test inFile expRes            

    it "test_opvars" $ do
      let inFile = "test_opvars"
      let expRes = Terminating (Success $ singleton NumVal)         
      helper_test inFile expRes         

    it "test_equal" $ do
      let inFile = "test_equal"
      let expRes = Terminating (Success $ singleton $ BoolVal B.True)         
      helper_test inFile expRes   

    it "test_cons" $ do
      let inFile = "test_cons"
      let expRes = Terminating (Success $ singleton $ BoolVal B.True)         
      helper_test inFile expRes 

    it "test_closure_gc" $ do
      let inFile = "test_closure_gc"
      let expRes = Terminating (Success $ singleton NumVal)         
      helper_test inFile expRes 

    it "lang_scheme_test" $ do
      let inFile = "lang_scheme_test"
      let expRes = Terminating (Success $ singleton NumVal)         
      helper_test inFile expRes 

    it "test_inner_define" $ do
      let inFile = "test_inner_define"
      let expRes = Terminating (Success $ singleton NumVal)         
      helper_test inFile expRes 

    it "test_unops" $ do
      let inFile = "test_unops"
      let expRes = Terminating (Success $ fromList [BoolVal B.True, BoolVal B.False])         
      helper_test inFile expRes

    it "test_eq" $ do
      let inFile = "test_eq"
      let expRes = Terminating (Success $ singleton $ BoolVal B.Top)         
      helper_test inFile expRes 

    it "test_binops" $ do
      let inFile = "test_binops"
      let expRes = Terminating (Success $ singleton $ Bottom)         
      helper_test inFile expRes 

    it "test_opvar_numbool" $ do
      let inFile = "test_opvar_numbool"
      let expRes = Terminating (Success $ singleton $ Bottom)         
      helper_test inFile expRes 

    it "test_opvar_numnum" $ do
      let inFile = "test_opvar_numnum"
      let expRes = Terminating (Success $ singleton NumVal)         
      helper_test inFile expRes 

    it "test_opvar_boolbool" $ do
      let inFile = "test_opvar_boolbool"
      let expRes = Terminating (Success $ fromList [BoolVal B.False, BoolVal B.True, StringVal])         
      helper_test inFile expRes 

    it "test_list" $ do
      let inFile = "test_list"
      let expRes = Terminating (Success $ singleton EmptyList)         
      helper_test inFile expRes       

    it "test_factorial" $ do
      let inFile = "test_factorial"
      let expRes = Terminating (Success $ singleton $ NumVal)         
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
          -- let (graph, res) = evalInterval'' [let_rec (getTopDefinesLam b) (getBody b)]
          let res = evalInterval'' [let_rec (getTopDefinesLam b) (getBody b)]
          -- _ <- draw_graph inFile graph
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