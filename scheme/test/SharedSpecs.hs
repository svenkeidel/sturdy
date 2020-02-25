{-# LANGUAGE OverloadedStrings #-}
module SharedSpecs where

import Prelude hiding (succ,pred)

import Control.Monad.State

import Data.Label
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import ConcreteInterpreter (Val(..),Addr)

import Syntax as S
import Test.Hspec

import System.Process -- deprecated
import System.Directory

import LispTypes as LT
import LispParser
import LispToHask



sharedSpecRun :: ([State Label Expr] -> Either (HashMap Addr Val, String) Val) -> Spec
sharedSpecRun run = describe "shared language behavior" $ do

  it "test set! " $
    run [define "x" (lit $ S.Number 2),
        define "y" (lit $ S.Number 3),
        set "y" (lit $ S.Number 3)] `shouldBe` Right ((StringVal "#<void>"))

  it "test simple let" $ do
    let es' = [let_ [("y", lit $ S.Number 3)] ["y"] ]
    run es' `shouldBe` Right (NumVal 3)

  it "test simple letrec" $ do
    let es' = [let_rec [("y", lit $ S.Number 3)] ["y"] ]
    run es' `shouldBe` Right (NumVal 3)

  it "test simple lambda" $ do
    let es' = [app (lam ["x"] ["x"]) [lit $ S.Number 3]]
    run es' `shouldBe` Right (NumVal 3)


  it "test define with lambda body  " $ do
    let es' = [let_rec [("y", lam ["y"] ["y"])]
                    [app "y" [lit $ S.Number 3]]]
    run es' `shouldBe` Right (NumVal 3)
    
  it "test define with lambda body  " $ do
    let es' = [let_ [("x", lam ["y"] ["y"])]
                    [app "x" [lit $ S.Number 3]]]   
    run es' `shouldBe` Right (NumVal 3)


  it "demonstrate need for stepwise evaluation of letrec bindings" $ do
    let es' = [let_rec [("x", lit $ S.Number 4),
                        ("y", "x")]
                    --  [lam ["x"] ["x"]]]
                        ["y"]]
    run es' `shouldBe` Right (NumVal 4)

  it "test define with lambda body  " $ do
    let es = [define "y" (lam ["x"] ["x"]),
              define "z" (lit $ S.Number 4),
              define "z1" (lit $ S.Number 5),
              define "k" (lit $ S.Number 6),
              app "y" [lit $ S.Number 3]]  
    run es `shouldBe` Right (NumVal 3)

  it "test define with lambda body  " $
    run [define "x" (lam ["n"] [app (lam ["x"] ["x"]) ["n"]]),
        app "x" [lit $ S.Number 2],
        app "x" [lit $ S.Number 3]] `shouldBe` Right (NumVal 3)

  it "test define with lambda body  " $ do
    let es = [define "x" (lam ["x"] ["x"]),
              app "x" [lit $ S.Number 3]]
    run es `shouldBe` Right (NumVal 3)

  it "test define with lambda body  " $ do
    let es = [let_ [("y", lam ["y"] ["y"])]
                   [define "x" (lam ["x"] ["x"]),
                   app "x" [lit $ S.Number 3]]]
    run es `shouldBe` Right (NumVal 3)

  it "illustrate necessity of store" $
    run [define "x" (lit $ S.Number 2),
        app 
          (lam [] [set "x" (lit $ S.Number 3)])
          [],
          "x"] `shouldBe` Right (NumVal 3)
  
  it "correctly evaluate sequential letrec" $
    run [let_rec [("x", lit $ S.Number 3),
                  ("y", lam [] ["x"])] 
            [app "y" []]] `shouldBe` Right (NumVal 3)

  it "correctly evaluate mutually recursive letrec" $
    run [let_rec [("y", lam [] ["x"]),
                  ("x", lit $ S.Number 3)] 
            [app "y" []]] `shouldBe` Right (NumVal 3)
  
  it "should fail define the same var twice" $ do 
    pendingWith "need to fix check in define "
    run [define "x" (lit $ S.Number 2),
        define "x" (lit $ S.Number 3)] `shouldBe` Left (M.fromList [], "Variable \"x\" already exists")

  


sharedSpecFile :: ([State Label Expr] -> Either (HashMap Addr Val, String) Val) -> Spec
sharedSpecFile run = do

-----------------GABRIEL BENCHMARKS---------------------------------------------
  describe "Gabriel-Benchmarks" $ do
    
    it "null" $ do 
      -- pendingWith "out of memory"
      file_str <- helper_import "//scheme_files//gabriel//test.scm"
      putStrLn file_str
      case readExprList file_str of
        Right a -> do
          putStrLn $ show a 
          case match a of
            Right b -> run [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Right (BoolVal True)
            Left b -> print b
        Left a -> print $ showError a

    it "boyer" $ do 
      -- pendingWith "out of memory"
      file_str <- helper_import "//scheme_files//gabriel//boyer.scm"
      putStrLn file_str
      case readExprList file_str of
        Right a -> do
          putStrLn $ show a 
          case match a of
            Right b -> run [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Right (BoolVal True)
            Left b -> print b
        Left a -> print $ showError a

    -- it "cpstak" $ do 
    --   pendingWith "out of memory"
    --   file_str <- helper_import "//scheme_files//gabriel//cpstak.scm"
    --   case readExprList file_str of
    --     Right a ->
    --       case match a of
    --         Right b -> run [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Right (NumVal 6)
    --         Left b -> print b
    --     Left a -> print $ showError a

    -- it "deriv" $ do
    --   file_str <- helper_import "//scheme_files//gabriel//deriv.scm"
    --   putStrLn file_str
    --   case readExprList file_str of
    --     Right a ->
    --       case match a of
    --         Right b -> run [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Right (BoolVal True)
    --         Left b -> print b
    --     Left a -> print $ showError a

    it "diviter" $ do
      file_str <- helper_import "//scheme_files//gabriel//diviter.scm"
      case readExprList file_str of
        Right a -> case match a of
            Right b -> run [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Right (BoolVal True)
            Left b -> print b
        Left a -> print a

    it "divrec" $ do
      file_str <- helper_import "//scheme_files//gabriel//divrec.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> run [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Right (BoolVal True)
            Left b -> print b
        Left a -> print $ showError a

    it "takl" $ do
      pendingWith "out of memory"
      file_str <- helper_import "//scheme_files//gabriel//takl.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> run [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Right (BoolVal True)
            Left b -> print b
        Left a -> print $ showError a

-------------------SCALA-AM BENCHMARKS------------------------------------------
  describe "Scala-AM-Benchmarks" $ do
    it "collatz" $ do
      file_str <- helper_import "//scheme_files//scala-am//collatz.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> run [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Right (NumVal 5)
            Left b -> print b
        Left a -> print $ showError a

    it "gcipd" $ do
      file_str <- helper_import "//scheme_files//scala-am//gcipd.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> run [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Right (NumVal 36)
            Left b -> print b
        Left a -> print $ showError a

    it "nqueens" $ do
      file_str <- helper_import "//scheme_files//scala-am//nqueens.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b ->  run [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Right (NumVal 92)
            Left b -> print b
        Left a -> print $ showError a

    it "rsa" $ do
      file_str <- helper_import "//scheme_files//scala-am//rsa.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> run [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Right (BoolVal True)
            Left b -> print b
        Left a -> print $ showError a


  -- describe "Custom_Tests" $ 
  --   it "let statements should extend the environment with bindings" $ do
  --     pendingWith "insert test"
  --     file_str <- helper_import "//scheme_files//test.scm"
  --     putStrLn file_str
  --     case readExprList file_str of
  --       Right a ->
  --         case match a of
  --           Right b -> do
  --             putStrLn (show b)
  --             let es = let_rec (getTopDefinesLam b) (getBody b)
  --             putStr $ show $ generate es
  --             run [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Right (BoolVal True)
  --           Left b -> print b
  --       Left a -> print $ showError a
------------------------------------------CUSTOMS--------------------------------------------

  -- describe "Custom_Tests" $ do
  --     -- let ?bound = 100
  --     -- let ?sensitivity = 0
  --     it "recursion and union with empty list" $ do
  --       let inFile = "test_rec_empty"
  --       let expRes = Right EmptyList
  --       helper_test run inFile expRes      

  --     it "recursion and union with non-empty list" $ do
  --       let inFile = "test_rec_nonempty"
  --       let expRes = Right $ NumVal 1 
  --       helper_test run inFile expRes               

  --     it "should return 3 for (car (cdr '(2 3 4)))" $ do
  --       let inFile = "test_cdr"
  --       let expRes = Right $ NumVal 3 
  --       helper_test run inFile expRes  

  --     it "should return correct val for car" $ do
  --       let inFile = "test_car"
  --       let expRes = Right $ NumVal 1
  --       helper_test run inFile expRes      

  --     it "should return true for null? cdr cdr '(1 2)" $ do
  --       let inFile = "test_null"
  --       let expRes = Right $ BoolVal True
  --       helper_test run inFile expRes       

  --     -- it "unifying two list of nums of different size should result in an error" $ do
  --     --   let inFile = "test_faulty_list"
  --     --   let expRes = Terminating (Fail "{\"cannot unify lists of differing lengths| List [Int: {1}], List [Int: {1},Int: {2}]\"}")           
  --     --   helper_test inFile expRes  

  --     it "test_if" $ do
  --       let inFile = "test_if"
  --       let expRes = Right $ BoolVal False
  --       helper_test run inFile expRes            

  --     it "test_opvars" $ do
  --       let inFile = "test_opvars"
  --       let expRes = Right $ NumVal 10          
  --       helper_test run inFile expRes         

  --     it "test_equal" $ do
  --       let inFile = "test_equal"
  --       let expRes = Right $ BoolVal True
  --       helper_test run inFile expRes   

  --     it "test_cons" $ do
  --       let inFile = "test_cons"
  --       let expRes = Right $ BoolVal True
  --       helper_test run inFile expRes 

  --     it "test_closures_gc" $ do
  --       let inFile = "test_closure_gc"
  --       let expRes = Right $ NumVal 16          
  --       helper_test run inFile expRes 

  --     it "lang_scheme_test" $ do
  --       let inFile = "lang_scheme_test"
  --       let expRes = Right $ NumVal 8   
  --       helper_test run inFile expRes 

  --     it "test_inner_define" $ do
  --       let inFile = "test_inner_define"
  --       let expRes = Right $ NumVal 10          
  --       helper_test run inFile expRes 

  --     it "test_subtraction" $ do
  --       let inFile = "test_subtraction"
  --       let expRes = Right $ NumVal (-4)   
  --       helper_test run inFile expRes     

  --     it "test_lits" $ do
  --       let inFile = "test_lits"
  --       let expRes = Right $ NumVal 3 
  --       helper_test run inFile expRes     

  --     it "test_simple_floats" $ do
  --       let inFile = "test_simple_floats"
  --       let expRes = Right $ BoolVal False 
  --       helper_test run inFile expRes     

  --     it "test_rec_defines" $ do
  --       let inFile = "test_rec_defines"
  --       let expRes = Right $ NumVal 720 
  --       helper_test run inFile expRes
        
  --     it "test_random" $ do
  --       let inFile = "test_random"
  --       let expRes = Right $ NumVal 1         
  --       helper_test run inFile expRes       
        


-------------------HELPER-------------------------------------------------------
helper_import :: String -> IO String
helper_import inFile = do
  root <- getCurrentDirectory
  let root' = root ++ inFile
  readCreateProcess (shell $ "raco expand " ++ root') ""

-- helper_test :: String -> Either (HashMap Addr Val, String) Val -> IO ()
-- helper_test inFile expRes = do
--   file_str <- helper_import inFile
--   case readExprList file_str of
--     Right a ->
--       case match a of
--         Right b -> do
--           let res = run [let_rec (getTopDefinesLam b) (getBody b)]
--           res`shouldBe` expRes
--         Left b -> print b
--     Left a -> print $ showError a


helper_test :: ([State Label Expr] -> Either (HashMap Addr Val, String) Val) -> String -> Either (HashMap Addr Val, String) Val -> IO ()
helper_test run inFile expRes = do
  file_str <- helper_import_ inFile
  case readExprList file_str of
    Right a ->
      case match a of
        Right b -> run [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` expRes
        Left b -> print b
    Left a -> print $ showError a

helper_import_ :: String -> IO String
helper_import_ inFile = do
  root <- getCurrentDirectory
  let root' = root ++ "//scheme_files//" ++  inFile ++ ".scm"
  readCreateProcess (shell $ "raco expand " ++ root') ""