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
-- import           Data.Label

-- import           GHC.Exts(toList)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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
      pendingWith "takes too long"  
      file_str <- helper_import "//scheme_files//gabriel//cpstak.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> 
              let ?sensitivity = 0 in evalInterval' [] [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Terminating (Success NumVal)
            Left b -> print b
        Left a -> print $ showError a


    it "deriv" $ do
      file_str <- helper_import "//scheme_files//gabriel//deriv.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> 
              let ?sensitivity = 0 in evalInterval' [] [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Terminating (Fail "message")
            Left b -> print b
        Left a -> print $ showError a

    it "diviter" $ do
      file_str <- helper_import "//scheme_files//gabriel//diviter.scm"
      case readExprList file_str of
        Right a -> case match a of
            Right b -> 
              let ?sensitivity = 0 in evalInterval' [] [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Terminating (Success $ BoolVal B.Top)              
            Left b -> print b
        Left a -> print a

    it "divrec" $ do
      file_str <- helper_import "//scheme_files//gabriel//divrec.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> 
              let ?sensitivity = 0 in evalInterval' [] [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Terminating (Success $ BoolVal B.Top)
            Left b -> print b
        Left a -> print $ showError a

    it "takl" $ do
      file_str <- helper_import "//scheme_files//gabriel//takl.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> 
              let ?sensitivity = 0 in evalInterval' [] [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Terminating (Success $ BoolVal B.Top)              
            Left b -> print b
        Left a -> print $ showError a

-------------------SCALA-AM BENCHMARKS------------------------------------------
  describe "Scala-AM-Benchmarks" $ do
    it "collatz" $ do
      pendingWith "takes too long"
      file_str <- helper_import "//scheme_files//scala-am//collatz.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> 
              let ?sensitivity = 0 in evalInterval' [] [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Terminating (Success NumVal)
            Left b -> print b
        Left a -> print $ showError a    

    it "gcipd" $ do
          file_str <- helper_import "//scheme_files//scala-am//gcipd.scm"
          case readExprList file_str of
            Right a ->
              case match a of
                Right b -> 
                  let ?sensitivity = 0 in evalInterval' [] [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Terminating (Success NumVal)
                Left b -> print b
            Left a -> print $ showError a

    it "nqueens" $ do
      pendingWith "takes too long"
      file_str <- helper_import "//scheme_files//scala-am//nqueens.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b ->
              let ?sensitivity = 0 in evalInterval' [] [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Terminating (Success NumVal)
            Left b -> print b
        Left a -> print $ showError a

    it "rsa" $ do
      pendingWith "takes too long"
      file_str <- helper_import "//scheme_files//scala-am//rsa.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> 
              let ?sensitivity = 0 in evalInterval' [] [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Terminating (Success $ BoolVal B.True)
            Left b -> print b
        Left a -> print $ showError a


-------------------Custom Tests------------------------------------------
  describe "Custom_Tests" $ do
    it "recursion and union with empty list" $ do
      file_str <- helper_import "//scheme_files//test_rec_empty.scm"
      -- putStrLn file_str
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> 
              -- do
              -- putStrLn (show b)
              -- let es = let_rec (getTopDefinesLam b) (getBody b)
              -- putStr $ show $ generate es
              let ?sensitivity = 0 in evalInterval' [] [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Terminating (Fail "{\"empty list\"}")
            Left b -> print b
        Left a -> print $ showError a


    it "recursion and union with non-empty list" $ do
      file_str <- helper_import "//scheme_files//test_rec_nonempty.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> 
              let ?sensitivity = 0 in evalInterval' [] [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Terminating (Success $ ListVal NumVal)
            Left b -> print b
        Left a -> print $ showError a        

    it "should return listVal for (cdr '(2 3 4))" $ do
      file_str <- helper_import "//scheme_files//test_cdr.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> 
              let ?sensitivity = 0 in evalInterval' [] [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Terminating (Success $ ListVal NumVal)
            Left b -> print b
        Left a -> print $ showError a

    it "should return correct val for car" $ do
      file_str <- helper_import "//scheme_files//test_car.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> 
              let ?sensitivity = 0 in evalInterval' [] [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Terminating (Success $ NumVal)
            Left b -> print b
        Left a -> print $ showError a        

    it "should return true for null? cdr cdr '(1 2)" $ do
      file_str <- helper_import "//scheme_files//test_null.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> 
              let ?sensitivity = 0 in evalInterval' [] [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Terminating (Success $ BoolVal B.Top)
            Left b -> print b
        Left a -> print $ showError a        

    it "unifying two list of nums of different size should result in list of nums" $ do
      file_str <- helper_import "//scheme_files//test_faulty_list.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> 
              let ?sensitivity = 0 in evalInterval' [] [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Terminating (Success $ ListVal NumVal)
            Left b -> print b
        Left a -> print $ showError a     

    it "test simple num lists" $ do
      file_str <- helper_import "//scheme_files//test_simple_list.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> 
              let ?sensitivity = 0 in evalInterval' [] [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Terminating (Success $ ListVal NumVal)
            Left b -> print b
        Left a -> print $ showError a             

    it "test_empty_lists" $ do
      file_str <- helper_import "//scheme_files//test_empty_lists.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> 
              let ?sensitivity = 0 in evalInterval' [] [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Terminating (Fail "{\"empty list\"}")
            Left b -> print b
        Left a -> print $ showError a

    it "test_opvars" $ do
          file_str <- helper_import "//scheme_files//test_opvars.scm"
          case readExprList file_str of
            Right a ->
              case match a of
                Right b -> 
                  let ?sensitivity = 0 in evalInterval' [] [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Terminating (Success $ NumVal)
                Left b -> print b
            Left a -> print $ showError a           


-------------------HELPER-------------------------------------------------------
helper_import :: String -> IO String
helper_import inFile = do
  root <- getCurrentDirectory
  let root' = root ++ inFile
  readCreateProcess (shell $ "raco expand " ++ root') ""


    -- it "should execute both branches on IfZero on interval containing zero" $
    --   let ?bound = I.Interval (-100) 100
    --       ?sensitivity = 1
    --   in evalInterval' [("x", num (-5) 5)]
    --       (ifZero "x" (succ zero) (pred zero))
    --       `shouldBe` Terminating (Success (num (-1) 1))

    -- it "should compute 0 + -1 + 1 = 0" $
    --   let ?bound = I.Interval (-100) 100
    --       ?sensitivity = 1
    --   in evalInterval' [] (succ (pred zero)) `shouldBe`
    --        Terminating (Success (num 0 0))

    -- it "should analyse addition correctly" $
    --   let ?bound = I.Interval 0 5Bool
    --       ?sensitivity = 2
    --   in do
    --     evalInterval' [] (let_ [("add",add)] (app "add" [zero,two])) `shouldBe` Terminating (Success (num 2 2))
    --     evalInterval' [] (let_ [("add",add)] (app "add" [one,two])) `shouldBe` Terminating (Success (num 3 3))
    --     evalInterval' [("x", num 0 1)] (let_ [("add",add)] (app "add" ["x",two]))
    --       -- Most precise would be [2,3], however, the analysis does not refine
    --       -- `x` and therefore introduces some imprecision.
    --       `shouldBe` Terminating (Success (num 2 Infinity))

    -- it "context sensitivity" $
    --   let diamond = let_ [("second",second),("id",id)] (app "second" [app "id" [one],app "id" [two]]) in
    --   let ?bound = I.Interval 0 5 in do
    --   let ?sensitivity = 0 in evalInterval' [] diamond `shouldBe` Terminating (Success (num 1 2))
    --   let ?sensitivity = 1 in evalInterval' [] diamond `shouldBe` Terminating (Success (num 2 2))

    -- it "context sensitivity reverse testing" $ 
    --   let diamond = let_ [("first",first),("id",id)] (app "first" [app "id" [one],app "id" [two]]) in
    --   let ?bound = I.Interval 0 5 in do
    --   let ?sensitivity = 0 in evalInterval' [] diamond `shouldBe` Terminating (Success (num 1 2))
    --   let ?sensitivity = 1 in evalInterval' [] diamond `shouldBe` Terminating (Success (num 1 1)) 

    -- it "should terminate for the non-terminating program" $
    --   let ?bound = I.Interval 0 5
    --       ?sensitivity = 2
    --   in evalInterval' [] (let_ [("id", lam ["x"] "x"),
    --                             ("fix",lam ["x"] (app "fix" ["x"]))]
    --                      (app "fix" ["id"]))
    --        `shouldBe` NonTerminating

    -- num i j = NumVal $ I.Interval i j

    -- toEither :: Terminating (Error (Pow String) a) -> Either String a
    -- toEither (Terminating (Fail e)) = Left (unwords (toList e))
    -- toEither (Terminating (Success x)) = Right x
    -- toEither NonTerminating = Left "NonTerminating"
