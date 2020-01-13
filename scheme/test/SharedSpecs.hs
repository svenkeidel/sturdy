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
  describe "Gabriel Benchmarks" $ do
    it "cpstak" $ do 
      -- pendingWith "passes, but takes veeery long"
      file_str <- helper_import "//scheme_files//gabriel//cpstak.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> run [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Right (NumVal 6)
            Left b -> print b
        Left a -> print $ showError a

    it "deriv" $ do
      file_str <- helper_import "//scheme_files//gabriel//deriv.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> run [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Right (BoolVal True)
            Left b -> print b
        Left a -> print $ showError a

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
      -- pendingWith "passes, but takes veeery long"
      file_str <- helper_import "//scheme_files//gabriel//takl.scm"
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> run [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Right (BoolVal True)
            Left b -> print b
        Left a -> print $ showError a

-------------------SCALA-AM BENCHMARKS------------------------------------------
  describe "Scala-AM Benchmarks" $ do
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

  describe "Custom_Tests" $ 
    it "let statements should extend the environment with bindings" $ do
      pendingWith "insert test"
      file_str <- helper_import "//scheme_files//test.scm"
      putStrLn file_str
      case readExprList file_str of
        Right a ->
          case match a of
            Right b -> do
              putStrLn (show b)
              let es = let_rec (getTopDefinesLam b) (getBody b)
              putStr $ show $ generate es
              run [let_rec (getTopDefinesLam b) (getBody b)] `shouldBe` Right (BoolVal True)
            Left b -> print b
        Left a -> print $ showError a

-------------------HELPER-------------------------------------------------------
helper_import :: String -> IO String
helper_import inFile = do
  root <- getCurrentDirectory
  let root' = root ++ inFile
  readCreateProcess (shell $ "raco expand " ++ root') ""
