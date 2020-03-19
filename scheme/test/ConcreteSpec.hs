{-# LANGUAGE OverloadedStrings #-}
module ConcreteSpec where

import           Prelude hiding (succ,pred)

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M

import           Syntax as S hiding (Nil)
import           Parser
import           ConcreteInterpreter

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "miscellaneous tests" $ do
    let run = evalConcrete''

    it "test set! " $
      run [define "x" (lit $ S.Int 2),
          define "y" (lit $ S.Int 3),
          set "y" (lit $ S.Int 3)] `shouldBe` Right (StringVal "#<void>")

    it "test simple let" $ do
      let es' = [let_ [("y", lit $ S.Int 3)] ["y"] ]
      run es' `shouldBe` Right (IntVal 3)

    it "test simple letrec" $ do
      let es' = [let_rec [("y", lit $ S.Int 3)] ["y"] ]
      run es' `shouldBe` Right (IntVal 3)

    it "test simple lambda" $ do
      let es' = [app (lam ["x"] ["x"]) [lit $ S.Int 3]]
      run es' `shouldBe` Right (IntVal 3)


    it "test define with lambda body  " $ do
      let es' = [let_rec [("y", lam ["y"] ["y"])]
                      [app "y" [lit $ S.Int 3]]]
      run es' `shouldBe` Right (IntVal 3)

    it "test define with lambda body  " $ do
      let es' = [let_ [("x", lam ["y"] ["y"])]
                      [app "x" [lit $ S.Int 3]]]
      run es' `shouldBe` Right (IntVal 3)


    it "demonstrate need for stepwise evaluation of letrec bindings" $ do
      let es' = [let_rec [("x", lit $ S.Int 4),
                          ("y", "x")]
                      --  [lam ["x"] ["x"]]]
                          ["y"]]
      run es' `shouldBe` Right (IntVal 4)

    it "test define with lambda body  " $ do
      let es = [define "y" (lam ["x"] ["x"]),
                define "z" (lit $ S.Int 4),
                define "z1" (lit $ S.Int 5),
                define "k" (lit $ S.Int 6),
                app "y" [lit $ S.Int 3]]
      run es `shouldBe` Right (IntVal 3)

    it "test define with lambda body  " $
      run [define "x" (lam ["n"] [app (lam ["x"] ["x"]) ["n"]]),
          app "x" [lit $ S.Int 2],
          app "x" [lit $ S.Int 3]] `shouldBe` Right (IntVal 3)

    it "test define with lambda body  " $ do
      let es = [define "x" (lam ["x"] ["x"]),
                app "x" [lit $ S.Int 3]]
      run es `shouldBe` Right (IntVal 3)

    it "test define with lambda body  " $ do
      let es = [let_ [("y", lam ["y"] ["y"])]
                     [define "x" (lam ["x"] ["x"]),
                     app "x" [lit $ S.Int 3]]]
      run es `shouldBe` Right (IntVal 3)

    it "illustrate necessity of store" $
      run [define "x" (lit $ S.Int 2),
          app
            (lam [] [set "x" (lit $ S.Int 3)])
            [],
            "x"] `shouldBe` Right (IntVal 3)

    it "correctly evaluate sequential letrec" $
      run [let_rec [("x", lit $ S.Int 3),
                    ("y", lam [] ["x"])]
              [app "y" []]] `shouldBe` Right (IntVal 3)

    it "correctly evaluate mutually recursive letrec" $
      run [let_rec [("y", lam [] ["x"]),
                    ("x", lit $ S.Int 3)]
              [app "y" []]] `shouldBe` Right (IntVal 3)

    it "should fail define the same var twice" $ do
      pendingWith "need to fix check in define "
      run [define "x" (lit $ S.Int 2),
          define "x" (lit $ S.Int 3)] `shouldBe` Left (M.fromList [], "Variable \"x\" already exists")

  -----------------GABRIEL BENCHMARKS---------------------------------------------
  describe "Gabriel-Benchmarks" $ do

    it "boyer" $ do
      pendingWith "no random"
      "gabriel/boyer.scm" `shouldEvaluateTo` Right (BoolVal True)

    it "cpstak" $
      "gabriel/cpstak.scm" `shouldEvaluateTo` Right (IntVal 6)

    it "deriv" $
      "gabriel/deriv.scm" `shouldEvaluateTo` Right (BoolVal True)

    it "diviter" $
      "gabriel/diviter.scm" `shouldEvaluateTo` Right (BoolVal True)

    it "divrec" $
      "gabriel/divrec.scm" `shouldEvaluateTo` Right (BoolVal True)

    it "takl" $
      "gabriel/takl.scm" `shouldEvaluateTo` Right (BoolVal True)

  -------------------SCALA-AM BENCHMARKS------------------------------------------
  describe "Scala-AM-Benchmarks" $ do
    it "collatz" $
      "scala-am/collatz.scm" `shouldEvaluateTo` Right (IntVal 5)

    it "gcipd" $
      "scala-am/gcipd.scm" `shouldEvaluateTo` Right (IntVal 36)

    it "nqueens" $
      "scala-am/nqueens.scm" `shouldEvaluateTo` Right (IntVal 92)

    it "rsa" $
      "scala-am/rsa.scm" `shouldEvaluateTo` Right (BoolVal True)

  ------------------------------------------CUSTOMS--------------------------------------------
  describe "Custom_Tests" $ do
      it "recursion and union with empty list" $
        "test_rec_empty.scm" `shouldEvaluateTo` Right (ListVal Nil)

      it "recursion and union with non-empty list" $
        "test_rec_nonempty.scm" `shouldEvaluateTo` Right (IntVal 1)

      it "should return 3 for (car (cdr '(2 3 4)))" $
        "test_cdr.scm" `shouldEvaluateTo` Right (IntVal 3)

      it "should return correct val for car" $
        "test_car.scm" `shouldEvaluateTo` Right (IntVal 1)

      it "should return true for null? cdr cdr '(1 2)" $
        "test_null.scm" `shouldEvaluateTo` Right (BoolVal True)

      -- it "unifying two list of nums of different size should result in an error" $ do
      --   let inFile = "test_faulty_list"
      --   let expRes = Terminating (Fail "{\"cannot unify lists of differing lengths| List [Int: {1}], List [Int: {1},Int: {2}]\"}")
      --   helper_test inFile expRes

      it "test_if" $
        "test_if.scm" `shouldEvaluateTo` Right (BoolVal False)

      it "test_opvars" $
        "test_opvars.scm" `shouldEvaluateTo` Right (IntVal 10)

      it "test_equal" $
        "test_equal.scm" `shouldEvaluateTo` Right (BoolVal True)

      it "test_cons" $
        "test_cons.scm" `shouldEvaluateTo` Right (BoolVal True)

      it "test_closures_gc" $
        "test_closure_gc.scm" `shouldEvaluateTo` Right (IntVal 16)

      it "lang_scheme_test" $
        "lang_scheme_test.scm" `shouldEvaluateTo` Right (IntVal 8)

      it "test_inner_define" $
        "test_inner_define.scm" `shouldEvaluateTo` Right (IntVal 10)

      it "test_subtraction" $
        "test_subtraction.scm" `shouldEvaluateTo` Right (IntVal (-4))

      it "test_lits" $
        "test_lits.scm" `shouldEvaluateTo` Right (IntVal 3)

      it "test_simple_floats" $
        "test_simple_floats.scm" `shouldEvaluateTo` Right (BoolVal False)

      it "test_rec_defines" $
        "test_rec_defines.scm" `shouldEvaluateTo` Right (IntVal 720)

      -- it "test_random" $ do
      --   let inFile = "test_random"
      --   let expRes = Right $ IntVal 1
      --   helper_test run inFile expRes


shouldEvaluateTo :: String -> Either (HashMap Addr Val, String) Val -> IO ()
shouldEvaluateTo inFile expRes = do
  prog <- loadSchemeFile inFile
  evalConcrete'' [prog] `shouldBe` expRes
