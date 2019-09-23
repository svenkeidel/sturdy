{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ConcreteInterpreterSpec(main, spec) where

import           Prelude hiding (map,or,and,error,seq,fail)

import           ConcreteInterpreter (eval)
import qualified ConcreteInterpreter as C
import           Syntax
import qualified Syntax as T

import           Control.Monad(when)

import           Data.Concrete.Error as E
import           Data.Concrete.Error as F
import           Data.Term (TermUtils(..))
import qualified Data.HashMap.Lazy as M
import           Data.Empty

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (Success,output)

import qualified CaseStudy

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "scope" $ do
    it "should hide declare variables" $ do
      let tenv = termEnv [("x", term1)]
      -- eval (Scope ["x"] (build "x")) empty tenv term2
      --   `shouldBe` error "unbound term variable x in build statement !x"
      eval (scope ["x"] (match "x")) empty tenv term2
        `shouldBe` success (tenv,term2)

    it "should make non-declared variables available" $ do
      let tenv = termEnv [("x", term1)]
      eval (scope ["y"] (build "x")) empty tenv term2 `shouldBe`
        success (tenv,term1)
      eval (scope ["y"] (match "z")) empty tenv term2 `shouldBe`
        success (termEnv [("x", term1), ("z", term2)],term2)

    it "should hide variables bound in a choice's test from the else branch" $
      let or1 = build (T.Cons "Zero" []) `seq` match "x" `seq` fail in
      let or2 = match "x" in
      eval (or1 `leftChoice` or2) empty (termEnv []) term1 `shouldBe` success (termEnv [("x", term1)], term1)

  describe "let" $
    it "should support recursion" $ do
      let t = convertToList (C.NumberLiteral <$> [2, 3, 4])
          tenv = termEnv []; tenv' = termEnv [("x",t)]
      eval (let_ [("map", map)] (match "x" `seq` call "map" [build 1] ["x"])) empty tenv t
        `shouldBe`
           success (tenv', convertToList [1, 1, 1])

  describe "call" $
    it "should support recursion" $ do
      let senv = stratEnv [("map", map)]
          t = convertToList [2, 3, 4]
          tenv = termEnv []; tenv' = termEnv [("x",t)]
      eval (match "x" `seq` call "map" [build (T.NumberLiteral 1)] ["x"]) senv tenv t
        `shouldBe`
           success (tenv', convertToList [1, 1, 1])

  describe "match" $ do
    prop "should introduce variables" $ \t ->
      let tenv = termEnv []
      in eval (match "x" `seq` match "y") empty tenv t `shouldBe`
           success (termEnv [("x", t), ("y", t)], t)

    prop "should support linear pattern matching" $ \t1 t2 ->
      let t' = C.Cons "f" [t1,t2]
          tenv = termEnv []; tenv' = termEnv [("x",t1)]
      in eval (match (T.Cons "f" ["x","x"])) empty tenv t' `shouldBe`
           if t1 == t2 then success (tenv', t') else uncaught ()

    prop "should match deep" $ \t -> do
      p <- C.similarTermPattern t 3
      return $ counterexample (show p) $ when (linear p) $
        fmap (fmap snd) (eval (match p) empty (termEnv []) t) `shouldBe`
          success t

    it "should succeed when exploding literals" $
      let tenv = termEnv []; tenv' = termEnv [("x", C.Cons "Nil" [])]
      in eval (match (T.Explode "_" "x")) empty tenv 1 `shouldBe`
           success (tenv', 1)

  describe "build" $ do
    prop "build should be inverse to match" $ \t -> do
      p <- C.similarTermPattern t 3
      return $ counterexample (show p) $ when (linear p) $
        fmap (fmap snd) (eval (match p `seq` build p) empty (termEnv []) t) `shouldBe`
          success t

    prop "build should lookup variables" $ \t -> do
      let tenv = termEnv [("x", t)]
      eval (build (T.Var "x")) empty tenv t `shouldBe`
        success (tenv,t)


  describe "simplify arithmetics" $ do
    it "reduce Add(Zero,y)" $
      let input  = C.Cons "Add" [C.Cons "Zero" [], C.Cons "Add" [C.Cons "One" [], C.Cons "One" []]] in
      let output = C.Cons "Add" [C.Cons "One" [], C.Cons "One" []] in
      let reduce = match (T.Cons "Add" [T.Cons "Zero" [], "y"]) `seq` build "y" in
      eval reduce empty (termEnv []) input `shouldBe` success (termEnv [("y", output)], output)

    it "reduce Add(x,Zero)" $
      let input  = C.Cons "Add" [C.Cons "Add" [C.Cons "One" [], C.Cons "One" []], C.Cons "Zero" []] in
      let output = C.Cons "Add" [C.Cons "One" [], C.Cons "One" []] in
      let reduce = match (T.Cons "Add" ["x", T.Cons "Zero" []]) `seq` build "x" in
      eval reduce empty (termEnv []) input `shouldBe` success (termEnv [("x", output)], output)


    it "reduce Add(Zero,y) < id + Add(x,Zero) 1" $
      let input  = C.Cons "Add" [C.Cons "Zero" [], C.Cons "Add" [C.Cons "One" [], C.Cons "One" []]] in
      let output = C.Cons "Add" [C.Cons "One" [], C.Cons "One" []] in
      let reduce1 = match (T.Cons "Add" [T.Cons "Zero" [], "y"]) `seq` build "y" in
      let reduce2 = match (T.Cons "Add" ["x", T.Cons "Zero" []]) `seq` build "x" in
      eval (reduce1 `leftChoice` reduce2) empty (termEnv []) input `shouldBe` success (termEnv [("y", output)], output)

    it "reduce Add(Zero,y) < id + Add(x,Zero) 2" $
      let input  = C.Cons "Add" [C.Cons "Add" [C.Cons "One" [], C.Cons "One" []], C.Cons "Zero" []] in
      let output = C.Cons "Add" [C.Cons "One" [], C.Cons "One" []] in
      let reduce1 = match (T.Cons "Add" [T.Cons "Zero" [], "y"]) `seq` build "y" in
      let reduce2 = match (T.Cons "Add" ["x", T.Cons "Zero" []]) `seq` build "x" in
      eval (reduce1 `leftChoice` reduce2) empty (termEnv []) input `shouldBe` success (termEnv [("x", output)], output)

    it "reduce Add(x,y); !x; ?Zero()" $
      let input  = C.Cons "Add" [C.Cons "Zero" [], C.Cons "Add" [C.Cons "One" [], C.Cons "One" []]] in
      let output = C.Cons "Add" [C.Cons "One" [], C.Cons "One" []] in
      let reduce = match (T.Cons "Add" ["x", "y"]) `seq` build "x" `seq` match (T.Cons "Zero" []) `seq` build "y" in
      eval reduce empty (termEnv []) input `shouldBe` success (termEnv [("x", C.Cons "Zero" []),("y", output)], output)

    it "reduce Double(x)" $
      let input  = C.Cons "Double" [C.Cons "Add" [C.Cons "One" [], C.Cons "One" []]] in
      let output = C.Cons "Add" [C.Cons "Add" [C.Cons "One" [], C.Cons "One" []], C.Cons "Add" [C.Cons "One" [], C.Cons "One" []]] in
      let reduce = match (T.Cons "Double" ["x"]) `seq` build (T.Cons "Add" ["x", "x"]) in
      eval reduce empty (termEnv []) input `shouldBe` success (termEnv [("x", C.Cons "Add" [C.Cons "One" [], C.Cons "One" []])], output)

    it "reduce Add(Zero,y) <+ Add(x,Zero) <+ Double(x) 1" $
      let input  = C.Cons "Add" [C.Cons "Zero" [], C.Cons "Add" [C.Cons "One" [], C.Cons "One" []]] in
      let output = C.Cons "Add" [C.Cons "One" [], C.Cons "One" []] in
      let reduce1 = match (T.Cons "Add" [T.Cons "Zero" [], "y"]) `seq` build "y" in
      let reduce2 = match (T.Cons "Add" ["x", T.Cons "Zero" []]) `seq` build "x" in
      let reduce3 = match (T.Cons "Double" ["x"]) `seq` build (T.Cons "Add" ["x", "x"]) in
      eval (reduce1 `leftChoice` reduce2 `leftChoice` reduce3) empty (termEnv []) input `shouldBe` success (termEnv [("y", output)], output)

    it "reduce Add(Zero,y) <+ Add(x,Zero) <+ Double(x) 2" $
      let input  = C.Cons "Add" [C.Cons "Add" [C.Cons "One" [], C.Cons "One" []], C.Cons "Zero" []] in
      let output = C.Cons "Add" [C.Cons "One" [], C.Cons "One" []] in
      let reduce1 = match (T.Cons "Add" [T.Cons "Zero" [], "y"]) `seq` build "y" in
      let reduce2 = match (T.Cons "Add" ["x", T.Cons "Zero" []]) `seq` build "x" in
      let reduce3 = match (T.Cons "Double" ["x"]) `seq` build (T.Cons "Add" ["x", "x"]) in
      eval (reduce1 `leftChoice` reduce2 `leftChoice` reduce3) empty (termEnv []) input `shouldBe` success (termEnv [("x", output)], output)

    it "reduce Add(Zero,y) <+ Add(x,Zero) <+ Double(x) 3" $
      let input  = C.Cons "Double" [C.Cons "Add" [C.Cons "One" [], C.Cons "One" []]] in
      let output = C.Cons "Add" [C.Cons "Add" [C.Cons "One" [], C.Cons "One" []], C.Cons "Add" [C.Cons "One" [], C.Cons "One" []]] in
      let reduce1 = match (T.Cons "Add" [T.Cons "Zero" [], "y"]) `seq` build "y" in
      let reduce2 = match (T.Cons "Add" ["x", T.Cons "Zero" []]) `seq` build "x" in
      let reduce3 = match (T.Cons "Double" ["x"]) `seq` build (T.Cons "Add" ["x", "x"]) in
      eval (reduce1 `leftChoice` reduce2 `leftChoice` reduce3) empty (termEnv []) input `shouldBe` success (termEnv [("x", C.Cons "Add" [C.Cons "One" [], C.Cons "One" []])], output)

  describe "Case Studies" $ do
    describe "Haskell Arrows" $ beforeAll CaseStudy.arrows $ do

      it "union should work" $ \(_,arrows) -> do
        let l1 = convertToList [1,2,3,4]
            l2 = convertToList [2,4]
            t = tup l1 l2
            tenv = termEnv []
        eval (call "union_0_0" [] []) arrows tenv t
          `shouldBe`
             success (tenv, convertToList [1,3,2,4])

      it "concat should work" $ \(_,arrows) ->
        let l = convertToList (fmap convertToList [[1,2,3],[4,5],[],[6]])
            tenv = termEnv []
        in eval (call "concat_0_0" [] []) arrows tenv l
          `shouldBe`
             success (tenv, convertToList [1,2,3,4,5,6])

      it "free-pat-vars should work" $ \(_,arrows) ->
        let var x = C.Cons "VarVar" [x]
            tuple x y = C.Cons "Tuple" [x,convertToList y]
            t = tuple (tuple (var "a") [var "b"])
                      [tuple (var "c") [var "a"]]
            tenv = termEnv []
        in eval (call "free_pat_vars_0_0" [] []) arrows tenv t
            `shouldBe`
               success (tenv, convertToList [var "b", var "c", var "a"])

    describe "NNF" $ beforeAll CaseStudy.nnf $ do
      let neg t = C.Cons "Neg" [t]
          or t1 t2 = C.Cons "Or" [t1,t2]
          and t1 t2 = C.Cons "And" [t1,t2]
          atom s = C.Cons "Atom" [C.StringLiteral s]
      it "¬((φ ∧ ψ) ∨ θ) =  (¬φ ∨ ¬ψ) ∧ ¬θ" $ \(_,nnf) ->
        let t = neg (or (and (atom "φ") (atom "ψ")) (atom "θ"))
            t' = and (or (neg (atom "φ")) (neg (atom "ψ"))) (neg (atom "θ"))
            tenv = termEnv []
        in eval (call "main_0_0" [] []) nnf tenv t
             `shouldBe` success (tenv, t')

      it "¬(¬(φ ∧ ψ) ∨ θ) = (φ ∧ ψ) ∧ ¬θ" $ \(_,nnf) ->
        let t = neg (or (neg (and (atom "φ") (atom "ψ"))) (atom "θ"))
            t' = and (and (atom "φ") (atom "ψ")) (neg (atom "θ"))
            tenv = termEnv []
        in eval (call "main_0_0" [] []) nnf tenv t
             `shouldBe` success (tenv, t')
  where
    map = strategy ["f"] ["l"] (scope ["x","xs","x'","xs'"] (
            build "l" `seq`
            guardedChoice
              (match (T.Cons "Cons" ["x","xs"]))
              (build "x" `seq`
               call "f" [] [] `seq`
               match "x'" `seq`
               call "map" [call "f" [] []] ["xs"] `seq`
               match "xs'" `seq`
               build (T.Cons "Cons" ["x'", "xs'"]))
              (build (T.Cons "Nil" []))))

    termEnv :: [(TermVar,C.Term)] -> C.TermEnv
    termEnv = M.fromList

    term1 = C.NumberLiteral 1
    term2 = C.NumberLiteral 2

    tup x y = C.Cons "" [x,y]

    success :: a -> Error String (Error () a)
    success a = F.Success $ E.Success a

    -- error :: String -> Error String a
    -- error = F.Fail

    uncaught :: () -> Error String (Error () a)
    uncaught = F.Success . E.Fail
