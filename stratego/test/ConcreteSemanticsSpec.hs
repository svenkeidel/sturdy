{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ConcreteSemanticsSpec(main, spec) where

import           Prelude hiding (map,or,and,error)

import           ConcreteSemantics
import           Syntax hiding (Fail,TermPattern(..),StringLiteral)
import qualified Syntax as T

import           Control.Monad

import           Data.Concrete.Error as E
import           Data.Concrete.Error as F
import           Data.Term (TermUtils(..))
import qualified Data.HashMap.Lazy as M

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
      -- eval (Scope ["x"] (Build "x")) M.empty tenv term2 
      --   `shouldBe` error "unbound term variable x in build statement !x"
      eval (Scope ["x"] (Match "x")) M.empty tenv term2
        `shouldBe` success (tenv,term2)

    it "should make non-declared variables available" $ do
      let tenv = termEnv [("x", term1)]
      eval (Scope ["y"] (Build "x")) M.empty tenv term2 `shouldBe`
        success (tenv,term1)
      eval (Scope ["y"] (Match "z")) M.empty tenv term2 `shouldBe`
        success (termEnv [("x", term1), ("z", term2)],term2)

    it "should hide variables bound in a choice's test from the else branch" $
      let or1 = Build (T.Cons "Zero" []) `Seq` Match "x" `Seq` T.Fail in
      let or2 = Match "x" in
      eval (or1 `leftChoice` or2) M.empty (termEnv []) term1 `shouldBe` success (termEnv [("x", term1)], term1)

  describe "let" $
    it "should support recursion" $ do
      let t = convertToList (NumberLiteral <$> [2, 3, 4])
          tenv = termEnv []; tenv' = termEnv [("x",t)]
      eval (Let [("map", map)] (Match "x" `Seq` Call "map" [Build 1] ["x"])) M.empty tenv t
        `shouldBe`
           success (tenv', convertToList [1, 1, 1])

  describe "call" $
    it "should support recursion" $ do
      let senv = M.fromList [("map", Closure map M.empty)]
          t = convertToList [2, 3, 4]
          tenv = termEnv []; tenv' = termEnv [("x",t)]
      eval (Match "x" `Seq` Call "map" [Build (T.NumberLiteral 1)] ["x"]) senv tenv t
        `shouldBe`
           success (tenv', convertToList [1, 1, 1])

  describe "match" $ do
    prop "should introduce variables" $ \t ->
      let tenv = termEnv [] 
      in eval (Match "x" `Seq` Match "y") M.empty tenv t `shouldBe`
           success (termEnv [("x", t), ("y", t)], t)

    prop "should support linear pattern matching" $ \t1 t2 ->
      let t' = Cons "f" [t1,t2]
          tenv = termEnv []; tenv' = termEnv [("x",t1)]
      in eval (Match (T.Cons "f" ["x","x"])) M.empty tenv t' `shouldBe`
           if t1 == t2 then success (tenv', t') else uncaught ()

    prop "should match deep" $ \t -> do
      p <- similarTermPattern t 3
      return $ counterexample (show p) $ when (linear p) $
        fmap (fmap snd) (eval (Match p) M.empty (termEnv []) t) `shouldBe`
          success t

    it "should succeed when exploding literals" $
      let tenv = termEnv []; tenv' = termEnv [("x", Cons "Nil" [])]
      in eval (Match (T.Explode "_" "x")) M.empty tenv 1 `shouldBe`
           success (tenv', 1)

  describe "build" $ do
    prop "build should be inverse to match" $ \t -> do
      p <- similarTermPattern t 3
      return $ counterexample (show p) $ when (linear p) $
        fmap (fmap snd) (eval (Match p `Seq` Build p) M.empty (termEnv []) t) `shouldBe`
          success t

    prop "build should lookup variables" $ \t -> do
      let tenv = termEnv [("x", t)]
      eval (Build (T.Var "x")) M.empty tenv t `shouldBe`
        success (tenv,t)


  describe "simplify arithmetics" $ do
    it "reduce Add(Zero,y)" $
      let input  = Cons "Add" [Cons "Zero" [], Cons "Add" [Cons "One" [], Cons "One" []]] in
      let output = Cons "Add" [Cons "One" [], Cons "One" []] in
      let reduce = Match (T.Cons "Add" [T.Cons "Zero" [], "y"]) `Seq` Build "y" in
      eval (reduce) M.empty (termEnv []) input `shouldBe` success (termEnv [("y", output)], output)

    it "reduce Add(x,Zero)" $
      let input  = Cons "Add" [Cons "Add" [Cons "One" [], Cons "One" []], Cons "Zero" []] in
      let output = Cons "Add" [Cons "One" [], Cons "One" []] in
      let reduce = Match (T.Cons "Add" ["x", T.Cons "Zero" []]) `Seq` Build "x" in
      eval (reduce) M.empty (termEnv []) input `shouldBe` success (termEnv [("x", output)], output)


    it "reduce Add(Zero,y) < id + Add(x,Zero) 1" $
      let input  = Cons "Add" [Cons "Zero" [], Cons "Add" [Cons "One" [], Cons "One" []]] in
      let output = Cons "Add" [Cons "One" [], Cons "One" []] in
      let reduce1 = Match (T.Cons "Add" [T.Cons "Zero" [], "y"]) `Seq` Build "y" in
      let reduce2 = Match (T.Cons "Add" ["x", T.Cons "Zero" []]) `Seq` Build "x" in
      eval (reduce1 `leftChoice` reduce2) M.empty (termEnv []) input `shouldBe` success (termEnv [("y", output)], output)

    it "reduce Add(Zero,y) < id + Add(x,Zero) 2" $
      let input  = Cons "Add" [Cons "Add" [Cons "One" [], Cons "One" []], Cons "Zero" []] in
      let output = Cons "Add" [Cons "One" [], Cons "One" []] in
      let reduce1 = Match (T.Cons "Add" [T.Cons "Zero" [], "y"]) `Seq` Build "y" in
      let reduce2 = Match (T.Cons "Add" ["x", T.Cons "Zero" []]) `Seq` Build "x" in
      eval (reduce1 `leftChoice` reduce2) M.empty (termEnv []) input `shouldBe` success (termEnv [("x", output)], output)

    it "reduce Add(x,y); !x; ?Zero()" $
      let input  = Cons "Add" [Cons "Zero" [], Cons "Add" [Cons "One" [], Cons "One" []]] in
      let output = Cons "Add" [Cons "One" [], Cons "One" []] in
      let reduce = Match (T.Cons "Add" ["x", "y"]) `Seq` Build "x" `Seq` Match (T.Cons "Zero" []) `Seq` Build "y" in
      eval (reduce) M.empty (termEnv []) input `shouldBe` success (termEnv [("x", Cons "Zero" []),("y", output)], output)

    it "reduce Double(x)" $
      let input  = Cons "Double" [Cons "Add" [Cons "One" [], Cons "One" []]] in
      let output = Cons "Add" [Cons "Add" [Cons "One" [], Cons "One" []], Cons "Add" [Cons "One" [], Cons "One" []]] in
      let reduce = Match (T.Cons "Double" ["x"]) `Seq` Build (T.Cons "Add" ["x", "x"]) in
      eval (reduce) M.empty (termEnv []) input `shouldBe` success (termEnv [("x", Cons "Add" [Cons "One" [], Cons "One" []])], output)

    it "reduce Add(Zero,y) <+ Add(x,Zero) <+ Double(x) 1" $
      let input  = Cons "Add" [Cons "Zero" [], Cons "Add" [Cons "One" [], Cons "One" []]] in
      let output = Cons "Add" [Cons "One" [], Cons "One" []] in
      let reduce1 = Match (T.Cons "Add" [T.Cons "Zero" [], "y"]) `Seq` Build "y" in
      let reduce2 = Match (T.Cons "Add" ["x", T.Cons "Zero" []]) `Seq` Build "x" in
      let reduce3 = Match (T.Cons "Double" ["x"]) `Seq` Build (T.Cons "Add" ["x", "x"]) in
      eval (reduce1 `leftChoice` reduce2 `leftChoice` reduce3) M.empty (termEnv []) input `shouldBe` success (termEnv [("y", output)], output)

    it "reduce Add(Zero,y) <+ Add(x,Zero) <+ Double(x) 2" $
      let input  = Cons "Add" [Cons "Add" [Cons "One" [], Cons "One" []], Cons "Zero" []] in
      let output = Cons "Add" [Cons "One" [], Cons "One" []] in
      let reduce1 = Match (T.Cons "Add" [T.Cons "Zero" [], "y"]) `Seq` Build "y" in
      let reduce2 = Match (T.Cons "Add" ["x", T.Cons "Zero" []]) `Seq` Build "x" in
      let reduce3 = Match (T.Cons "Double" ["x"]) `Seq` Build (T.Cons "Add" ["x", "x"]) in
      eval (reduce1 `leftChoice` reduce2 `leftChoice` reduce3) M.empty (termEnv []) input `shouldBe` success (termEnv [("x", output)], output)

    it "reduce Add(Zero,y) <+ Add(x,Zero) <+ Double(x) 3" $
      let input  = Cons "Double" [Cons "Add" [Cons "One" [], Cons "One" []]] in
      let output = Cons "Add" [Cons "Add" [Cons "One" [], Cons "One" []], Cons "Add" [Cons "One" [], Cons "One" []]] in
      let reduce1 = Match (T.Cons "Add" [T.Cons "Zero" [], "y"]) `Seq` Build "y" in
      let reduce2 = Match (T.Cons "Add" ["x", T.Cons "Zero" []]) `Seq` Build "x" in
      let reduce3 = Match (T.Cons "Double" ["x"]) `Seq` Build (T.Cons "Add" ["x", "x"]) in
      eval (reduce1 `leftChoice` reduce2 `leftChoice` reduce3) M.empty (termEnv []) input `shouldBe` success (termEnv [("x", Cons "Add" [Cons "One" [], Cons "One" []])], output)

  describe "Case Studies" $ do
    describe "Haskell Arrows" $ beforeAll CaseStudy.arrows $ do

      it "union should work" $ \arrows -> do
        let l1 = convertToList [1,2,3,4]
            l2 = convertToList [2,4]
            t = tup l1 l2
            tenv = termEnv []
        eval (Call "union_0_0" [] []) (stratEnv arrows) tenv t
          `shouldBe`
             success (tenv, convertToList [1,3,2,4])

      it "concat should work" $ \arrows ->
        let l = convertToList (fmap convertToList [[1,2,3],[4,5],[],[6]])
            tenv = termEnv []
        in eval (Call "concat_0_0" [] []) (stratEnv arrows) tenv l
          `shouldBe`
             success (tenv, convertToList [1,2,3,4,5,6])

      it "free-pat-vars should work" $ \arrows ->
        let var x = Cons "VarVar" [x]
            tuple x y = Cons "Tuple" [x,convertToList y]
            t = tuple (tuple (var "a") [var "b"])
                      [tuple (var "c") [var "a"]]
            tenv = termEnv []
        in eval (Call "free_pat_vars_0_0" [] []) (stratEnv arrows) tenv t
            `shouldBe`
               success (tenv, convertToList [var "b", var "c", var "a"])

    describe "NNF" $ beforeAll CaseStudy.nnf $ do
      let neg t = Cons "Neg" [t]
          or t1 t2 = Cons "Or" [t1,t2]
          and t1 t2 = Cons "And" [t1,t2]
          atom s = Cons "Atom" [StringLiteral s]
      it "¬((φ ∧ ψ) ∨ θ) =  (¬φ ∨ ¬ψ) ∧ ¬θ" $ \nnf ->
        let t = neg (or (and (atom "φ") (atom "ψ")) (atom "θ"))
            t' = and (or (neg (atom "φ")) (neg (atom "ψ"))) (neg (atom "θ"))
            tenv = termEnv []
        in eval (Call "main_0_0" [] []) (stratEnv nnf) tenv t
             `shouldBe` success (tenv, t')

      it "¬(¬(φ ∧ ψ) ∨ θ) = (φ ∧ ψ) ∧ ¬θ" $ \nnf ->
        let t = neg (or (neg (and (atom "φ") (atom "ψ"))) (atom "θ"))
            t' = and (and (atom "φ") (atom "ψ")) (neg (atom "θ"))
            tenv = termEnv []
        in eval (Call "main_0_0" [] []) (stratEnv nnf) tenv t
             `shouldBe` success (tenv, t')


  where

    map = Strategy ["f"] ["l"] (Scope ["x","xs","x'","xs'"] (
            Build "l" `Seq`
            GuardedChoice
              (Match (T.Cons "Cons" ["x","xs"]))
              (Build "x" `Seq`
               Call "f" [] [] `Seq`
               Match "x'" `Seq`
               Call "map" ["f"] ["xs"] `Seq`
               Match "xs'" `Seq`
               Build (T.Cons "Cons" ["x'", "xs'"]))
              (Build (T.Cons "Nil" []))))

    termEnv = TermEnv . M.fromList
    
    term1 = NumberLiteral 1
    term2 = NumberLiteral 2

    tup x y = Cons "" [x,y]

    success :: a -> Error String (Error () a)
    success a = F.Success $ E.Success a

    error :: String -> Error String a
    error = F.Fail
    
    uncaught :: () -> Error String (Error () a)
    uncaught = F.Success . E.Fail
