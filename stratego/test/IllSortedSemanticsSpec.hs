{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
module IllSortedSemanticsSpec(main, spec) where

import Prelude hiding (exp,seq,fail)

import           Syntax hiding (Fail)
import           Syntax as T
import           IllSortedSemantics
import           AbstractInterpreter
import           SortContext(Context,Sort(..))
import qualified SortContext as Ctx
import           Abstract.TermEnvironment

import           Control.Arrow

import           Data.Abstract.Stable(Stable(..))
import           Data.Abstract.FreeCompletion (fromCompletion)
import           Data.Abstract.Except as E
import           Data.Abstract.Error as F
import qualified Data.Abstract.Maybe as A
import qualified Data.Abstract.Map as S
import           Data.Abstract.Terminating (fromTerminating)
import qualified Data.HashMap.Lazy as M
import           Data.Set (Set)
import           Data.Order

import           Test.Hspec hiding (context)

import           GHC.Exts(IsString(..),IsList(..))
import           Text.Printf(printf)

import qualified CaseStudy

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  it "Widening" $ do
    let ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")]
    let expr = Sorted "Exp" ctx
    let top = Sorted Top ctx
    termWidening ctx 0 10 expr top `shouldBe` (Unstable,top)
    termWidening ctx 0 10 top expr `shouldBe` (Stable,top)
    termWidening ctx 0 10 (IllSorted [("Succ", [expr])]) (IllSorted [("Zero", [])]) `shouldBe` (Unstable, expr)
    termWidening ctx 1 10 (IllSorted [("Succ", [expr])]) (IllSorted [("Zero", [])]) `shouldBe` (Unstable, IllSorted [("Succ", [expr]), ("Zero", [])])
    termWidening ctx 0 10 (IllSorted [("Zero", [])]) (IllSorted [("Succ", [expr])]) `shouldBe` (Unstable, expr)
    termWidening ctx 0 10 expr (IllSorted [("Succ", [expr])]) `shouldBe` (Stable, expr)
    termWidening ctx 0 10 (IllSorted [("Succ", [expr])]) expr `shouldBe` (Unstable, expr)
    termWidening ctx 0 10 top (IllSorted [("Succ", [expr])]) `shouldBe` (Stable, top)
    termWidening ctx 0 10 (IllSorted [("Succ", [expr])]) top `shouldBe` (Unstable, top)


  describe "Utilities" $ do
    it "convertToList should work with identical sorts" $
      let ?ctx = Ctx.empty in
      convertToList [ lexical, lexical ] ?ctx `shouldBe` term (List Lexical)

    it "convertToList should work with different sorts" $
      let ?ctx = Ctx.empty in
      convertToList [ lexical, lexical, numerical ] ?ctx `shouldBe` term (List Top)

    it "convertToList should work on an empty list" $
      let ?ctx = Ctx.empty in
      convertToList [] ?ctx `shouldBe` term (List Bottom)

  describe "Match" $ do
    it "should match an identical builtin string literal" $
      let ?ctx = Ctx.empty in
      seval (match (StringLiteral "x")) lexical `shouldBe`
        successOrFail () (emptyEnv, lexical)

    it "should match another builtin string literal" $
      let ?ctx = Ctx.empty in
      seval (match (StringLiteral "y")) lexical `shouldBe`
        successOrFail () (emptyEnv, lexical)

    it "should match an equal builtin number literal" $
      let ?ctx = Ctx.empty in
      seval (match (NumberLiteral 42)) numerical `shouldBe`
        successOrFail () (emptyEnv, numerical)

    it "should match another builtin number literal" $
      let ?ctx = Ctx.empty in
      seval (match (NumberLiteral 1)) numerical `shouldBe`
        successOrFail () (emptyEnv, numerical)

    it "a string literal should not match a number literal" $
      let ?ctx = Ctx.empty in
      seval (match (NumberLiteral 1)) lexical `shouldBe`
        uncaught ()

    it "a number literal should not match a string literal" $
      let ?ctx = Ctx.empty in
      seval (match (StringLiteral "x")) numerical `shouldBe`
        uncaught ()

    it "should match a PCF expression" $
      let ?ctx = Ctx.fromList [("Zero",[],"Exp")] in
      let t = term "Exp" in
      seval (match (Cons "Zero" [])) t `shouldBe`
        success (emptyEnv,t)

    it "should match a nested PCF expression" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")] in
      let t = term "Exp" in
      seval (match (Cons "Succ" [Cons "Zero" []])) t `shouldBe`
        successOrFail () (emptyEnv, t)

    it "should match a constructor with more than one argument" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp") ,("Zero",[],"Exp") ,("Ifz",["Exp","Exp","Exp"],"Exp")] in
      let t = term "Exp" in
      seval (match (Cons "Ifz" [Cons "Zero" [], Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) t `shouldBe`
        successOrFail () (emptyEnv, t)

    it "should introduce one variable" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")] in
      let t = term "Exp" in
      seval (match (Cons "Succ" ["x"])) t `shouldBe`
        successOrFail () (termEnv [("x", t)], t)

    it "should introduce one variable 2" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")] in
      let t = term "Exp" in
      seval (match (Cons "Succ" ["x"])) t `shouldBe`
        successOrFail () (termEnv [("x", t)], t)

    it "should introduce multiple variables and support linear pattern matching" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")] in
      let t = term "Exp" in
      seval (match (Cons "Succ" ["x"]) `seq` match (Cons "Succ" ["y"])) t `shouldBe`
         successOrFail () (termEnv [("x", t), ("y", t)], t)

    it "should support linear pattern matching" $
      let ?ctx = Ctx.fromList [] in
      let t = term (Tuple [Lexical, Lexical]) in
      seval (match (Cons "" ["x", "x"])) t `shouldBe`
        successOrFail () (termEnv [("x",lexical)],t)

    it "should succeed when exploding literals" $
      let ?ctx = Ctx.empty in
      let tenv = termEnv [("x", term (List Bottom))] in
      seval' (match (Explode "_" "x")) (S.delete "x" emptyEnv) numerical `shouldBe`
        success (tenv, numerical)

    -- it "should handle inconsistent environments" $ do
    --   let t1 = C.Cons "f" []
    --       t2 = C.Cons "g" []
    --   sound' (match "x") [(t1, [("x", t1)]), (t2, [("y", t2)])]

    -- prop "should be sound" $ do
    --   [t1,t2,t3] <- C.similarTerms 3 7 2 10
    --   matchPattern <- C.similarTermPattern t1 3
    --   return $ counterexample
    --              (printf "pattern: %s\n %s ⊔ %s = %s"
    --                 (show matchPattern) (show t2) (show t3)
    --                 (showLub t2 t3))
    --          $ sound' (match matchPattern) [(t2,[]),(t3,[])]

  describe "Build" $ do
    it "should build a builtin string literal" $
      let ?ctx = Ctx.empty in
      seval (build (StringLiteral "foo")) bottom `shouldBe`
        success (emptyEnv, lexical)

    it "should build a builtin number literal" $
      let ?ctx = Ctx.empty in
      seval (build (NumberLiteral 1)) bottom `shouldBe`
        success (emptyEnv, numerical)

    it "a string grammar should not be build on a number literal" $
      let ?ctx = Ctx.empty in
      seval (build (NumberLiteral 1)) bottom `shouldNotBe`
        success (emptyEnv, lexical)

    it "a number grammar should not be build on a string literal" $
      let ?ctx = Ctx.empty in
      seval (match (StringLiteral "x")) bottom `shouldNotBe`
        success (emptyEnv, numerical)

    it "should build a simple constant PCF expression" $
      let ?ctx = Ctx.fromList [("Zero",[],"Exp")] in
      let t = bottom
          t' = term "Exp"
      in seval (build (Cons "Zero" [])) t `shouldBe` success (emptyEnv, t')

    it "should build a nested PCF expression" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")] in
      let t = bottom
          t' = term "Exp"
      in seval (build (Cons "Succ" [Cons "Zero" []])) t `shouldBe` success (emptyEnv, t')

    it "should build a constructor with more than one argument" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp") ,("Zero",[],"Exp") ,("Ifz",["Exp","Exp","Exp"],"Exp")] in
      let t = term "Exp"
      in seval (build (Cons "Ifz" [Cons "Zero" [], Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) t `shouldBe`
        success (emptyEnv, t)

    it "build should be inverse to match" $
      let ?ctx = Ctx.empty in
      let pat = NumberLiteral 1
      in seval (match pat `seq` build pat) numerical `shouldBe` successOrFail () (emptyEnv, numerical)

    it "build should be inverse to match with a more complicated term" $
      let ?ctx = Ctx.empty in
      let pat = Cons "Cons" [Var "x", Var "xs"]
          t = convertToList [numerical] ?ctx
          tenv = termEnv [("x", numerical), ("xs", term (List Numerical))]
      in seval' (match pat `seq` build pat) tenv t `shouldBe` successOrFail () (tenv, t)

    it "should throw away the current subject term if needed" $
      let ?ctx = Ctx.empty in
      let tenv = termEnv [("x", numerical)]
      in seval' (build (Var "x")) tenv lexical `shouldBe` success (tenv, numerical)

    it "should lookup variables" $
      let ?ctx = Ctx.empty in
      let tenv = termEnv [("x", numerical)]
      in seval' (build (Var "x")) tenv bottom `shouldBe` success (tenv, numerical)

    it "should merge two variables into one term" $
      let ?ctx = Ctx.empty in
      let tenv = termEnv [("x", numerical), ("y", term (List Lexical))]
          t = bottom
      in seval' (build (Cons "Cons" [Var "x", Var "y"])) tenv t `shouldBe` success (tenv, term (List Top))

    it "should properly construct a list of the same type" $
      let ?ctx = Ctx.empty in
      let tenv = termEnv [("x", numerical), ("y", term (List Numerical))]
          t = bottom
      in seval' (build (Cons "Cons" [Var "x", Var "y"])) tenv t `shouldBe` success (tenv, term (List Numerical))

    it "should merge a variable and the given subject term" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp") ,("Zero",[],"Exp") ,("Ifz",["Exp","Exp","Exp"],"Exp")] in
      let t = bottom
          tenv = termEnv [("x", term "Exp")]
      in seval' (build (Cons "Ifz" [Var "x", Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) tenv t `shouldBe`
        success (tenv, term "Exp")

    -- prop "should be sound" $ do
    --   [t1,t2,t3] <- C.similarTerms 3 7 2 10
    --   matchPattern <- C.similarTermPattern t1 3
    --   let vars = patternVars' matchPattern
    --   buildPattern <- arbitraryTermPattern 5 2 (if not (null vars) then elements vars else arbitrary)
    --   return $ counterexample
    --            (printf "match pattern: %s\nbuild pattern: %s\nt2: %s\nt3: %s\nlub t2 t3 = %s"
    --              (show matchPattern) (show buildPattern) (show t2) (show t3)
    --              (showLub t2 t3))
    --          $ sound'' (match matchPattern `seq` build buildPattern) [(t2,[]),(t3,[])] buildPattern

  describe "scope" $ do
    it "should hide declared variables" $
      let ?ctx = Ctx.empty in
      let tenv = termEnv [("x", numerical)]
      in do
         seval' (scope ["x"] (build "x")) tenv numerical `shouldBe` failure "unbound term variable x in build pattern x"
         seval' (scope ["x"] (match "x")) tenv numerical `shouldBe` success (tenv, numerical)

    it "should make non-declared variables available" $
      let ?ctx = Ctx.empty in
      let tenv = termEnv [ ("x",numerical) ]
      in do
         seval' (scope ["y"] (build "x")) tenv numerical `shouldBe`
           success (termEnv [("x", numerical) ], numerical)
         seval' (scope ["y"] (match "z")) tenv numerical `shouldBe`
           success (termEnv [ ("x",numerical), ("z",numerical)], numerical)

    it "should hide variables bound in a choice's test from the else branch" $
      let ?ctx = Ctx.fromList [("Zero",[],"Exp"), ("One",[],"Exp")] in
      let exp = term "Exp"
          tenv = emptyEnv
          prog = (build (T.Cons "Zero" []) `seq` match "x" `seq` fail)
                `leftChoice`
                (match "x")
      in seval' prog tenv exp `shouldBe` success (termEnv [("x", exp)], exp)

  describe "Let" $ do
    it "should apply a single function call" $
      let ?ctx = Ctx.empty in
      let t = term (Tuple ["Exp","Exp"])
          -- tenv = termEnv [("x",t)]
      in seval' (let_ [("swap", swap)] (scope ["x"] (match "x" `seq` call "swap" [] ["x"]))) emptyEnv t
           `shouldBe` success (emptyEnv, t)

    it "should support recursion" $
      let ?ctx = Ctx.empty in
      let t = convertToList [numerical, numerical, numerical] ?ctx
      in seval' (let_ [("map", map')] (scope ["x"] (match "x" `seq` call "map" [build (NumberLiteral 1)] ["x"]))) emptyEnv t
        `shouldBe` success (emptyEnv, term (List Numerical))

  describe "call" $ do
    it "should apply a single function call" $
      let ?ctx = Ctx.empty in
      let t = term (Tuple ["Exp","Exp"])
      in seval' (let_ [("swap",swap)](scope ["x"] (match "x" `seq` call "swap" [] []))) emptyEnv t `shouldBe` success (emptyEnv, t)

    it "should support an empty list in recursive applications" $
      let ?ctx = Ctx.empty in
      let t = convertToList [] ?ctx
      in seval' (let_ [("map", map')](scope ["x"] (match "x" `seq` call "map" [build (NumberLiteral 1)] ["x"]))) emptyEnv t `shouldBe`
           success (emptyEnv, term (List Numerical))

    it "should support a singleton list in recursive applications" $
      let ?ctx = Ctx.empty in
      let t = convertToList [numerical] ?ctx
      in seval' (let_ [("map", map')](scope ["x"] (match "x" `seq` call "map" [build (NumberLiteral 1)] ["x"]))) emptyEnv t `shouldBe`
           success (emptyEnv, term (List Numerical))

    it "should support recursion on a list of numbers" $
      let ?ctx = Ctx.empty in
      let t = convertToList [numerical, numerical, numerical] ?ctx
      in seval' (let_ [("map", map')] (scope ["x"] (match "x" `seq` call "map" [build (NumberLiteral 1)] ["x"]))) emptyEnv t `shouldBe`
           success (emptyEnv, term (List Numerical))

    it "should terminate and not produce infinite sorts" $
      let ?ctx = Ctx.empty in
      let t = term Top
      in do
        pendingWith "does not terminate"
        seval'' 0 0 (let_ [("map",map'), ("foo",strategy [] [] (scope ["x"] (match "x" `seq` call "map" [call "foo" [] []] ["x"])))] (call "foo" [] [])) (return M.empty) emptyEnv t `shouldBe`
           success (emptyEnv, term (List (List (List Top))))

  describe "simplify arithmetics" $ do
    it "reduce Add(Zero,y)" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")] in
      let exp = term "Exp"
          reduce = match (Cons "Add" [Cons "Zero" [], "y"]) `seq` build "y"
      in seval' reduce emptyEnv exp `shouldBe` successOrFail () (termEnv [("y", exp)], exp)

    it "reduce Add(x,Zero)" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")] in
      let exp = term "Exp"
          reduce = match (Cons "Add" ["x", Cons "Zero" []]) `seq` build "x"
      in seval' reduce emptyEnv exp `shouldBe` successOrFail () (termEnv [("x", exp)], exp)

    it "reduce Add(Zero,y) < id + Add(x,Zero)" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")] in
      let exp = term "Exp"
          reduce1 = match (Cons "Add" [Cons "Zero" [], "y"]) `seq` build "y"
          reduce2 = match (Cons "Add" ["x", Cons "Zero" []]) `seq` build "x"
      in seval' (reduce1 `leftChoice` reduce2) emptyEnv exp `shouldOverapproximate`
           successOrFail () (termEnv [("x",exp), ("y",exp)], exp)

    it "reduce Add(x,y); !x; ?Zero()" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")] in
      let exp = term "Exp"
          reduce = match (Cons "Add" ["x", "y"]) `seq` build "x" `seq` match (Cons "Zero" []) `seq` build "y"
      in seval' reduce emptyEnv exp `shouldBe` successOrFail () (termEnv [("x", exp),("y", exp)], exp)

    it "reduce Double(x)" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp"),("Double",["Exp"],"Exp")] in
      let exp = term "Exp"
          reduce = match (Cons "Double" ["x"]) `seq` build (Cons "Add" ["x", "x"])
      in seval' reduce emptyEnv exp `shouldBe` successOrFail () (termEnv [("x", exp)], exp)

    it "reduce Add(Zero,y) <+ Add(x,Zero) <+ Double(x)" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")] in
      let exp = term "Exp"
          reduce1 = match (Cons "Add" [Cons "Zero" [], "y"]) `seq` build "y"
          reduce2 = match (Cons "Add" ["x", Cons "Zero" []]) `seq` build "x"
          reduce3 = match (Cons "Double" ["x"]) `seq` build (Cons "Add" ["x", "x"])
      in seval' (reduce1 `leftChoice` reduce2 `leftChoice` reduce3) emptyEnv exp `shouldOverapproximate`
           successOrFail () (termEnv [("x",exp),("y",exp)], exp)

    -- prop "should be sound" $ do
    --   i <- choose (0,10)
    --   j <- choose (0,10)
    --   l <- C.similarTerms i 7 2 10
    --   let (l1,l2) = splitAt j l
    --   let t1 = C.convertToList l1
    --   let t2 = C.convertToList l2
    --   return $ counterexample (printf "t: %s\n" (showLub t1 t2))
    --          $ sound' (Let [("map", map')] (match "x" `seq` call "map" [build 1] ["x"])) [(t1,[]),(t2,[])]

  describe "Boolean Algebra" $
    caseStudy CaseStudy.balg $ do
      it "trans_bottomup: BExp -> NExp" $ \balg -> do
          seval'' 0 10 (call "trans__bottomup_0_0" [] []) balg emptyEnv (term "BExp") `shouldBe`
            successOrFail () (emptyEnv, term "NExp")

      it "trans_topdown: BExp -> NExp" $ \balg -> do
          seval'' 1 10 (call "trans__topdown_0_0" [] []) balg emptyEnv (term "BExp") `shouldBe`
            successOrFail () (emptyEnv, term "NExp")

  describe "PCF" $
    caseStudy CaseStudy.pcf $ do
      it "lookup: String * Env -> Val" $ \pcf ->
        let prog = term (Tuple [Lexical, List (Tuple [Lexical, "Val"])])
            val  = term "Val"
        in do
          seval'' 0 1 (call "lookup_0_0" [] []) pcf emptyEnv prog `shouldBe`
            successOrFail () (emptyEnv, val)

      it "eval: Env * Exp -> Val" $ \pcf ->
        let prog = term (Tuple [List (Tuple [Lexical, "Val"]), "Exp"])
            val  = term "Val"
        in do
          seval'' 2 10 (call "eval_0_0" [] []) pcf emptyEnv prog `shouldBe`
             successOrFail () (emptyEnv, val)

  describe "Negative Normal Form" $
    caseStudy CaseStudy.nnf $
      it "nnf: Formula -> Formula" $ \nnf ->
        seval'' 0 10 (call "main_0_0" [] []) nnf emptyEnv (term "Formula") `shouldBe`
            success (emptyEnv, term "Formula")

  describe "Causal Commutative Arrows Normalization" $
    caseStudy CaseStudy.cca $
      it "norm: Exp -> Exp" $ \cca ->
        seval'' 0 10 (call "norm_0_0" [] []) cca emptyEnv (term "Exp") `shouldBe`
            successOrFail () (emptyEnv, term "Exp")

  describe "Arrow Desugaring" $
    caseStudy CaseStudy.arrows $ do
      it "tuple-pat: List Var -> APat" $ \desugar ->
        let prog = term $ List "Var"
            val  = term "APat"
        in do
          seval'' 0 10 (call "tuple_pat_0_0" [] []) desugar emptyEnv prog `shouldBe`
            successOrFail () (emptyEnv, val)

      it "tuple: List Var -> Exp" $ \desugar ->
        let prog = term $ List "Var"
            val  = term "Exp"
        in do
          seval'' 0 10 (call "tuple_0_0" [] []) desugar emptyEnv prog `shouldBe`
            successOrFail () (emptyEnv, val)

      it "tuple-exp: List Exp -> Exp" $ \desugar ->
        let prog = term $ List "Exp"
            val  = term "Exp"
        in do
          seval'' 0 10 (call "tuple_exp_0_0" [] []) desugar emptyEnv prog `shouldBe`
            successOrFail () (emptyEnv, val)

      it "at-end: (=> List Var) -> List Var -> List Var " $ \desugar ->
        let prog = term $ List "Var"
            val  = term $ List "Var"
            s = build $ Cons "Cons" [Cons "VarVar" [StringLiteral "v"], Cons "Nil" []]
        in do
          seval'' 0 10 (call "at_end_1_0" [s] []) desugar emptyEnv prog `shouldBe`
            successOrFail () (emptyEnv, val)

      it "at-end (scoped strat arg): (=> List Var) -> List Var -> List Var " $ \desugar ->
        let prog = term $ List "Var"
            val  = term $ List "Var"
            env = termEnv [("vars-list", term $ List "Var")]
            s = build $ Var "vars-list"
        in do
          seval'' 0 10 (call "at_end_1_0" [s] []) desugar env prog `shouldBe`
            successOrFail () (env, val)

      it "conc: (List Var, List Var) -> List Var " $ \desugar ->
        let prog = term $ Tuple [List "Var", List "Var"]
            val  = term $ List "Var"
        in do
          seval'' 0 10 (call "conc_0_0" [] []) desugar emptyEnv prog `shouldBe`
            successOrFail () (emptyEnv, val)

      it "free-pat-vars: APat -> List Var " $ \desugar ->
        let prog = term "APat"
            val  = term $ List "Var"
        in do
          seval'' 1 10 (call "free_pat_vars_0_0" [] []) desugar emptyEnv prog `shouldBe`
            successOrFail () (emptyEnv, val)

      it "free-decls-vars: Declbinds -> List Var " $ \desugar ->
        let prog = term "Declbinds"
            val  = term $ List "Var"
        in do
          seval'' 1 10 (call "free_decls_vars_0_0" [] []) desugar emptyEnv prog `shouldBe`
            successOrFail () (emptyEnv, val)

      it "tuple-pat: List(Var) -> APat" $ \desugar -> do
        seval'' 1 10 (call "tuple_pat_0_0" [] []) desugar emptyEnv (term (List "Var"))`shouldBe`
          successOrFail () (emptyEnv, term "APat")

      it "tuple: List(Var) -> Exp" $ \desugar -> do
        seval'' 1 10 (call "tuple_0_0" [] []) desugar emptyEnv (term (List "Var"))`shouldBe`
          successOrFail () (emptyEnv, term "Exp")
        seval'' 1 10 (call "tuple_0_0" [] []) desugar emptyEnv (IllSorted [("Cons",[term "Var", term (List "Var")]), ("Nil",[])])`shouldBe`
          successOrFail () (emptyEnv, term "Exp")

      it "apply-all(|k:Exp): Exp * List(Exp) -> Expr" $ \desugar -> do
        let tenv = termEnv [("v_14", term "Exp")]
        seval'' 1 10 (call "apply_all_0_1" [] ["v_14"]) desugar tenv (term (Tuple ["Exp", List "Exp"]))`shouldBe`
          successOrFail () (tenv, term "Exp")

      it "well-sorted" $ \desugar -> do
        let t = IllSorted [("OpApp",[term "Exp",term "Qop",term "Exp"])
                          ,("Product",[term "Exps2"])
                          ,("Let",[term "Declbinds",term "Exp"])
                          ,("Negation",[term "Exp"])
                          ,("Labeled",[term "Exp",term "LabelBinds"])
                          ,("AppBin",[term "Exp",term "Exp"])
                          ,("Typed",[term "Exp", term (Option "Context"), term "Type"])
                          ,("Case",[term "AnyExp", term "AltList"])
                          ,("ArrProcedure",[term "APat",term "ArrCommand"])
                          ,("If",[term "AnyExp", term "AnyExp", term "Exp"])
                          ,("LSection",[term "Exp",term "Qop"])
                          ,("ListFromTo",[term "Exp",term "Exp"])
                          ,("List",[term (List "Exp")])
                          ,("ListCompr",[term "Exp", term (List "Qual")])
                          ,("ListFrom",[term "Exp"])
                          ,("Abs",[term "Fargs",term "Exp"])
                          ,("ListFirstFromTo",[term "Exp",term "Exp",term "Exp"])
                          ,("RSection",[term "QopNoNeg",term "Exp"])
                          ,("Constr",[term "Gcon"])
                          ,("Named",[term "Qvar",term "Exp"])
                          ,("Lit",[term "Literal"])
                          ,("ListFirstFrom",[term "Exp",term "Exp"])
                          ,("Var",[term "Qvar"])
                          ,("Do",[term "StmtList"])
                          ]
          in termWidening ?ctx 0 10 t bottom `shouldBe` (Stable,term "Exp")

        let t = IllSorted [("VarVar",[term "Varid"])
                          ,("BinOp",[term "Varsym"])
                          ,("TuplePat",[term "Var", term (List "Var")])
                          ,("ConstrPat",[term "Gcon"])]
          in termWidening ?ctx 0 10 t bottom `shouldBe` (Unstable,term "APat")

        let t = IllSorted [("BinCon",[term "Qop"])]
          in termWidening ?ctx 0 10 t bottom `shouldBe` (Unstable,term "Exp")

      it "builds" $ \desugar -> do
        let pat = build (Cons "OpApp"
                        [Cons "AppBin"
                          [ Cons "Var" [ StringLiteral "arr" ]
                          , Cons "Abs"
                            [ Cons "Cons"
                              [ "g_17",
                                Cons "Nil" []
                              ],
                              "e_17"
                            ]
                          ],
                          StringLiteral ">>>",
                          "f_17"])

            tenv = termEnv [("g_17", term "APat")
                           ,("e_17", term "Exp")
                           ,("f_17", term "Exp")
                           ]
          in seval'' 1 10 pat desugar tenv bottom `shouldBe`
              success (tenv, term "Exp")

        let pat = build (Cons "OpApp"
                        [Cons "AppBin"
                          [ Cons "Var" [ StringLiteral "arr" ]
                          , Cons "Abs"
                            [ Cons "Cons"
                              [ "b_17",
                                Cons "Nil" []
                              ],
                              Cons "Product"
                              [ Cons "ECons"
                                [ "z_16"
                                , Cons "Cons"
                                  [ "a_17"
                                  , Cons "Nil" []
                                  ]
                                ]
                              ]
                            ]
                          ],
                          StringLiteral ">>>",
                          Cons "Var" [StringLiteral "app"]])

            tenv = termEnv [("b_17", term "APat")
                           ,("z_16", term "Exp")
                           ,("a_17", term "Exp")
                           ]
          in seval'' 1 10 pat desugar tenv bottom `shouldBe`
              success (tenv, term "Exp")

        let pat = build (Cons "OpApp"
                        [Cons "AppBin"
                          [ Cons "Var" [ StringLiteral "arr" ]
                          , Cons "Abs"
                            [ Cons "Cons"
                              [ "o_16",
                                Cons "Nil" []
                              ],
                              Cons "If"
                              [ "l_16"
                              , Cons "AppBin"
                                [ Cons "Constr" [StringLiteral "Left"]
                                , "p_16"
                                ]
                              , Cons "AppBin"
                                [ Cons "Constr" [StringLiteral "Right"]
                                , "q_16"
                                ]
                              ]
                            ]
                          ],
                          StringLiteral ">>>",
                          Cons "OpApp"
                          [ "r_16"
                          , StringLiteral "|||"
                          , "s_16"
                          ]])

            tenv = termEnv [("o_16", term "APat")
                           ,("l_16", term "Exp")
                           ,("p_16", term "Exp")
                           ,("q_16", term "Exp")
                           ,("r_16", term "Exp")
                           ,("s_16", term "Exp")
                           ]
          in seval'' 1 10 pat desugar tenv bottom `shouldBe`
              success (tenv, term "Exp")


        let pat = build (Cons "OpApp"
                        [Cons "AppBin"
                          [ Cons "Var" [ StringLiteral "arr" ]
                          , Cons "Abs"
                            [ Cons "Cons"
                              [ "e_16",
                                Cons "Nil" []
                              ],
                              Cons "Let"
                              [ "a_16"
                              , "f_16"
                              ]
                            ]
                          ],
                          StringLiteral ">>>",
                          "g_16"
                        ])

            tenv = termEnv [("e_16", term "APat")
                           ,("a_16", term "Declbinds")
                           ,("f_16", term "Exp")
                           ,("g_16", term "Exp")
                           ]
          in seval'' 1 10 pat desugar tenv bottom `shouldBe`
              success (tenv, term "Exp")


        let pat = build (Cons "OpApp"
                        [Cons "AppBin"
                          [ Cons "Var" [ StringLiteral "arr" ]
                          , Cons "Abs"
                            [ Cons "Cons"
                              [ Cons "TuplePat"
                                [ "s_15"
                                , Cons "Cons"
                                  [ "o_15"
                                  , Cons "Nil" []
                                  ]
                                ]
                              , Cons "Nil" []
                              ],
                              "t_15"
                            ]
                          ],
                          StringLiteral ">>>",
                          "u_15"
                        ])

            tenv = termEnv [("s_15", term "APat")
                           ,("o_15", term "APat")
                           ,("t_15", term "Exp")
                           ,("u_15", term "Exp")
                           ]
          in seval'' 1 10 pat desugar tenv bottom `shouldBe`
              success (tenv, term "Exp")


        let pat = build (Cons "OpApp"
                        [Cons "AppBin"
                          [ Cons "Var" [ StringLiteral "arr" ]
                          , Cons "Abs"
                            [ Cons "Cons"
                              [ "g_15"
                              , Cons "Nil" []
                              ]
                            , Cons "Product"
                              [ Cons "ECons"
                                [ "h_15"
                                , Cons "Cons" [ "e_15", Cons "Nil" []]
                                ]
                              ]
                            ]
                          ],
                          StringLiteral ">>>",
                          "i_15"
                        ])

            tenv = termEnv [("g_15", term "APat")
                           ,("h_15", term "Exp")
                           ,("e_15", term "Exp")
                           ,("i_15", term "Exp")
                           ]
          in seval'' 1 10 pat desugar tenv bottom `shouldBe`
              success (tenv, term "Exp")

        let pat = build (Cons "ArrForm"
                        [ Cons "Constr" [Cons "BinCon" ["p_14"]]
                        , Cons "Cons"
                          [ "q_14"
                          , Cons "Cons"
                            [ "r_14"
                            , Cons "Nil" []
                            ]
                          ]
                        ])
            tenv = termEnv [("p_14", term "Qop")
                           ,("q_14", term "ArrCommand")
                           ,("r_14", term "ArrCommand")
                           ]
          in seval'' 1 10 pat desugar tenv bottom `shouldBe`
              success (tenv, term "ArrCommand")

        let pat = build (Cons "BinCon" ["p_14"])
            tenv = termEnv [("p_14", term "Qop")]
          in seval'' 1 10 pat desugar tenv bottom `shouldBe`
              success (tenv, term "Qcon")

        let pat = build (Cons "OpApp"
                         [Cons "AppBin"
                           [ Cons "Var" [ StringLiteral "arr" ]
                           , Cons "Abs"
                             [ Cons "Cons"
                               [ "g_14"
                               , Cons "Nil" []
                               ]
                             , Cons "Let"
                               [ "c_14"
                               , "h_14"
                               ]
                             ]
                           ],
                           StringLiteral ">>>",
                           "i_14"
                         ])
            tenv = termEnv [("g_14", term "APat")
                           ,("c_14", term "Declbinds")
                           ,("h_14", term "Exp")
                           ,("i_14", term "Exp")
                           ]
          in seval'' 1 10 pat desugar tenv bottom `shouldBe`
              success (tenv, term "Exp")


      it "desugar-arrow': ArrCommand -> Exp" $ \desugar ->
        let prog = term "ArrCommand"
            val  = term "Exp"
            env = termEnv [("vars-list", term $ List "Var")]
        in do
          seval'' 1 10 (call "desugar_arrow_p__0_1" [] [TermVar "vars-list"]) desugar env prog `shouldBe`
            successOrFail () (env, val)

  where
    termEnv :: [(TermVar, Term)] -> TermEnv Term
    termEnv = S.fromList

    delete :: TermVars s => s -> TermEnv Term -> TermEnv Term
    delete s = S.delete' (termVars s :: Set TermVar)

    emptyEnv :: TermEnv Term
    emptyEnv = S.empty

    -- showLub :: C.Term -> C.Term -> String
    -- showLub t1 t2 = show (alpha (C.fromFoldable [t1,t2] :: C.Pow C.Term) :: Term)

    swap = strategy [] [] (scope ["x","y"] (match (Cons "" ["x","y"]) `seq` build (Cons "" ["y","x"])))

    map' = strategy ["f"] ["l"] (scope ["x","xs","x'","xs'"] (
            build "l" `seq`
            guardedChoice
              (match (Cons "Cons" ["x","xs"]))
              (build "x" `seq`
               call "f" [] [] `seq`
               match "x'" `seq`
               call "map" [call "f" [] []] ["xs"] `seq`
               match "xs'" `seq`
               build (Cons "Cons" ["x'", "xs'"]))
              (build (Cons "Nil" []))))

    seval :: (?ctx :: Context) => LStrat -> Term -> Error TypeError (Except () (TermEnv Term,Term))
    seval s = seval'' 0 10 s (return M.empty) emptyEnv

    seval' :: (?ctx :: Context) => LStrat -> TermEnv Term -> Term -> Error TypeError (Except () (TermEnv Term,Term))
    seval' s = seval'' 0 10 s (return M.empty)

    seval'' :: (?ctx :: Context) => Int -> Int -> LStrat -> LStratEnv -> TermEnv Term -> Term -> Error TypeError (Except () (TermEnv Term,Term))
    seval'' j k s senv tenv t =
      fmap (fmap (fmap (typecheck' ?ctx) *** typecheck' ?ctx)) $
      fromCompletion (error "top element")
        (fromTerminating (error "sort semantics does not terminate")
           (eval j k s senv ?ctx tenv t))

    term :: (?ctx :: Context) => Sort -> Term
    term s = Sorted s ?ctx

    bottom :: (?ctx :: Context) => Term
    bottom = term Bottom

    lexical :: (?ctx :: Context) => Term
    lexical = term Lexical

    numerical :: (?ctx :: Context) => Term
    numerical = term Numerical

    success :: a -> Error e (Except () a)
    success a = F.Success $ E.Success a

    successOrFail :: () -> a -> Error e (Except () a)
    successOrFail () a = F.Success $ E.SuccessOrFail () a

    uncaught :: () -> Error e (Except () a)
    uncaught = F.Success . E.Fail

    failure :: String -> Error TypeError (Except () a)
    failure = F.Fail . fromString

    caseStudy :: IO (Context,LStratEnv) -> ((?ctx :: Context) => SpecWith LStratEnv) -> Spec
    caseStudy loadCaseStudy spc = do
      (cntx,senv) <- runIO loadCaseStudy
      let ?ctx = cntx in beforeAll (return senv) spc

    shouldOverapproximate :: (Show a, PreOrd a) => a -> a -> Expectation
    shouldOverapproximate a b
      | b ⊑ a     = return ()
      | otherwise = expectationFailure (printf "%s does not overapproximate %s" (show a) (show b))
