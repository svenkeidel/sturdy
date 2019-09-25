{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
module SortSemanticsSpec(main, spec) where

import Prelude hiding (exp,seq,fail)

import           Syntax hiding (fail)
import qualified Syntax as T
import           SortSemantics -- hiding (sortContext)
import           AbstractInterpreter
import           Abstract.TermEnvironment
import           SortContext(Context,Sort(..))
import qualified SortContext as Ctx

import           Data.Abstract.FreeCompletion (fromCompletion)
import           Data.Abstract.Except as E
import           Data.Abstract.Error as F
import qualified Data.Abstract.Maybe as A
import           Data.Abstract.There
import qualified Data.Abstract.Map as S
import           Data.Abstract.Terminating (fromTerminating)
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as H

import           Test.Hspec hiding (context)

import           GHC.Exts(IsString(..))

import qualified CaseStudy

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

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

  describe "match" $ do
    it "should match an identical builtin string literal" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 0 in
      seval (match (StringLiteral "x")) lexical `shouldBe` successOrfail () (emptyEnv, lexical)

    it "should match another builtin string literal" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 0 in
      seval (match (StringLiteral "y")) lexical `shouldBe` successOrfail () (emptyEnv, lexical)

    it "should match an equal builtin number literal" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 0 in
      seval (match (NumberLiteral 42)) numerical `shouldBe` successOrfail () (emptyEnv, numerical)

    it "should match another builtin number literal" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 0 in
      seval (match (NumberLiteral 1)) numerical `shouldBe`  successOrfail () (emptyEnv, numerical)

    it "a string literal should not match a number literal" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 0 in
      seval (match (NumberLiteral 1)) lexical `shouldBe` uncaught ()

    it "a number literal should not match a string literal" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 0 in
      seval (match (StringLiteral "x")) numerical `shouldBe` uncaught ()

    it "should match a PCF expression" $
      let ?ctx = Ctx.fromList [("Zero",[],"Exp")] in
      let ?sensitivity = 0 in
      let t = term "Exp"
      in seval (match (Cons "Zero" [])) t `shouldBe` success (emptyEnv, t)

    it "should match a nested PCF expression" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")] in
      let ?sensitivity = 0 in
      let t = term "Exp"
      in seval (match (Cons "Succ" [Cons "Zero" []])) t `shouldBe` successOrfail () (emptyEnv, t)

    it "should match a constructor with more than one argument" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp") ,("Zero",[],"Exp") ,("Ifz",["Exp","Exp","Exp"],"Exp")] in
      let ?sensitivity = 0 in
      let t = term "Exp"
      in seval (match (Cons "Ifz" [Cons "Zero" [], Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) t `shouldBe`
        successOrfail () (emptyEnv, t)

    it "should introduce one variable" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")] in
      let ?sensitivity = 0 in
      let t = term "Exp"
      in seval (match (Cons "Succ" ["x"])) t `shouldBe` successOrfail () (termEnv [("x", t)], t)

    it "should introduce one variable 2" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")] in
      let ?sensitivity = 0 in
      let t = term "Exp"
      in seval (match (Cons "Succ" ["x"])) t `shouldBe` successOrfail () (termEnv [("x", t)], t)

    it "should introduce multiple variables and support linear pattern matching" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")] in
      let ?sensitivity = 0 in
      let t = term "Exp"
      in seval (match (Cons "Succ" ["x"]) `seq` match (Cons "Succ" ["y"])) t `shouldBe`
         successOrfail () (termEnv [("x", t), ("y", t)], t)

    it "should support linear pattern matching" $
      let ?ctx = Ctx.fromList [] in
      let ?sensitivity = 0 in
      let t = term (Tuple [Lexical, Lexical])
      in seval (match (Cons "" ["x", "x"])) t `shouldBe` successOrfail () (termEnv [("x",lexical)],t)

    it "should succeed when exploding literals" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 0 in
      let tenv = termEnv [("x", term (List Bottom))]
      in seval' (match (Explode "_" "x")) (S.delete "x" emptyEnv) numerical `shouldBe` success (tenv, numerical)

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

  describe "build" $ do
    it "should build a builtin string literal" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 0 in
      seval (build (StringLiteral "foo")) bottom `shouldBe` success (emptyEnv, lexical)

    it "should build a builtin number literal" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 0 in
      seval (build (NumberLiteral 1)) bottom `shouldBe` success (emptyEnv, numerical)

    it "a string grammar should not be build on a number literal" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 0 in
      seval (build (NumberLiteral 1)) bottom `shouldNotBe` success (emptyEnv, lexical)

    it "a number grammar should not be build on a string literal" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 0 in
      seval (match (StringLiteral "x")) bottom `shouldNotBe` success (emptyEnv, numerical)

    it "should build a simple constant PCF expression" $
      let ?ctx = Ctx.fromList [("Zero",[],"Exp")] in
      let ?sensitivity = 0 in
      let t = bottom
          t' = term "Exp"
      in seval (build (Cons "Zero" [])) t `shouldBe` success (emptyEnv, t')

    it "should build a nested PCF expression" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")] in
      let ?sensitivity = 0 in
      let t = bottom
          t' = term "Exp"
      in seval (build (Cons "Succ" [Cons "Zero" []])) t `shouldBe` success (emptyEnv, t')

    it "should build a constructor with more than one argument" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp") ,("Zero",[],"Exp") ,("Ifz",["Exp","Exp","Exp"],"Exp")] in
      let ?sensitivity = 0 in
      let t = term "Exp"
      in seval (build (Cons "Ifz" [Cons "Zero" [], Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) t `shouldBe`
        success (emptyEnv, t)

    it "build should be inverse to match" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 0 in
      let pat = NumberLiteral 1
      in seval (match pat `seq` build pat) numerical `shouldBe` successOrfail () (emptyEnv, numerical)

    it "build should be inverse to match with a more complicated term" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 0 in
      let pat = Cons "Cons" [Var "x", Var "xs"]
          t = convertToList [numerical] ?ctx
          tenv = termEnv [("x", numerical), ("xs", term (List Numerical))]
      in seval' (match pat `seq` build pat) tenv t `shouldBe` successOrfail () (tenv, t)

    it "should throw away the current subject term if needed" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 0 in
      let tenv = termEnv [("x", numerical)]
      in seval' (build (Var "x")) tenv lexical `shouldBe` success (tenv, numerical)

    it "should lookup variables" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 0 in
      let tenv = termEnv [("x", numerical)]
      in seval' (build (Var "x")) tenv bottom `shouldBe` success (tenv, numerical)

    it "should merge two variables into one term" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 0 in
      let tenv = termEnv [("x", numerical), ("y", term (List Lexical))]
          t = bottom
      in seval' (build (Cons "Cons" [Var "x", Var "y"])) tenv t `shouldBe` success (tenv, term (List Top))

    it "should properly construct a list of the same type" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 0 in
      let tenv = termEnv [("x", numerical), ("y", term (List Numerical))]
          t = bottom
      in seval' (build (Cons "Cons" [Var "x", Var "y"])) tenv t `shouldBe` success (tenv, term (List Numerical))

    it "should merge a variable and the given subject term" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp") ,("Zero",[],"Exp") ,("Ifz",["Exp","Exp","Exp"],"Exp")] in
      let ?sensitivity = 0 in
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
      let ?sensitivity = 0 in
      let tenv = termEnv [("x", numerical)]
      in do
         seval' (scope ["x"] (build "x")) tenv numerical `shouldBe` failure "unbound term variable x in build pattern !x"
         seval' (scope ["x"] (match "x")) tenv numerical `shouldBe` success (tenv, numerical)

    it "should make non-declared variables available" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 0 in
      let tenv = termEnv'' [ ("x",numerical) ] ["y"] in
      do
         seval' (scope ["y"] (build "x")) tenv numerical `shouldBe`
           success (tenv, numerical)
         seval' (scope ["y"] (match "z")) (S.delete' @[] ["z"] tenv) numerical `shouldBe`
           success (S.delete' @[] ["y"] $ termEnv [ ("x",numerical), ("z",numerical)], numerical)

    it "should hide variables bound in a choice's test from the else branch" $
      let ?ctx = Ctx.fromList [("Zero",[],"Exp"), ("One",[],"Exp")] in
      let ?sensitivity = 0 in
      let exp = term "Exp"
          tenv = S.delete "x" emptyEnv
          prog = (build (T.Cons "Zero" []) `seq` match "x" `seq` T.fail)
                `leftChoice`
                (match "x")
      in seval' prog tenv exp `shouldBe` success (termEnv [("x", exp)], exp)

  describe "Let" $ do
    it "should apply a single function call" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 2 in
      let t = term (Tuple ["Exp","Exp"])
          -- tenv = termEnv [("x",t)]
          tenv = termEnv'' [] ["x","y"]
      in seval' (let_ [("swap", swap)] (scope ["x"] (match "x" `seq` call "swap" [] ["x"]))) tenv t
           `shouldBe` success (tenv, t)

    it "should support recursion" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 2 in
      let t = convertToList [numerical, numerical, numerical] ?ctx
          tenv = termEnv' []
      in seval (let_ [("map", map')] (scope ["x"] (match "x" `seq` call "map" [scope [] $ build (NumberLiteral 1)] ["x"]))) t
        `shouldBe` success (tenv, term (List Numerical))

  describe "call" $ do
    it "should apply a single function call" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 1 in
      let senv = stratEnv [("swap",swap)]
          t = term (Tuple ["Exp","Exp"])
          tenv = termEnv []
      in seval'' 10 (scope ["x"] (match "x" `seq` call "swap" [] [])) senv emptyEnv t `shouldBe` success (tenv, t)

    it "should support an empty list in recursive applications" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 2 in
      let senv = stratEnv [("map",map')]
          t = convertToList [] ?ctx
          tenv = termEnv []
      in seval'' 10 (scope ["x"] (match "x" `seq` call "map" [scope [] $ build (NumberLiteral 1)] ["x"])) senv emptyEnv t `shouldBe`
           success (tenv, term (List Numerical))

    it "should support a singleton list in recursive applications" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 2 in
      let senv = stratEnv [("map",map')]
          t = convertToList [numerical] ?ctx
          tenv = termEnv []
      in seval'' 10 (scope ["x"] (match "x" `seq` call "map" [scope [] $ build (NumberLiteral 1)] ["x"])) senv emptyEnv t `shouldBe`
           success (tenv, term (List Numerical))

    it "should support recursion on a list of numbers" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 2 in
      let senv = stratEnv [("map",map')]
          c = Ctx.empty
          t = convertToList [numerical, numerical, numerical] c
          tenv = termEnv []
      in seval'' 10 (scope ["x"] (match "x" `seq` call "map" [scope [] $ build (NumberLiteral 1)] ["x"])) senv emptyEnv t `shouldBe`
           success (tenv, term (List Numerical))

    it "should terminate and not produce infinite sorts" $
      let ?ctx = Ctx.empty in
      let ?sensitivity = 10 in
      let senv = -- stratEnv [("map",T.liftStrategyScopes map'),
                 --          ("foo",T.liftStrategyScopes $ Strategy [] [] (scope ["x"] (match "x" `seq` call "map" ["foo"] ["x"])))]
            stratEnv [("map",map'),
                      ("foo",strategy [] [] (scope ["x"] (match "x" `seq` call "map" [call "foo" [] []] ["x"])))]
          c = Ctx.empty
          t = Term Top c
          tenv = termEnv []
      in seval'' 3 (call "foo" [] []) senv emptyEnv t `shouldBe`
           success (tenv, Term (List (List (List Top))) c)

  describe "simplify arithmetics" $ do
    it "reduce Add(Zero,y)" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")] in
      let ?sensitivity = 0 in
      let exp = term "Exp"
          reduce = match (Cons "Add" [Cons "Zero" [], "y"]) `seq` build "y"
      in seval' (reduce) (S.delete "y" emptyEnv) exp
           `shouldBe` successOrfail () (termEnv [("y", exp)], exp)

    it "reduce Add(x,Zero)" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")] in
      let ?sensitivity = 0 in
      let exp = term "Exp"
          reduce = match (Cons "Add" ["x", Cons "Zero" []]) `seq` build "x"
      in seval' reduce (S.delete "x" emptyEnv) exp
           `shouldBe` successOrfail () (termEnv [("x", exp)], exp)

    it "reduce Add(Zero,y) < id + Add(x,Zero)" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")] in
      let ?sensitivity = 0 in
      let exp = term "Exp"
          reduce1 = match (Cons "Add" [Cons "Zero" [], "y"]) `seq` build "y"
          reduce2 = match (Cons "Add" ["x", Cons "Zero" []]) `seq` build "x"
      in seval' (reduce1 `leftChoice` reduce2) (S.delete' @[] ["x", "y"] emptyEnv) exp
           `shouldBe` successOrfail () (termEnv' [may "x" exp, may "y" exp], exp)

    it "reduce Add(x,y); !x; ?Zero()" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")] in
      let ?sensitivity = 0 in
      let exp = term "Exp"
          reduce = match (Cons "Add" ["x", "y"]) `seq` build "x" `seq` match (Cons "Zero" []) `seq` build "y"
      in seval' reduce (S.delete' @[] ["x", "y"] emptyEnv) exp `shouldBe` successOrfail () (termEnv [("x", exp),("y", exp)], exp)

    it "reduce Double(x)" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp"),("Double",["Exp"],"Exp")] in
      let ?sensitivity = 0 in
      let exp = term "Exp"
          reduce = match (Cons "Double" ["x"]) `seq` build (Cons "Add" ["x", "x"])
      in seval' reduce (S.delete' @[] ["x"] emptyEnv) exp `shouldBe` successOrfail () (termEnv [("x", exp)], exp)

    it "reduce Add(Zero,y) <+ Add(x,Zero) <+ Double(x)" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")] in
      let ?sensitivity = 0 in
      let exp = term "Exp"
          reduce1 = match (Cons "Add" [Cons "Zero" [], "y"]) `seq` build "y"
          reduce2 = match (Cons "Add" ["x", Cons "Zero" []]) `seq` build "x"
          reduce3 = match (Cons "Double" ["x"]) `seq` build (Cons "Add" ["x", "x"])
      in seval' (reduce1 `leftChoice` reduce2 `leftChoice` reduce3) (S.delete' @[] ["x","y"] emptyEnv) exp
           `shouldBe` successOrfail () (termEnv' [may "x" exp, may "y" exp], exp)

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
    caseStudy CaseStudy.balg $
      it "trans_bottomup: BExp -> Exp" $ \balg ->
        let ?sensitivity = 2 in do
          pendingWith "The sort semantics is too imprecise to check this traversal"
          seval'' 10 (call "trans__bottomup_0_0" [] []) balg emptyEnv (term "BExp") `shouldBe`
            successOrfail () (emptyEnv, term "Exp")

  describe "PCF interpreter in Stratego" $
    caseStudy CaseStudy.pcf $ do
      it "lookup: String * Env -> Val" $ \pcf ->
        let ?sensitivity = 2 in
        let prog = term (Tuple [Lexical, List (Tuple [Lexical, "Val"])])
            val  = term "Val"
        in seval'' 10 (call "lookup_0_0" [] []) pcf emptyEnv prog `shouldBe`
            successOrfail () (emptyEnv, val)

      it "eval: Env * Exp -> Val" $ \pcf ->
        let ?sensitivity = 10 in
        let prog = term (Tuple [List (Tuple [Lexical, "Val"]), "Exp"])
            val  = term "Val"
        in seval'' 10 (call "eval_0_0" [] []) pcf emptyEnv prog `shouldBe`
             successOrfail () (emptyEnv, val)

  describe "Arrow" $
    caseStudy CaseStudy.arrows $ do
      it "tuple-pat: List Var -> APat" $ \desugar ->
        let ?sensitivity = 2 in
        let prog = term $ List "Var"
            val  = term "APat"
            env = termEnv []
        in sevalNoNeg'' 10 (call "tuple_pat_0_0" [] []) desugar env prog `shouldBe`
            successOrfail () (env, val)

      it "tuple-exp: List Exp -> Exp" $ \desugar ->
        let ?sensitivity = 0 in
        let prog = term $ List "Exp"
            val  = term "Exp"
            env = termEnv [] -- delete desugar $ termEnv []
        in seval'' 10 (call "tuple_exp_0_0" [] []) desugar env prog `shouldBe`
            successOrfail () (env, val)

      it "tuple: List Var -> Exp" $ \desugar ->
        let ?sensitivity = 2 in
        let prog = term $ List "Var"
            val  = term "Exp"
            env = termEnv []
        in sevalNoNeg'' 10 (call "tuple_0_0" [] []) desugar env prog `shouldBe`
            successOrfail () (env, val)

      it "at-end: (=> List Var) -> List Var -> List Var " $ \desugar ->
        let ?sensitivity = 2 in
        let prog = term $ List "Var"
            val  = term $ List "Var"
            env = termEnv []
            s = build $ Cons "Cons" [Cons "VarVar" [StringLiteral "v"], Cons "Nil" []]
        in sevalNoNeg'' 10 (call "at_end_1_0" [s] []) desugar env prog `shouldBe`
            successOrfail () (env, val)

      it "at-end (scoped strat arg): (=> List Var) -> List Var -> List Var " $ \desugar ->
        let ?sensitivity = 2 in
        let prog = term $ List "Var"
            val  = term $ List "Var"
            env = termEnv [("vars-list", term $ List "Var")]
            s = build $ Var "vars-list"
        in sevalNoNeg'' 10 (call "at_end_1_0" [s] []) desugar env prog `shouldBe`
            successOrfail () (env, val)

      it "conc: (List Var, List Var) -> List Var " $ \desugar ->
        let ?sensitivity = 3 in
        let prog = term $ Tuple [List "Var", List "Var"]
            val  = term $ List "Var"
            env = termEnv []
        in sevalNoNeg'' 10 (call "conc_0_0" [] []) desugar env prog `shouldBe`
            successOrfail () (env, val)

      it "free-pat-vars: APat -> List Var " $ \desugar ->
        let ?sensitivity = 2 in
        let prog = term "APat"
            val  = term $ List "Var"
            env = termEnv []
        in sevalNoNeg'' 10 (call "free_pat_vars_0_0" [] []) desugar env prog `shouldBe`
            successOrfail () (env, val)

      it "free-decls-vars: Declbinds -> List Var " $ \desugar ->
        let ?sensitivity = 2 in
        let prog = term "Declbinds"
            val  = term $ List "Var"
            env = termEnv []
            senv = filterStratEnv ["free_decls_vars_0_0", "collect_all_3_0",
                                   "union_0_0", "crush_3_0", "foldr_3_0"] <$> desugar
        in sevalNoNeg'' 10 (call "free_decls_vars_0_0" [] []) senv env prog `shouldBe`
            successOrfail () (env, val)

      it "desugar-arrow': ArrCommand -> Exp" $ \desugar ->
        let ?sensitivity = 4 in
        let prog = term "ArrCommand"
            val  = term "Exp"
            env = termEnv [("vars-list", term $ List "Var")]
        in sevalNoNeg'' 10 (call "desugar_arrow_p__0_1" [] [TermVar "vars-list"]) desugar env prog `shouldBe`
            successOrfail () (env, val)

  where
    -- sound' :: Strat -> [(C.Term,[(TermVar,C.Term)])] -> Property
    -- sound' s xs = sound (M.empty,ctx) (C.fromFoldable $ fmap (second termEnv') xs) (eval' s) (eval' s :: Interp (SW.Categories (Strat,StratEnv) (TermEnv,Term) SW.Stack) Term Term) where
    --   ctx = (alpha::C.Pow C.Term -> Context) (C.fromFoldable (map fst xs))

    -- sound'' :: Strat -> [(C.Term,[(TermVar,C.Term)])] -> TermPattern -> Property
    -- sound'' s xs pat = sound (M.empty,ctx) (C.fromFoldable $ fmap (second termEnv') xs) (eval' s) (eval' s :: Interp (SW.Categories (Strat,StratEnv) (TermEnv,Term) SW.Stack) Term Term) where
    --   ctx = (alpha::C.Pow C.Term -> Context) (C.fromFoldable (map fst xs)) `union` patContext tenv pat
    --   tenv = (termEnv [ (v,(alpha::C.Pow C.Term -> Term) (C.singleton t)) | (_,xs') <- xs, (v,t) <- xs'])

    -- patContext :: TermEnv -> TermPattern -> Context
    -- patContext tenv pat = case pat of
      -- Cons c pats -> SortContext
      --   { signatures = unionsWith (++) (M.singleton c [(map (patToSort tenv) pats, patToSort tenv pat)]:map (signatures . (patContext tenv)) pats)
      --   , lexicals = Set.empty
      --   , injectionClosure = M.empty -- Since we're taking the union, we can just leave this empty.
      --   }
      -- _ -> SortContext { signatures = M.empty, lexicals = Set.empty, injectionClosure = M.empty }

    -- patToSort :: TermEnv -> TermPattern -> Sort
    -- patToSort _ (Cons (Constructor c) _) = Sort (SortId c)
    -- patToSort _ (StringLiteral _) = Lexical
    -- patToSort _ (NumberLiteral _) = Numerical
    -- patToSort _ (As _ _) = error "no such sort: as"
    -- patToSort _ (Explode _ _) = error "no such sort: explode"
    -- patToSort tenv (Var x) = case S.lookup x tenv of
    --   M.Just (Term s _) -> s
    --   M.JustNothing (Term s _) -> s
    --   M.Nothing -> Bottom

    -- termEnv' = C.TermEnv . M.fromList
    termEnv :: [(TermVar, Term)] -> TermEnv Term
    termEnv = S.fromList

    -- termEnv' = S.fromThereList
    termEnv' :: [(TermVar, A.Maybe Term)] -> TermEnv Term
    -- termEnv' ts = termEnv (filter (\(_,)))
    -- termEnv' :: [(TermVar, A.Maybe Term)] -> TermEnv
    termEnv' = S.fromThereList . concatMap (\(v,mt) -> case mt of
      A.Nothing -> []
      A.Just t -> [(v,(Must, t))]
      A.JustNothing t -> [(v,(May, t))]
     )

    termEnv''  :: [(TermVar, Term)] -> [TermVar] -> TermEnv Term
    termEnv'' bound = foldr S.delete (S.fromList bound)

    -- delete :: TermVars s => s -> TermEnv Term -> TermEnv Term
    -- delete s = S.delete' (termVars s :: Set TermVar)

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

    seval :: (?sensitivity :: Int) => LStrat -> Term -> Error TypeError (Except () (TermEnv Term,Term))
    seval s = seval'' 10 s (stratEnv []) emptyEnv

    seval' :: (?sensitivity :: Int) => LStrat -> TermEnv Term -> Term -> Error TypeError (Except () (TermEnv Term,Term))
    seval' s = seval'' 10 s (stratEnv [])

    seval'' :: (?sensitivity :: Int) => Int -> LStrat -> LStratEnv -> TermEnv Term -> Term -> Error TypeError (Except () (TermEnv Term,Term))
    seval'' j s senv tenv t =
      fromCompletion (error "top element")
        (fromTerminating (error "sort semantics does not terminate")
           (eval j s senv (context t) tenv t))

    sevalNoNeg'' :: (?sensitivity :: Int) => Int -> LStrat -> LStratEnv -> TermEnv Term -> Term -> Error TypeError (Except () (TermEnv Term,Term))
    sevalNoNeg'' j s senv tenv t = dropNegativeBindings $ seval'' j s senv tenv t

    dropNegativeBindings :: Error TypeError (Except () (TermEnv Term,a)) -> Error TypeError (Except () (TermEnv Term,a))
    -- dropNegativeBindings (F.Success (E.Success (env,a))) = (F.Success (E.Success (S.dropNegativeBindings env,a)))
    -- dropNegativeBindings (F.Success (E.SuccessOrfail e (env,a))) = (F.Success (E.SuccessOrfail e (S.dropNegativeBindings env,a)))
    dropNegativeBindings res = res

    term :: (?ctx :: Context) => Sort -> Term
    term s = Term s ?ctx

    bottom :: (?ctx :: Context) => Term
    bottom = term Bottom

    lexical :: (?ctx :: Context) => Term
    lexical = term Lexical

    numerical :: (?ctx :: Context) => Term
    numerical = term Numerical

    -- top :: (?ctx :: Context) => Term
    -- top = term Top

    -- atop :: (?ctx :: Context) => A.Maybe Term
    -- atop = A.JustNothing top

    may :: k -> v -> (k,A.Maybe v)
    may k v = (k,A.JustNothing v)

    -- must :: k -> v -> (k,A.Maybe v)
    -- must k v = (k,A.Just v)

    -- notThere :: k -> (k, A.Maybe v) 
    -- notThere k = (k,A.Nothing)

    success :: a -> Error e (Except () a)
    success a = F.Success $ E.Success a

    successOrfail :: () -> a -> Error e (Except () a)
    successOrfail () a = F.Success $ E.SuccessOrFail () a

    uncaught :: () -> Error e (Except () a)
    uncaught = F.Success . E.Fail

    failure :: String -> Error TypeError (Except () a)
    failure = F.Fail . fromString

    caseStudy :: IO (Context,LStratEnv) -> ((?ctx :: Context) => SpecWith LStratEnv) -> Spec
    caseStudy loadCaseStudy spc = do
      (cntx,senv) <- runIO loadCaseStudy
      let ?ctx = cntx in beforeAll (return senv) spc
