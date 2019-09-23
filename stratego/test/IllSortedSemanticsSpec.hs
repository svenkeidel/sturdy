{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
module IllSortedSemanticsSpec(main, spec) where

import Prelude hiding (exp)

import           Syntax hiding (Fail)
import           Syntax as T
import           IllSortedSemantics
import           AbstractInterpreter
import           SortContext(Context,Sort(..))
import qualified SortContext as Ctx
import           Abstract.TermEnvironment

import           Control.Arrow

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

import qualified CaseStudy

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  return ()
  -- describe "Utilities" $ do
  --   it "convertToList should work with identical sorts" $
  --     let ?ctx = Ctx.empty in
  --     convertToList [ lexical, lexical ] ?ctx `shouldBe` term (List Lexical)

  --   it "convertToList should work with different sorts" $
  --     let ?ctx = Ctx.empty in
  --     convertToList [ lexical, lexical, numerical ] ?ctx `shouldBe` term (List Top)

  --   it "convertToList should work on an empty list" $
  --     let ?ctx = Ctx.empty in
  --     convertToList [] ?ctx `shouldBe` term (List Bottom)

  -- describe "Match" $ do
  --   it "should match an identical builtin string literal" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 0 in
  --     seval (Match (StringLiteral "x")) lexical `shouldBe`
  --       successOrFail () (emptyEnv, lexical)

  --   it "should match another builtin string literal" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 0 in
  --     seval (Match (StringLiteral "y")) lexical `shouldBe`
  --       successOrFail () (emptyEnv, lexical)

  --   it "should match an equal builtin number literal" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 0 in
  --     seval (Match (NumberLiteral 42)) numerical `shouldBe`
  --       successOrFail () (emptyEnv, numerical)

  --   it "should match another builtin number literal" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 0 in
  --     seval (Match (NumberLiteral 1)) numerical `shouldBe`
  --       successOrFail () (emptyEnv, numerical)

  --   it "a string literal should not match a number literal" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 0 in
  --     seval (Match (NumberLiteral 1)) lexical `shouldBe`
  --       uncaught ()

  --   it "a number literal should not match a string literal" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 0 in
  --     seval (Match (StringLiteral "x")) numerical `shouldBe`
  --       uncaught ()

  --   it "should match a PCF expression" $
  --     let ?ctx = Ctx.fromList [("Zero",[],"Exp")]
  --         ?sensitivity = 0 in
  --     let t = term "Exp" in
  --     seval (Match (Cons "Zero" [])) t `shouldBe`
  --       success (emptyEnv,t)

  --   it "should match a nested PCF expression" $
  --     let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")]
  --         ?sensitivity = 0 in
  --     let t = term "Exp" in
  --     seval (Match (Cons "Succ" [Cons "Zero" []])) t `shouldBe`
  --       successOrFail () (emptyEnv, t)

  --   it "should match a constructor with more than one argument" $
  --     let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp") ,("Zero",[],"Exp") ,("Ifz",["Exp","Exp","Exp"],"Exp")]
  --         ?sensitivity = 0 in
  --     let t = term "Exp" in
  --     seval (Match (Cons "Ifz" [Cons "Zero" [], Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) t `shouldBe`
  --       successOrFail () (emptyEnv, t)

  --   it "should introduce one variable" $
  --     let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")]
  --         ?sensitivity = 0 in
  --     let t = term "Exp" in
  --     seval (Match (Cons "Succ" ["x"])) t `shouldBe`
  --       successOrFail () (termEnv [("x", t)], t)

  --   it "should introduce one variable 2" $
  --     let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")]
  --         ?sensitivity = 0 in
  --     let t = term "Exp" in
  --     seval (Match (Cons "Succ" ["x"])) t `shouldBe`
  --       successOrFail () (termEnv [("x", t)], t)

  --   it "should introduce multiple variables and support linear pattern matching" $
  --     let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")]
  --         ?sensitivity = 0 in
  --     let t = term "Exp" in
  --     seval (Match (Cons "Succ" ["x"]) `Seq` Match (Cons "Succ" ["y"])) t `shouldBe`
  --        successOrFail () (termEnv [("x", t), ("y", t)], t)

  --   it "should support linear pattern matching" $
  --     let ?ctx = Ctx.fromList []
  --         ?sensitivity = 0 in
  --     let t = term (Tuple [Lexical, Lexical]) in
  --     seval (Match (Cons "" ["x", "x"])) t `shouldBe`
  --       successOrFail () (termEnv [("x",lexical)],t)

  --   it "should succeed when exploding literals" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 0 in
  --     let tenv = termEnv [("x", term (List Bottom))] in
  --     seval' (Match (Explode "_" "x")) (S.delete "x" emptyEnv) numerical `shouldBe`
  --       success (tenv, numerical)

  --   -- it "should handle inconsistent environments" $ do
  --   --   let t1 = C.Cons "f" []
  --   --       t2 = C.Cons "g" []
  --   --   sound' (Match "x") [(t1, [("x", t1)]), (t2, [("y", t2)])]

  --   -- prop "should be sound" $ do
  --   --   [t1,t2,t3] <- C.similarTerms 3 7 2 10
  --   --   matchPattern <- C.similarTermPattern t1 3
  --   --   return $ counterexample
  --   --              (printf "pattern: %s\n %s ⊔ %s = %s"
  --   --                 (show matchPattern) (show t2) (show t3)
  --   --                 (showLub t2 t3))
  --   --          $ sound' (Match matchPattern) [(t2,[]),(t3,[])]

  -- describe "Build" $ do
  --   it "should build a builtin string literal" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 0 in
  --     seval (Build (StringLiteral "foo")) bottom `shouldBe`
  --       success (emptyEnv, lexical)

  --   it "should build a builtin number literal" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 0 in
  --     seval (Build (NumberLiteral 1)) bottom `shouldBe`
  --       success (emptyEnv, numerical)

  --   it "a string grammar should not be build on a number literal" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 0 in
  --     seval (Build (NumberLiteral 1)) bottom `shouldNotBe`
  --       success (emptyEnv, lexical)

  --   it "a number grammar should not be build on a string literal" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 0 in
  --     seval (Match (StringLiteral "x")) bottom `shouldNotBe`
  --       success (emptyEnv, numerical)

  --   it "should build a simple constant PCF expression" $
  --     let ?ctx = Ctx.fromList [("Zero",[],"Exp")]
  --         ?sensitivity = 0 in
  --     let t = bottom
  --         t' = term "Exp"
  --     in seval (Build (Cons "Zero" [])) t `shouldBe` success (emptyEnv, t')

  --   it "should build a nested PCF expression" $
  --     let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")]
  --         ?sensitivity = 0 in
  --     let t = bottom
  --         t' = term "Exp"
  --     in seval (Build (Cons "Succ" [Cons "Zero" []])) t `shouldBe` success (emptyEnv, t')

  --   it "should build a constructor with more than one argument" $
  --     let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp") ,("Zero",[],"Exp") ,("Ifz",["Exp","Exp","Exp"],"Exp")]
  --         ?sensitivity = 0 in
  --     let t = term "Exp"
  --     in seval (Build (Cons "Ifz" [Cons "Zero" [], Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) t `shouldBe`
  --       success (emptyEnv, t)

  --   it "build should be inverse to match" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 0 in
  --     let pat = NumberLiteral 1
  --     in seval (Match pat `Seq` Build pat) numerical `shouldBe` successOrFail () (emptyEnv, numerical)

  --   it "build should be inverse to match with a more complicated term" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 0 in
  --     let pat = Cons "Cons" [Var "x", Var "xs"]
  --         t = convertToList [numerical] ?ctx
  --         tenv = termEnv [("x", numerical), ("xs", term (List Numerical))]
  --     in seval' (Match pat `Seq` Build pat) tenv t `shouldBe` successOrFail () (tenv, t)

  --   it "should throw away the current subject term if needed" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 0 in
  --     let tenv = termEnv [("x", numerical)]
  --     in seval' (Build (Var "x")) tenv lexical `shouldBe` success (tenv, numerical)

  --   it "should lookup variables" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 0 in
  --     let tenv = termEnv [("x", numerical)]
  --     in seval' (Build (Var "x")) tenv bottom `shouldBe` success (tenv, numerical)

  --   it "should merge two variables into one term" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 0 in
  --     let tenv = termEnv [("x", numerical), ("y", term (List Lexical))]
  --         t = bottom
  --     in seval' (Build (Cons "Cons" [Var "x", Var "y"])) tenv t `shouldBe` success (tenv, term (List Top))

  --   it "should properly construct a list of the same type" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 0 in
  --     let tenv = termEnv [("x", numerical), ("y", term (List Numerical))]
  --         t = bottom
  --     in seval' (Build (Cons "Cons" [Var "x", Var "y"])) tenv t `shouldBe` success (tenv, term (List Numerical))

  --   it "should merge a variable and the given subject term" $
  --     let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp") ,("Zero",[],"Exp") ,("Ifz",["Exp","Exp","Exp"],"Exp")]
  --         ?sensitivity = 0 in
  --     let t = bottom
  --         tenv = termEnv [("x", term "Exp")]
  --     in seval' (Build (Cons "Ifz" [Var "x", Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) tenv t `shouldBe`
  --       success (tenv, term "Exp")

  --   -- prop "should be sound" $ do
  --   --   [t1,t2,t3] <- C.similarTerms 3 7 2 10
  --   --   matchPattern <- C.similarTermPattern t1 3
  --   --   let vars = patternVars' matchPattern
  --   --   buildPattern <- arbitraryTermPattern 5 2 (if not (null vars) then elements vars else arbitrary)
  --   --   return $ counterexample
  --   --            (printf "match pattern: %s\nbuild pattern: %s\nt2: %s\nt3: %s\nlub t2 t3 = %s"
  --   --              (show matchPattern) (show buildPattern) (show t2) (show t3)
  --   --              (showLub t2 t3))
  --   --          $ sound'' (Match matchPattern `Seq` Build buildPattern) [(t2,[]),(t3,[])] buildPattern

  -- describe "Scope" $ do
  --   it "should hide declared variables" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 0 in
  --     let tenv = termEnv [("x", numerical)]
  --     in do
  --        seval' (Scope ["x"] (Build "x")) tenv numerical `shouldBe` failure "unbound term variable x in build statement !x"
  --        seval' (Scope ["x"] (Match "x")) tenv numerical `shouldBe` success (tenv, numerical)

  --   it "should make non-declared variables available" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 0 in
  --     let tenv = termEnv [ ("x",numerical) ]
  --     in do
  --        seval' (Scope ["y"] (Build "x")) tenv numerical `shouldBe`
  --          success (termEnv [("x", numerical) ], numerical)
  --        seval' (Scope ["y"] (Match "z")) tenv numerical `shouldBe`
  --          success (termEnv [ ("x",numerical), ("z",numerical)], numerical)

  --   it "should hide variables bound in a choice's test from the else branch" $
  --     let ?ctx = Ctx.fromList [("Zero",[],"Exp"), ("One",[],"Exp")]
  --         ?sensitivity = 0 in
  --     let exp = term "Exp"
  --         tenv = emptyEnv
  --         prog = (Build (T.Cons "Zero" []) `Seq` Match "x" `Seq` T.Fail)
  --               `leftChoice`
  --               (Match "x")
  --     in seval' prog tenv exp `shouldBe` success (termEnv [("x", exp)], exp)

  -- describe "Let" $ do
  --   it "should apply a single function call" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 2 in
  --     let t = term (Tuple ["Exp","Exp"])
  --         -- tenv = termEnv [("x",t)]
  --         tenv = delete swap (termEnv [])
  --     in seval' (Let [("swap", swap)] (Scope ["x"] (Match "x" `Seq` Call "swap" [] ["x"]))) tenv t
  --          `shouldBe` success (tenv, t)

  --   it "should support recursion" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 2 in
  --     let t = convertToList [numerical, numerical, numerical] ?ctx
  --         tenv = delete map' $ termEnv []
  --     in seval'' 0 10 (Let [("map", map')] (Scope ["x"] (Match "x" `Seq` Call "map" [Build (NumberLiteral 1)] ["x"]))) M.empty tenv t
  --       `shouldBe` success (tenv, term (List Numerical))

  -- describe "Call" $ do
  --   it "should apply a single function call" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 1 in
  --     let senv = M.singleton "swap" (Closure swap M.empty)
  --         t = term (Tuple ["Exp","Exp"])
  --         tenv = delete swap $ termEnv []
  --     in seval'' 0 10 (Scope ["x"] (Match "x" `Seq` Call "swap" [] [])) senv tenv t `shouldBe` success (tenv, t)

  --   it "should support an empty list in recursive applications" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 2 in
  --     let senv = M.singleton "map" (Closure map' M.empty)
  --         t = convertToList [] ?ctx
  --         tenv = delete map' $ termEnv []
  --     in seval'' 0 10 (Scope ["x"] (Match "x" `Seq` Call "map" [Build (NumberLiteral 1)] ["x"])) senv tenv t `shouldBe`
  --          success (delete map' tenv, term (List Numerical))

  --   it "should support a singleton list in recursive applications" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 2 in
  --     let senv = M.singleton "map" (Closure map' M.empty)
  --         t = convertToList [numerical] ?ctx
  --         tenv = delete map' $ termEnv []
  --     in seval'' 0 10 (Scope ["x"] (Match "x" `Seq` Call "map" [Build (NumberLiteral 1)] ["x"])) senv tenv t `shouldBe`
  --          success (tenv, term (List Numerical))

  --   it "should support recursion on a list of numbers" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 2 in
  --     let senv = M.singleton "map" (Closure map' M.empty)
  --         t = convertToList [numerical, numerical, numerical] ?ctx
  --         tenv = delete map' $ termEnv []
  --     in seval'' 0 10 (Scope ["x"] (Match "x" `Seq` Call "map" [Build (NumberLiteral 1)] ["x"])) senv tenv t `shouldBe`
  --          success (tenv, term (List Numerical))

  --   it "should terminate and not produce infinite sorts" $
  --     let ?ctx = Ctx.empty
  --         ?sensitivity = 0 in
  --     let senv = M.fromList [("map",Closure map' M.empty),
  --                            ("foo",Closure (Strategy [] [] (Scope ["x"] (Match "x" `Seq` Call "map" ["foo"] ["x"]))) M.empty)]
  --         t = term Top
  --         tenv = delete map' $ termEnv []
  --     in do
  --       pendingWith "does not terminate"
  --       seval'' 0 0 (Call "foo" [] []) senv tenv t `shouldBe`
  --          success (tenv, term (List (List (List Top))))

  -- describe "simplify arithmetics" $ do
  --   it "reduce Add(Zero,y)" $
  --     let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")]
  --         ?sensitivity = 0 in
  --     let exp = term "Exp"
  --         reduce = Match (Cons "Add" [Cons "Zero" [], "y"]) `Seq` Build "y"
  --     in seval' reduce emptyEnv exp `shouldBe` successOrFail () (termEnv [("y", exp)], exp)

  --   it "reduce Add(x,Zero)" $
  --     let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")]
  --         ?sensitivity = 0 in
  --     let exp = term "Exp"
  --         reduce = Match (Cons "Add" ["x", Cons "Zero" []]) `Seq` Build "x"
  --     in seval' reduce emptyEnv exp `shouldBe` successOrFail () (termEnv [("x", exp)], exp)

  --   it "reduce Add(Zero,y) < id + Add(x,Zero)" $
  --     let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")]
  --         ?sensitivity = 0 in
  --     let exp = term "Exp"
  --         reduce1 = Match (Cons "Add" [Cons "Zero" [], "y"]) `Seq` Build "y"
  --         reduce2 = Match (Cons "Add" ["x", Cons "Zero" []]) `Seq` Build "x"
  --     in seval' (reduce1 `leftChoice` reduce2) emptyEnv exp `shouldBe` successOrFail () (termEnv [("x",exp), ("y",exp)], exp)

  --   it "reduce Add(x,y); !x; ?Zero()" $
  --     let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")]
  --         ?sensitivity = 0 in
  --     let exp = term "Exp"
  --         reduce = Match (Cons "Add" ["x", "y"]) `Seq` Build "x" `Seq` Match (Cons "Zero" []) `Seq` Build "y"
  --     in seval' reduce emptyEnv exp `shouldBe` successOrFail () (termEnv [("x", exp),("y", exp)], exp)

  --   it "reduce Double(x)" $
  --     let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp"),("Double",["Exp"],"Exp")]
  --         ?sensitivity = 0 in
  --     let exp = term "Exp"
  --         reduce = Match (Cons "Double" ["x"]) `Seq` Build (Cons "Add" ["x", "x"])
  --     in seval' reduce emptyEnv exp `shouldBe` successOrFail () (termEnv [("x", exp)], exp)

  --   it "reduce Add(Zero,y) <+ Add(x,Zero) <+ Double(x)" $
  --     let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")]
  --         ?sensitivity = 0 in
  --     let exp = term "Exp"
  --         reduce1 = Match (Cons "Add" [Cons "Zero" [], "y"]) `Seq` Build "y"
  --         reduce2 = Match (Cons "Add" ["x", Cons "Zero" []]) `Seq` Build "x"
  --         reduce3 = Match (Cons "Double" ["x"]) `Seq` Build (Cons "Add" ["x", "x"])
  --     in seval' (reduce1 `leftChoice` reduce2 `leftChoice` reduce3) emptyEnv exp
  --          `shouldBe` successOrFail () (termEnv [("x",exp),("y",exp)], exp)

  --   -- prop "should be sound" $ do
  --   --   i <- choose (0,10)
  --   --   j <- choose (0,10)
  --   --   l <- C.similarTerms i 7 2 10
  --   --   let (l1,l2) = splitAt j l
  --   --   let t1 = C.convertToList l1
  --   --   let t2 = C.convertToList l2
  --   --   return $ counterexample (printf "t: %s\n" (showLub t1 t2))
  --   --          $ sound' (Let [("map", map')] (Match "x" `Seq` Call "map" [Build 1] ["x"])) [(t1,[]),(t2,[])]

  -- describe "Boolean Algebra" $
  --   beforeAll CaseStudy.balg $ do
  --     it "trans_bottomup: BExp -> NExp" $ \balg ->
  --       let ?ctx = signature balg
  --           ?sensitivity = 0 in
  --       let senv = stratEnv balg
  --           tenv = delete balg $ emptyEnv
  --       in do
  --         seval'' 0 10 (Call "trans__bottomup_0_0" [] []) senv tenv (term "BExp") `shouldBe`
  --           successOrFail () (tenv, term "NExp")

  --     it "trans_topdown: BExp -> NExp" $ \balg ->
  --       let ?ctx = signature balg
  --           ?sensitivity = 5 in
  --       let senv = stratEnv balg
  --           tenv = emptyEnv
  --       in do
  --         seval'' 0 10 (Call "trans__topdown_0_0" [] []) senv tenv (term "BExp") `shouldBe`
  --           successOrFail () (tenv, term "NExp")

  -- describe "PCF" $
  --   beforeAll CaseStudy.pcf $ do
  --     it "lookup: String * Env -> Val" $ \pcf ->
  --       let ?ctx = signature pcf
  --           ?sensitivity = 0 in
  --       let senv = stratEnv pcf
  --           prog = term (Tuple [Lexical, List (Tuple [Lexical, "Val"])])
  --           val  = term "Val"
  --           tenv = delete pcf emptyEnv
  --       in do
  --         seval'' 0 10 (Call "lookup_0_0" [] []) senv tenv prog `shouldBe`
  --           successOrFail () (delete (senv M.! "lookup_0_0") tenv, val)

  --     it "eval: Env * Exp -> Val" $ \pcf ->
  --       let ?ctx = signature pcf
  --           ?sensitivity = 0 in
  --       let senv = stratEnv pcf
  --           prog = term (Tuple [List (Tuple [Lexical, "Val"]), "Exp"])
  --           val  = term "Val"
  --           tenv = delete pcf emptyEnv
  --       in do
  --         seval'' 0 10 (Call "eval_0_0" [] []) senv tenv prog `shouldBe`
  --            successOrFail () (tenv, val)

  -- describe "Arrow" $
  --   beforeAll CaseStudy.arrows $ do
  --     it "tuple-pat: List Var -> APat" $ \desugar ->
  --       let ?ctx = signature desugar
  --           ?sensitivity = 0 in
  --       let senv = stratEnv desugar
  --           prog = term $ List "Var"
  --           val  = term "APat"
  --           tenv = termEnv []
  --       in do
  --         seval'' 0 10 (Call "tuple_pat_0_0" [] []) senv tenv prog `shouldBe`
  --           successOrFail () (termEnv [], val)

  --     it "tuple: List Var -> Exp" $ \desugar ->
  --       let ?ctx = signature desugar
  --           ?sensitivity = 0 in
  --       let senv = stratEnv desugar
  --           prog = term $ List "Var"
  --           val  = term "Exp"
  --           env = delete desugar $ termEnv []
  --       in do
  --         seval'' 0 10 (Call "tuple_0_0" [] []) senv env prog `shouldBe`
  --           successOrFail () (env, val)

  --     it "tuple-exp: List Exp -> Exp" $ \desugar ->
  --       let ?ctx = signature desugar
  --           ?sensitivity = 0 in
  --       let senv = stratEnv desugar
  --           prog = term $ List "Exp"
  --           val  = term "Exp"
  --           env = termEnv []
  --       in do
  --         seval'' 0 10 (Call "tuple_exp_0_0" [] []) senv env prog `shouldBe`
  --           successOrFail () (env, val)

  --     it "at-end: (=> List Var) -> List Var -> List Var " $ \desugar ->
  --       let ?ctx = signature desugar
  --           ?sensitivity = 0 in
  --       let senv = stratEnv desugar
  --           prog = term $ List "Var"
  --           val  = term $ List "Var"
  --           env = termEnv []
  --           s = Build $ Cons "Cons" [Cons "VarVar" [StringLiteral "v"], Cons "Nil" []]
  --       in do
  --         seval'' 0 10 (Call "at_end_1_0" [s] []) senv env prog `shouldBe`
  --           successOrFail () (env, val)

  --     it "at-end (scoped strat arg): (=> List Var) -> List Var -> List Var " $ \desugar ->
  --       let ?ctx = signature desugar
  --           ?sensitivity = 0 in
  --       let senv = stratEnv desugar
  --           prog = term $ List "Var"
  --           val  = term $ List "Var"
  --           env = termEnv [("vars-list", term $ List "Var")]
  --           s = Build $ Var "vars-list"
  --       in do
  --         seval'' 0 10 (Call "at_end_1_0" [s] []) senv env prog `shouldBe`
  --           successOrFail () (env, val)

  --     it "conc: (List Var, List Var) -> List Var " $ \desugar ->
  --       let ?ctx = signature desugar
  --           ?sensitivity = 0 in
  --       let senv = stratEnv desugar
  --           prog = term $ Tuple [List "Var", List "Var"]
  --           val  = term $ List "Var"
  --           env = termEnv []
  --       in do
  --         seval'' 0 10 (Call "conc_0_0" [] []) senv env prog `shouldBe`
  --           successOrFail () (env, val)

  --     it "free-pat-vars: APat -> List Var " $ \desugar ->
  --       let ?ctx = signature desugar
  --           ?sensitivity = 0 in
  --       let senv = stratEnv desugar
  --           prog = term "APat"
  --           val  = term $ List "Var"
  --           env = termEnv []
  --       in do
  --         seval'' 0 10 (Call "free_pat_vars_0_0" [] []) senv env prog `shouldBe`
  --           successOrFail () (env, val)

  --     it "free-decls-vars: Declbinds -> List Var " $ \desugar ->
  --       let ?ctx = signature desugar
  --           ?sensitivity = 0 in
  --       let senv = stratEnv desugar
  --           prog = term "Declbinds"
  --           val  = term $ List "Var"
  --           env = termEnv []
  --       in do
  --         seval'' 0 10 (Call "free_decls_vars_0_0" [] []) senv env prog `shouldBe`
  --           successOrFail () (env, val)

  --     it "desugar-arrow': ArrCommand -> Exp" $ \desugar ->
  --       let ?ctx = signature desugar
  --           ?sensitivity = 0 in
  --       let senv = stratEnv desugar
  --           prog = term "ArrCommand"
  --           val  = term "Exp"
  --           env = termEnv [("vars-list", term $ List "Var")]
  --       in do
  --         seval'' 0 10 (Call "desugar_arrow_p__0_1" [] [TermVar "vars-list"]) senv env prog `shouldBe`
  --           successOrFail () (env, val)

  -- where
  --   termEnv :: [(TermVar, Term)] -> TermEnv Term
  --   termEnv = S.fromList

  --   delete :: TermVars s => s -> TermEnv Term -> TermEnv Term
  --   delete s = S.delete' (termVars s :: Set TermVar)

  --   emptyEnv :: TermEnv Term
  --   emptyEnv = S.empty

  --   -- showLub :: C.Term -> C.Term -> String
  --   -- showLub t1 t2 = show (alpha (C.fromFoldable [t1,t2] :: C.Pow C.Term) :: Term)

  --   swap = Strategy [] [] (Scope ["x","y"] (Match (Cons "" ["x","y"]) `Seq` Build (Cons "" ["y","x"])))

  --   map' = Strategy ["f"] ["l"] (Scope ["x","xs","x'","xs'"] (
  --           Build "l" `Seq`
  --           GuardedChoice
  --             (Match (Cons "Cons" ["x","xs"]))
  --             (Build "x" `Seq`
  --              Call "f" [] [] `Seq`
  --              Match "x'" `Seq`
  --              Call "map" ["f"] ["xs"] `Seq`
  --              Match "xs'" `Seq`
  --              Build (Cons "Cons" ["x'", "xs'"]))
  --             (Build (Cons "Nil" []))))

  --   seval :: (?sensitivity :: Int, ?ctx :: Context) => Strat -> Term -> Error TypeError (Except () (TermEnv Term,Term))
  --   seval s = seval'' 0 10 s M.empty emptyEnv

  --   seval' :: (?sensitivity :: Int, ?ctx :: Context) => Strat -> TermEnv Term -> Term -> Error TypeError (Except () (TermEnv Term,Term))
  --   seval' s = seval'' 0 10 s M.empty

  --   seval'' :: (?sensitivity :: Int, ?ctx :: Context) => Int -> Int -> Strat -> StratEnv -> TermEnv Term -> Term -> Error TypeError (Except () (TermEnv Term,Term))
  --   seval'' j k s senv tenv t =
  --     fmap (fmap (fmap (typecheck' ?ctx) *** typecheck' ?ctx)) $
  --     fromCompletion (error "top element")
  --       (fromTerminating (error "sort semantics does not terminate")
  --          (eval j k s senv ?ctx tenv t))

  --   term :: (?ctx :: Context) => Sort -> Term
  --   term s = Sorted s ?ctx

  --   bottom :: (?ctx :: Context) => Term
  --   bottom = term Bottom

  --   lexical :: (?ctx :: Context) => Term
  --   lexical = term Lexical

  --   numerical :: (?ctx :: Context) => Term
  --   numerical = term Numerical

  --   success :: a -> Error e (Except () a)
  --   success a = F.Success $ E.Success a

  --   successOrFail :: () -> a -> Error e (Except () a)
  --   successOrFail () a = F.Success $ E.SuccessOrFail () a

  --   uncaught :: () -> Error e (Except () a)
  --   uncaught = F.Success . E.Fail

  --   failure :: String -> Error TypeError (Except () a)
  --   failure = F.Fail . fromString
