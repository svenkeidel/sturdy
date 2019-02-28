{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
module SortSemanticsSpec(main, spec) where

import Prelude hiding (exp)

-- import qualified ConcreteSemantics as C
-- import           Soundness
-- import           Sort (SortId(..))
import           Syntax hiding (Fail)
import           Syntax as T
import           SortSemantics -- hiding (sortContext)
import           SortContext(Context,Sort(..))
import qualified SortContext as Ctx

-- import           Control.Arrow

import           Data.Abstract.FreeCompletion (fromCompletion)
import           Data.Abstract.Except as E
import           Data.Abstract.Error as F
import qualified Data.Abstract.Maybe as A
-- import qualified Data.Abstract.Maybe as M
import qualified Data.Abstract.Map as S
import           Data.Abstract.There
-- import qualified Data.Abstract.StackWidening as SW
import           Data.Abstract.Terminating (fromTerminating)
-- import qualified Data.Concrete.Powerset as C
-- import           Data.Constructor
-- import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
-- import           Data.GaloisConnection
-- import qualified Data.Set as Set
-- import qualified Data.Term as C
import           Data.Set (Set)
    
import           Test.Hspec hiding (context)
-- import           Test.Hspec.QuickCheck
-- import           Test.QuickCheck hiding (Success)

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

  describe "Match" $ do
    it "should match an identical builtin string literal" $
      let ?ctx = Ctx.empty in
      seval 0 (Match (StringLiteral "x")) lexical `shouldBe` successOrFail () (emptyEnv, lexical)

    it "should match another builtin string literal" $
      let ?ctx = Ctx.empty in
      seval 0 (Match (StringLiteral "y")) lexical `shouldBe` successOrFail () (emptyEnv, lexical)

    it "should match an equal builtin number literal" $
      let ?ctx = Ctx.empty in
      seval 0 (Match (NumberLiteral 42)) numerical `shouldBe` successOrFail () (emptyEnv, numerical)

    it "should match another builtin number literal" $
      let ?ctx = Ctx.empty in
      seval 0 (Match (NumberLiteral 1)) numerical `shouldBe`  successOrFail () (emptyEnv, numerical)

    it "a string literal should not match a number literal" $
      let ?ctx = Ctx.empty in
      seval 0 (Match (NumberLiteral 1)) lexical `shouldBe` uncaught ()

    it "a number literal should not match a string literal" $
      let ?ctx = Ctx.empty in
      seval 0 (Match (StringLiteral "x")) numerical `shouldBe` uncaught ()

    it "should match a PCF expression" $
      let ?ctx = Ctx.fromList [("Zero",[],"Exp")] in
      let t = term "Exp"
      in seval 0 (Match (Cons "Zero" [])) t `shouldBe` success (emptyEnv, t)

    it "should match a nested PCF expression" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")] in
      let t = term "Exp"
      in seval 0 (Match (Cons "Succ" [Cons "Zero" []])) t `shouldBe` successOrFail () (emptyEnv, t)

    it "should match a constructor with more than one argument" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp") ,("Zero",[],"Exp") ,("Ifz",["Exp","Exp","Exp"],"Exp")] in
      let t = term "Exp"
      in seval 0 (Match (Cons "Ifz" [Cons "Zero" [], Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) t `shouldBe`
        successOrFail () (emptyEnv, t)

    it "should introduce one variable" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")] in
      let t = term "Exp"
      in seval 0 (Match (Cons "Succ" ["x"])) t `shouldBe` successOrFail () (termEnv [("x", t)], t)

    it "should introduce one variable 2" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")] in
      let t = term "Exp"
      in seval 0 (Match (Cons "Succ" ["x"])) t `shouldBe` successOrFail () (termEnv [("x", t)], t)

    it "should introduce multiple variables and support linear pattern matching" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")] in
      let t = term "Exp"
      in seval 0 (Match (Cons "Succ" ["x"]) `Seq` Match (Cons "Succ" ["y"])) t `shouldBe`
         successOrFail () (termEnv [("x", t), ("y", t)], t)

    it "should support linear pattern matching" $
      let ?ctx = Ctx.fromList [] in
      let t = term (Tuple [Lexical, Lexical])
      in seval 0 (Match (Cons "" ["x", "x"])) t `shouldBe` successOrFail () (termEnv [("x",lexical)],t)

    it "should succeed when exploding literals" $
      let ?ctx = Ctx.empty in
      let tenv = termEnv [("x", term (List Bottom))]
      in seval' 0 (Match (Explode "_" "x")) (S.delete "x" emptyEnv) numerical `shouldBe` success (tenv, numerical)

    -- it "should handle inconsistent environments" $ do
    --   let t1 = C.Cons "f" []
    --       t2 = C.Cons "g" []
    --   sound' (Match "x") [(t1, [("x", t1)]), (t2, [("y", t2)])]

    -- prop "should be sound" $ do
    --   [t1,t2,t3] <- C.similarTerms 3 7 2 10
    --   matchPattern <- C.similarTermPattern t1 3
    --   return $ counterexample
    --              (printf "pattern: %s\n %s ⊔ %s = %s"
    --                 (show matchPattern) (show t2) (show t3)
    --                 (showLub t2 t3))
    --          $ sound' (Match matchPattern) [(t2,[]),(t3,[])]

  describe "Build" $ do
    it "should build a builtin string literal" $
      let ?ctx = Ctx.empty in
      seval 0 (Build (StringLiteral "foo")) bottom `shouldBe` success (emptyEnv, lexical)

    it "should build a builtin number literal" $
      let ?ctx = Ctx.empty in
      seval 0 (Build (NumberLiteral 1)) bottom `shouldBe` success (emptyEnv, numerical)

    it "a string grammar should not be build on a number literal" $
      let ?ctx = Ctx.empty in
      seval 0 (Build (NumberLiteral 1)) bottom `shouldNotBe` success (emptyEnv, lexical)

    it "a number grammar should not be build on a string literal" $
      let ?ctx = Ctx.empty in
      seval 0 (Match (StringLiteral "x")) bottom `shouldNotBe` success (emptyEnv, numerical)

    it "should build a simple constant PCF expression" $
      let ?ctx = Ctx.fromList [("Zero",[],"Exp")] in
      let t = bottom
          t' = term "Exp"
      in seval 0 (Build (Cons "Zero" [])) t `shouldBe` success (emptyEnv, t')

    it "should build a nested PCF expression" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")] in
      let t = bottom
          t' = term "Exp"
      in seval 0 (Build (Cons "Succ" [Cons "Zero" []])) t `shouldBe` success (emptyEnv, t')

    it "should build a constructor with more than one argument" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp") ,("Zero",[],"Exp") ,("Ifz",["Exp","Exp","Exp"],"Exp")] in
      let t = term "Exp"
      in seval 0 (Build (Cons "Ifz" [Cons "Zero" [], Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) t `shouldBe`
        success (emptyEnv, t)

    it "build should be inverse to match" $
      let ?ctx = Ctx.empty in
      let pat = NumberLiteral 1
      in seval 0 (Match pat `Seq` Build pat) numerical `shouldBe` successOrFail () (emptyEnv, numerical)

    it "build should be inverse to match with a more complicated term" $
      let ?ctx = Ctx.empty in
      let pat = Cons "Cons" [Var "x", Var "xs"]
          t = convertToList [numerical] ?ctx
          tenv = termEnv [("x", numerical), ("xs", term (List Numerical))]
      in seval' 0 (Match pat `Seq` Build pat) tenv t `shouldBe` successOrFail () (tenv, t)

    it "should throw away the current subject term if needed" $
      let ?ctx = Ctx.empty in
      let tenv = termEnv [("x", numerical)]
      in seval' 0 (Build (Var "x")) tenv lexical `shouldBe` success (tenv, numerical)

    it "should lookup variables" $
      let ?ctx = Ctx.empty in
      let tenv = termEnv [("x", numerical)]
      in seval' 0 (Build (Var "x")) tenv bottom `shouldBe` success (tenv, numerical)

    it "should merge two variables into one term" $
      let ?ctx = Ctx.empty in
      let tenv = termEnv [("x", numerical), ("y", term (List Lexical))]
          t = bottom
      in seval' 0 (Build (Cons "Cons" [Var "x", Var "y"])) tenv t `shouldBe` success (tenv, term (List Top))

    it "should properly construct a list of the same type" $
      let ?ctx = Ctx.empty in
      let tenv = termEnv [("x", numerical), ("y", term (List Numerical))]
          t = bottom
      in seval' 0 (Build (Cons "Cons" [Var "x", Var "y"])) tenv t `shouldBe` success (tenv, term (List Numerical))

    it "should merge a variable and the given subject term" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp") ,("Zero",[],"Exp") ,("Ifz",["Exp","Exp","Exp"],"Exp")] in
      let t = bottom
          tenv = termEnv [("x", term "Exp")]
      in seval' 0 (Build (Cons "Ifz" [Var "x", Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) tenv t `shouldBe`
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
    --          $ sound'' (Match matchPattern `Seq` Build buildPattern) [(t2,[]),(t3,[])] buildPattern

  describe "Scope" $ do
    it "should hide declared variables" $
      let ?ctx = Ctx.empty in
      let tenv = termEnv [("x", numerical)]
      in do
         seval' 0 (Scope ["x"] (Build "x")) tenv numerical `shouldBe` failure "unbound term variable x in build statement !x"
         seval' 0 (Scope ["x"] (Match "x")) tenv numerical `shouldBe` success (tenv, numerical)

    it "should make non-declared variables available" $
      let ?ctx = Ctx.empty in
      let tenv = termEnv [ ("x",numerical) ]
      in do
         seval' 0 (Scope ["y"] (Build "x")) tenv numerical `shouldBe`
           success (S.delete' ["y"] $ termEnv [("x", numerical)], numerical)
         seval' 0 (Scope ["y"] (Match "z")) (S.delete' ["z"] tenv) numerical `shouldBe`
           success (S.delete' ["y"] $ termEnv [ ("x",numerical), ("z",numerical)], numerical)

    it "should hide variables bound in a choice's test from the else branch" $
      let ?ctx = Ctx.fromList [("Zero",[],"Exp"), ("One",[],"Exp")] in
      let exp = term "Exp"
          tenv = S.delete "x" emptyEnv
          prog = (Build (T.Cons "Zero" []) `Seq` Match "x" `Seq` T.Fail)
                `leftChoice`
                (Match "x")
      in seval' 0 prog tenv exp `shouldBe` success (termEnv [("x", exp)], exp)

  describe "Let" $ do
    it "should apply a single function call" $
      let ?ctx = Ctx.empty in
      let t = term (Tuple ["Exp","Exp"])
          tenv = termEnv [("x",t)]
      in seval' 2 (Let [("swap", swap)] (Scope ["x"] (Match "x" `Seq` Call "swap" [] ["x"]))) tenv t
           `shouldBe` success (tenv, t)

    it "should support recursion" $
      let ?ctx = Ctx.empty in
      let t = convertToList [numerical, numerical, numerical] ?ctx
          tenv = termEnv []
      in seval 2 (Let [("map", map')] (Scope ["x"] (Match "x" `Seq` Call "map" [Build (NumberLiteral 1)] ["x"]))) t
        `shouldBe` success (tenv, term (List Numerical))

  describe "Call" $ do
    it "should apply a single function call" $
      let ?ctx = Ctx.empty in
      let senv = M.singleton "swap" (Closure swap M.empty)
          t = term (Tuple ["Exp","Exp"])
          tenv = termEnv []
      in seval'' 1 10 (Scope ["x"] (Match "x" `Seq` Call "swap" [] [])) senv emptyEnv t `shouldBe` success (delete swap tenv, t)

    it "should support an empty list in recursive applications" $
      let ?ctx = Ctx.empty in
      let senv = M.singleton "map" (Closure map' M.empty)
          t = convertToList [] ?ctx
          tenv = termEnv []
      in seval'' 2 10 (Scope ["x"] (Match "x" `Seq` Call "map" [Build (NumberLiteral 1)] ["x"])) senv emptyEnv t `shouldBe`
           success (delete map' tenv, term (List Numerical))

    it "should support a singleton list in recursive applications" $
      let ?ctx = Ctx.empty in
      let senv = M.singleton "map" (Closure map' M.empty)
          t = convertToList [numerical] ?ctx
          tenv = termEnv []
      in seval'' 2 10 (Scope ["x"] (Match "x" `Seq` Call "map" [Build (NumberLiteral 1)] ["x"])) senv emptyEnv t `shouldBe`
           success (delete map' tenv, term (List Numerical))

    it "should support recursion on a list of numbers" $
      let ?ctx = Ctx.empty in
      let senv = M.singleton "map" (Closure map' M.empty)
          c = Ctx.empty
          t = convertToList [numerical, numerical, numerical] c
          tenv = termEnv []
      in seval'' 2 10 (Scope ["x"] (Match "x" `Seq` Call "map" [Build (NumberLiteral 1)] ["x"])) senv emptyEnv t `shouldBe`
           success (delete map' tenv, term (List Numerical))

    it "should terminate and not produce infinite sorts" $ do
      let senv = M.fromList [("map",Closure map' M.empty),
                             ("foo",Closure (Strategy [] [] (Scope ["x"] (Match "x" `Seq` Call "map" ["foo"] ["x"]))) M.empty)]
          c = Ctx.empty
          t = Term Top c
          tenv = termEnv []
      seval'' 10 3 (Call "foo" [] []) senv emptyEnv t `shouldBe`
        success (delete map' tenv, Term (List (List (List Top))) c)

  describe "simplify arithmetics" $ do
    it "reduce Add(Zero,y)" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")] in
      let exp = term "Exp"
          reduce = Match (Cons "Add" [Cons "Zero" [], "y"]) `Seq` Build "y"
      in seval' 0 (reduce) (S.delete "y" emptyEnv) exp
           `shouldBe` successOrFail () (termEnv [("y", exp)], exp)

    it "reduce Add(x,Zero)" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")] in
      let exp = term "Exp"
          reduce = Match (Cons "Add" ["x", Cons "Zero" []]) `Seq` Build "x"
      in seval' 0 reduce (S.delete "x" emptyEnv) exp
           `shouldBe` successOrFail () (termEnv [("x", exp)], exp)

    it "reduce Add(Zero,y) < id + Add(x,Zero)" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")] in
      let exp = term "Exp"
          reduce1 = Match (Cons "Add" [Cons "Zero" [], "y"]) `Seq` Build "y"
          reduce2 = Match (Cons "Add" ["x", Cons "Zero" []]) `Seq` Build "x"
      in seval' 0 (reduce1 `leftChoice` reduce2) (S.delete' ["x", "y"] emptyEnv) exp
           `shouldBe` successOrFail () (termEnv' [may "x" exp, may "y" exp], exp)

    it "reduce Add(x,y); !x; ?Zero()" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")] in
      let exp = term "Exp"
          reduce = Match (Cons "Add" ["x", "y"]) `Seq` Build "x" `Seq` Match (Cons "Zero" []) `Seq` Build "y"
      in seval' 0 reduce (S.delete' ["x", "y"] emptyEnv) exp `shouldBe` successOrFail () (termEnv [("x", exp),("y", exp)], exp)

    it "reduce Double(x)" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp"),("Double",["Exp"],"Exp")] in
      let exp = term "Exp"
          reduce = Match (Cons "Double" ["x"]) `Seq` Build (Cons "Add" ["x", "x"])
      in seval' 0 reduce (S.delete' ["x"] emptyEnv) exp `shouldBe` successOrFail () (termEnv [("x", exp)], exp)

    it "reduce Add(Zero,y) <+ Add(x,Zero) <+ Double(x)" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp"),("Add",["Exp","Exp"],"Exp")] in
      let exp = term "Exp"
          reduce1 = Match (Cons "Add" [Cons "Zero" [], "y"]) `Seq` Build "y"
          reduce2 = Match (Cons "Add" ["x", Cons "Zero" []]) `Seq` Build "x"
          reduce3 = Match (Cons "Double" ["x"]) `Seq` Build (Cons "Add" ["x", "x"])
      in seval' 0 (reduce1 `leftChoice` reduce2 `leftChoice` reduce3) (S.delete' ["x","y"] emptyEnv) exp
           `shouldBe` successOrFail () (termEnv' [may "x" exp, may "y" exp], exp)

    -- prop "should be sound" $ do
    --   i <- choose (0,10)
    --   j <- choose (0,10)
    --   l <- C.similarTerms i 7 2 10
    --   let (l1,l2) = splitAt j l
    --   let t1 = C.convertToList l1
    --   let t2 = C.convertToList l2
    --   return $ counterexample (printf "t: %s\n" (showLub t1 t2))
    --          $ sound' (Let [("map", map')] (Match "x" `Seq` Call "map" [Build 1] ["x"])) [(t1,[]),(t2,[])]

  describe "PCF interpreter in Stratego" $
    before CaseStudy.pcf $ do
      it "lookup: String * Env -> Val" $ \pcf ->
        let ?ctx = signature pcf in
        let senv = stratEnv pcf
            prog = term (Tuple [Lexical, List (Tuple [Lexical, "Val"])])
            val  = term "Val"
        in do
          seval'' 2 10 (Call "lookup_0_0" [] []) senv emptyEnv prog `shouldBe`
            successOrFail () (delete (senv M.! "lookup_0_0") emptyEnv, val)

      it "eval: Env * Exp -> Val" $ \pcf ->
        let ?ctx = signature pcf in
        let senv = stratEnv pcf
            prog = term (Tuple [List (Tuple [Lexical, "Val"]), "Exp"])
            val  = term "Val"
        in seval'' 10 10 (Call "eval_0_0" [] []) senv emptyEnv prog `shouldBe`
             successOrFail () (emptyEnv, val)


  describe "Arrow desugaring in Stratego" $
    before CaseStudy.arrows $ do
      it "tuple-pat': List Var -> APat" $ \desugar ->
        let ?ctx = signature desugar in
        let senv = stratEnv desugar
            prog = term $ List "Var"
            val  = term "APat"
            env = termEnv []
        in do
          seval'' 2 10 (Call "tuple_pat_0_0" [] []) senv env prog `shouldBe`
            successOrFail () (env, val)

      -- it "desugar-arrow': ArrCommand -> Exp" $ \desugar ->
      --   let ?ctx = signature desugar in
      --   let senv = stratEnv desugar
      --       prog = term "ArrCommand"
      --       val  = term "APat"
      --       env = termEnv [("vars-list", term $ List "Var")]
      --   in do
      --     print (Ctx.lookupCons ?ctx "Constr")
      --     seval'' 2 10 (Call "desugar_arrow_p__0_1" [] [TermVar "vars-list"]) senv env prog `shouldBe`
      --       successOrFail () (env, val)

      -- it "eval: Env * Exp -> Val" $ \desugar ->
      --   let ?ctx = signature desugar in
      --   let senv = stratEnv desugar
      --       prog = term (Tuple [List (Tuple [Lexical, "Val"]), "Exp"])
      --       val  = term "Val"
      --   in seval'' 5 10 (Call "eval_0_0" [] []) senv emptyEnv prog `shouldBe`
      --        successOrFail () (emptyEnv, val)


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
    termEnv :: [(TermVar, Term)] -> TermEnv
    termEnv = S.fromList

    termEnv' = S.fromThereList
    -- termEnv' :: [(TermVar, A.Maybe Term)] -> TermEnv
    -- termEnv' = S.fromList'

    delete :: TermVars s => s -> TermEnv -> TermEnv
    delete s = S.delete' (termVars s :: Set TermVar)

    emptyEnv :: TermEnv
    emptyEnv = S.empty

    -- showLub :: C.Term -> C.Term -> String
    -- showLub t1 t2 = show (alpha (C.fromFoldable [t1,t2] :: C.Pow C.Term) :: Term)

    swap = Strategy [] [] (Scope ["x","y"] (Match (Cons "" ["x","y"]) `Seq` Build (Cons "" ["y","x"])))

    map' = Strategy ["f"] ["l"] (Scope ["x","xs","x'","xs'"] (
            Build "l" `Seq`
            GuardedChoice
              (Match (Cons "Cons" ["x","xs"]))
              (Build "x" `Seq`
               Call "f" [] [] `Seq`
               Match "x'" `Seq`
               Call "map" ["f"] ["xs"] `Seq`
               Match "xs'" `Seq`
               Build (Cons "Cons" ["x'", "xs'"]))
              (Build (Cons "Nil" []))))

    seval :: Int -> Strat -> Term -> Error TypeError (Except () (TermEnv,Term))
    seval i s = seval'' i 10 s M.empty emptyEnv

    seval' :: Int -> Strat -> TermEnv -> Term -> Error TypeError (Except () (TermEnv,Term))
    seval' i s = seval'' i 10 s M.empty

    seval'' :: Int -> Int -> Strat -> StratEnv -> TermEnv -> Term -> Error TypeError (Except () (TermEnv,Term))
    seval'' i j s senv tenv t = fromCompletion (error "top element")
                               (fromTerminating (error "sort semantics does not terminate")
                                (eval i j s senv (context t) tenv t))

    term :: (?ctx :: Context) => Sort -> Term
    term s = Term s ?ctx

    bottom :: (?ctx :: Context) => Term
    bottom = term Bottom

    lexical :: (?ctx :: Context) => Term
    lexical = term Lexical

    numerical :: (?ctx :: Context) => Term
    numerical = term Numerical

    top :: (?ctx :: Context) => Term
    top = term Top

    may :: k -> v -> (k,(There,v))
    may k v = (k,(May,v))

    must :: k -> v -> (k, A.Maybe v)
    must k v = (k, A.Just v)

    notThere :: k -> (k, A.Maybe v) 
    notThere k = (k,A.Nothing)

    success :: a -> Error e (Except () a)
    success a = F.Success $ E.Success a
    
    successOrFail :: () -> a -> Error e (Except () a)
    successOrFail () a = F.Success $ E.SuccessOrFail () a
    
    uncaught :: () -> Error e (Except () a)
    uncaught = F.Success . E.Fail
    
    failure :: String -> Error TypeError (Except () a)
    failure = F.Fail . fromString

