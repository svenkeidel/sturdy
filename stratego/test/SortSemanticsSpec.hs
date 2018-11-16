{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
module SortSemanticsSpec(main, spec) where

import qualified ConcreteSemantics as C
import           SharedSemantics hiding (cons)
import           Soundness
import           Sort (SortId(..))
import           Syntax hiding (Fail)
import           SortSemantics hiding (sortContext)
import           SortContext(Context,Sort(..))
import qualified SortContext as Ctx

import           Control.Arrow

import           Data.Abstract.FreeCompletion (fromCompletion)
import           Data.Abstract.HandleError
import qualified Data.Abstract.Maybe as M
import qualified Data.Abstract.PreciseStore as S
import qualified Data.Abstract.StackWidening as SW
import           Data.Abstract.Terminating (fromTerminating)
import qualified Data.Concrete.Powerset as C
import           Data.Constructor
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.GaloisConnection
import qualified Data.Set as Set
import qualified Data.Term as C
    
import           Text.Printf

import           Test.Hspec hiding (context)
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (Success)

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
      seval 0 (Match (StringLiteral "x")) lexical `shouldBe` SuccessOrFail () (emptyEnv, lexical)

    it "should match another builtin string literal" $
      let ?ctx = Ctx.empty in
      seval 0 (Match (StringLiteral "y")) lexical `shouldBe` SuccessOrFail () (emptyEnv, lexical)

    it "should match an equal builtin number literal" $
      let ?ctx = Ctx.empty in
      seval 0 (Match (NumberLiteral 42)) numerical `shouldBe` SuccessOrFail () (emptyEnv, numerical)

    it "should match another builtin number literal" $
      let ?ctx = Ctx.empty in
      seval 0 (Match (NumberLiteral 1)) numerical `shouldBe`  SuccessOrFail () (emptyEnv, numerical)

    it "a string literal should not match a number literal" $
      let ?ctx = Ctx.empty in
      seval 0 (Match (NumberLiteral 1)) lexical `shouldBe` Fail ()

    it "a number literal should not match a string literal" $
      let ?ctx = Ctx.empty in
      seval 0 (Match (StringLiteral "x")) numerical `shouldBe` Fail ()

    it "should match a PCF expression" $
      let ?ctx = Ctx.fromList [("Zero",[],"Exp")] in
      let t = term "Exp"
      in seval 0 (Match (Cons "Zero" [])) t `shouldBe` Success (emptyEnv, t)

    it "should match a nested PCF expression" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")] in
      let t = term "Exp"
      in seval 0 (Match (Cons "Succ" [Cons "Zero" []])) t `shouldBe` SuccessOrFail () (emptyEnv, t)

    it "should match a constructor with more than one argument" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp") ,("Zero",[],"Exp") ,("Ifz",["Exp","Exp","Exp"],"Exp")] in
      let t = term "Exp"
      in seval 0 (Match (Cons "Ifz" [Cons "Zero" [], Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) t `shouldBe`
        SuccessOrFail () (emptyEnv, t)

    it "should introduce one variable" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")] in
      let t = term "Exp"
      in seval 0 (Match (Cons "Succ" ["x"])) t `shouldBe` SuccessOrFail () (termEnv [("x", t)], t)

    it "should introduce one variable 2" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")] in
      let t = term "Exp"
      in seval 0 (Match (Cons "Succ" ["x"])) t `shouldBe` SuccessOrFail () (termEnv [("x", t)], t)

    it "should introduce multiple variables and support linear pattern matching" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")] in
      let t = term "Exp"
      in seval 0 (Match (Cons "Succ" ["x"]) `Seq` Match (Cons "Succ" ["y"])) t `shouldBe`
         SuccessOrFail () (termEnv [("x", t), ("y", t)], t)

    it "should support linear pattern matching" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp")] in
      let t = term "Exp"
      in seval 0 (Match (Cons "Succ" ["x"]) `Seq` Match (Cons "Var" ["x"])) t `shouldBe` Fail ()

    it "should succeed when exploding literals" $
      let ?ctx = Ctx.empty in
      let tenv = termEnv [("x", convertToList [] ?ctx)]
      in seval 0 (Match (Explode "_" "x")) numerical `shouldBe` Success (tenv, numerical)

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
      seval 0 (Build (StringLiteral "foo")) bottom `shouldBe` Success (emptyEnv, lexical)

    it "should build a builtin number literal" $
      let ?ctx = Ctx.empty in
      seval 0 (Build (NumberLiteral 1)) bottom `shouldBe` Success (emptyEnv, numerical)

    it "a string grammar should not be build on a number literal" $
      let ?ctx = Ctx.empty in
      seval 0 (Build (NumberLiteral 1)) bottom `shouldNotBe` Success (emptyEnv, lexical)

    it "a number grammar should not be build on a string literal" $
      let ?ctx = Ctx.empty in
      seval 0 (Match (StringLiteral "x")) bottom `shouldNotBe` Success (emptyEnv, numerical)

    it "should build a simple constant PCF expression" $
      let ?ctx = Ctx.fromList [("Zero",[],"Exp")] in
      let t = bottom
          t' = term "Exp"
      in seval 0 (Build (Cons "Zero" [])) t `shouldBe` Success (emptyEnv, t')

    it "should build a nested PCF expression" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp"),("Zero",[],"Exp")] in
      let t = bottom
          t' = term "Exp"
      in seval 0 (Build (Cons "Succ" [Cons "Zero" []])) t `shouldBe` Success (emptyEnv, t')

    it "should build a constructor with more than one argument" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp") ,("Zero",[],"Exp") ,("Ifz",["Exp","Exp","Exp"],"Exp")] in
      let t = term "Exp"
      in seval 0 (Build (Cons "Ifz" [Cons "Zero" [], Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) t `shouldBe`
        Success (emptyEnv, t)

    it "build should be inverse to match" $
      let ?ctx = Ctx.empty in
      let pat = NumberLiteral 1
      in seval 0 (Match pat `Seq` Build pat) numerical `shouldBe` SuccessOrFail () (emptyEnv, numerical)

    it "build should be inverse to match with a more complicated term" $
      let ?ctx = Ctx.empty in
      let pat = Cons "Cons" [Var "x", Var "xs"]
          t = convertToList [numerical] ?ctx
          tenv = termEnv [("x", numerical), ("xs", term (List Numerical))]
      in seval' 0 (Match pat `Seq` Build pat) tenv t `shouldBe` SuccessOrFail () (tenv, t)

    it "should throw away the current subject term if needed" $
      let ?ctx = Ctx.empty in
      let tenv = termEnv [("x", numerical)]
      in seval' 0 (Build (Var "x")) tenv lexical `shouldBe` Success (tenv, numerical)

    it "should lookup variables" $
      let ?ctx = Ctx.empty in
      let tenv = termEnv [("x", numerical)]
      in seval' 0 (Build (Var "x")) tenv bottom `shouldBe` Success (tenv, numerical)

    it "should merge two variables into one term" $
      let ?ctx = Ctx.empty in
      let tenv = termEnv [("x", numerical), ("y", term (List Lexical))]
          t = bottom
      in seval' 0 (Build (Cons "Cons" [Var "x", Var "y"])) tenv t `shouldBe` Success (tenv, term (List Top))

    it "should properly construct a list of the same type" $
      let ?ctx = Ctx.empty in
      let tenv = termEnv [("x", numerical), ("y", term (List Numerical))]
          t = bottom
      in seval' 0 (Build (Cons "Cons" [Var "x", Var "y"])) tenv t `shouldBe` Success (tenv, term (List Numerical))

    it "should support linear pattern matching" $
      let ?ctx = Ctx.empty in
      let tenv = termEnv [("x", numerical)]
          t = bottom
      in seval' 0 (Build (Cons "Cons" [Var "x", Var "x"])) tenv t `shouldBe` Success (tenv, top)

    it "should merge a variable and the given subject term" $
      let ?ctx = Ctx.fromList [("Succ",["Exp"],"Exp") ,("Zero",[],"Exp") ,("Ifz",["Exp","Exp","Exp"],"Exp")] in
      let t = bottom
          tenv = termEnv [("x", term "Exp")]
      in seval' 0 (Build (Cons "Ifz" [Var "x", Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) tenv t `shouldBe`
        Success (tenv, term "Exp")

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
         seval' 0 (Scope ["x"] (Build "x")) tenv numerical `shouldBe` Fail ()
         seval' 0 (Scope ["x"] (Match "x")) tenv numerical `shouldBe` Success (tenv, numerical)

    it "should make non-declared variables available" $
      let ?ctx = Ctx.empty in
      let tenv = termEnv [("x", numerical)]
      in do
         seval' 0 (Scope ["y"] (Build "x")) tenv numerical `shouldBe` Success (tenv, numerical)
         seval' 0 (Scope ["y"] (Match "z")) tenv numerical `shouldBe`
           Success (termEnv [("x", numerical), ("z", numerical)], numerical)

  describe "Let" $ do
    it "should apply a single function call" $
      let ?ctx = Ctx.empty in
      let t = term "Exp"
          tenv = termEnv [("x",t)]
      in seval 1 (Let [("swap", swap)] (Match "x" `Seq` Call "swap" [] [])) t `shouldBe` Success (tenv, t)

    it "should support recursion" $
      let ?ctx = Ctx.empty in
      let t = convertToList [numerical, numerical, numerical] ?ctx
          tenv = termEnv [("x",t)]
      in seval 1 (Let [("map", map')] (Match "x" `Seq` Call "map" [Build (NumberLiteral 1)] ["x"])) t
        `shouldBe` Success (tenv, term (List Numerical))

  describe "Call" $ do
    it "should apply a single function call" $
      let ?ctx = Ctx.empty in
      let senv = M.singleton "swap" (Closure swap M.empty)
          t = term "Exp"
          tenv = termEnv [("x",t)]
      in seval'' 1 10 (Match "x" `Seq` Call "swap" [] []) senv emptyEnv t `shouldBe` Success (tenv, t)

    it "should support an empty list in recursive applications" $
      let ?ctx = Ctx.empty in
      let senv = M.singleton "map" (Closure map' M.empty)
          t = convertToList [] ?ctx
          tenv = termEnv [("x",t)]
      in seval'' 1 10 (Match "x" `Seq` Call "map" [Build (NumberLiteral 1)] ["x"]) senv emptyEnv t `shouldBe`
           Success (tenv, term (List Numerical))

    it "should support a singleton list in recursive applications" $
      let ?ctx = Ctx.empty in
      let senv = M.singleton "map" (Closure map' M.empty)
          t = convertToList [numerical] ?ctx
          tenv = termEnv [("x",t)]
      in seval'' 1 10 (Match "x" `Seq` Call "map" [Build (NumberLiteral 1)] ["x"]) senv emptyEnv t `shouldBe`
           Success (tenv, term (List Numerical))

    it "should support recursion on a list of numbers" $
      let ?ctx = Ctx.empty in
      let senv = M.singleton "map" (Closure map' M.empty)
          c = Ctx.empty
          t = convertToList [numerical, numerical, numerical] c
          tenv = termEnv [("x",t)]
      in seval'' 1 10 (Match "x" `Seq` Call "map" [Build (NumberLiteral 1)] ["x"]) senv emptyEnv t `shouldBe`
           Success (tenv, term (List Numerical))

    it "should terminate and not produce infinite sorts" $ do
      let senv = M.fromList [("map",Closure map' M.empty),
                             ("foo",Closure (Strategy [] [] (Match "x" `Seq` Call "map" ["foo"] ["x"])) M.empty)]
          c = Ctx.empty
          t = Term Top c
          tenv = termEnv [("x",t)]
      seval'' 1 3 (Call "foo" [] []) senv emptyEnv t `shouldBe`
        Success (tenv, Term (List (List (List Top))) c)

    -- prop "should be sound" $ do
    --   i <- choose (0,10)
    --   j <- choose (0,10)
    --   l <- C.similarTerms i 7 2 10
    --   let (l1,l2) = splitAt j l
    --   let t1 = C.convertToList l1
    --   let t2 = C.convertToList l2
    --   return $ counterexample (printf "t: %s\n" (showLub t1 t2))
    --          $ sound' (Let [("map", map')] (Match "x" `Seq` Call "map" [Build 1] ["x"])) [(t1,[]),(t2,[])]

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

    termEnv' = C.TermEnv . M.fromList
    termEnv :: [(TermVar, Term)] -> TermEnv
    termEnv = S.fromList
    emptyEnv :: TermEnv
    emptyEnv = S.empty

    -- showLub :: C.Term -> C.Term -> String
    -- showLub t1 t2 = show (alpha (C.fromFoldable [t1,t2] :: C.Pow C.Term) :: Term)

    swap = Strategy [] [] (Scope ["x","y"] (Match (Cons "Tuple" ["x","y"]) `Seq` Build (Cons "Tuple" ["y","x"])))

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

    seval :: Int -> Strat -> Term -> Error () (TermEnv,Term)
    seval i s t = seval'' i 10 s M.empty emptyEnv t

    seval' :: Int -> Strat -> TermEnv -> Term -> Error () (TermEnv,Term)
    seval' i s tenv t = seval'' i 10 s M.empty tenv t

    seval'' :: Int -> Int -> Strat -> StratEnv -> TermEnv -> Term -> Error () (TermEnv,Term)
    seval'' i j s senv tenv t = fromCompletion (error "top element")
                               (fromTerminating (error "non-terminating sort semantics")
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
