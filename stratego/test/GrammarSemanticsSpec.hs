{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module GrammarSemanticsSpec(main, spec) where

import           GrammarSemantics
import           Syntax hiding (Fail)

import           Data.ATerm
import           Data.Abstract.UncertainResult
import qualified Data.HashMap.Lazy as LM
import qualified Data.Map as M
import qualified Data.Text.IO as TIO

import           TreeAutomata

import           Paths_sturdy_stratego

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Construction" $ do
    it "should correctly construct an RTG from a Stratego signature" $ do
      file <- TIO.readFile =<< getDataFileName "case-studies/pcf/pcf.aterm"
      case parseModule =<< parseATerm file of
        Left e -> fail (show e)
        Right m -> createGrammar (signature m) `shouldBe` pcf

  describe "Match" $ do
    it "should match an identical builtin string literal" $
      geval 1 (Match (StringLiteral "x")) (termEnv []) (stringGrammar "x") `shouldBe`
        Success (termEnv [], stringGrammar "x")

    it "should not match another builtin string literal" $
      geval 1 (Match (StringLiteral "y")) (termEnv []) (stringGrammar "x") `shouldBe`
        Fail

    it "should match an equal builtin number literal" $
      geval 1 (Match (NumberLiteral 42)) (termEnv []) (numberGrammar 42) `shouldBe`
        Success (termEnv [], numberGrammar 42)

    it "should not match another builtin number literal" $
      geval 1 (Match (NumberLiteral 1)) (termEnv []) (numberGrammar 42) `shouldBe`
        Fail

    it "a string grammar should not match a number literal" $
      geval 1 (Match (NumberLiteral 1)) (termEnv []) (stringGrammar "x") `shouldBe`
        Fail

    it "a number grammar should not match a string literal" $
      geval 1 (Match (StringLiteral "x")) (termEnv []) (numberGrammar 42) `shouldBe`
        Fail

    it "should match a PCF expression" $
      geval 1 (Match (Cons "Zero" [])) (termEnv []) (Term pcf) `shouldBe`
        Success (termEnv [], Term (singleton (Constr "Zero")))

    it "should match a nested PCF expression" $
      geval 1 (Match (Cons "Succ" [Cons "Zero" []])) (termEnv []) (Term pcf) `shouldBe`
        Success (termEnv [], Term (grammar "S0" (M.fromList [("S0", [ Ctor (Constr "Succ") ["S1"] ])
                                                            ,("S1", [ Ctor (Constr "Zero") [] ])])))

    it "should match a constructor with more than one argument" $
      geval 1 (Match (Cons "Ifz" [Cons "Zero" [], Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) (termEnv []) (Term pcf) `shouldBe`
        Success (termEnv [], Term (grammar "S0" (M.fromList [("S0", [ Ctor (Constr "Ifz") ["S1","S2","S3"]])
                                                            ,("S1", [ Ctor (Constr "Zero") []])
                                                            ,("S2", [ Ctor (Constr "Succ") ["S3"]])
                                                            ,("S3", [ Ctor (Constr "Zero") []])
                                                            ,("S4", [ Ctor (Constr "Zero") []])])))

    it "should introduce one variable" $
      let g = grammar "S0" (M.fromList [("S0", [ Ctor (Constr "Succ") [ "S1" ] ])
                                       ,("S1", [ Ctor (Constr "Zero") [] ])])
          g' = grammar "S0" (M.fromList [("S0", [ Ctor (Constr "Zero") [] ])])
      in geval 1 (Match (Cons "Succ" ["x"])) (termEnv []) (Term g) `shouldBe`
        Success (termEnv [("x", Term g')], Term g)

    it "should introduce multiple variables and support linear pattern matching" $ do
      let g = grammar "S0" (M.fromList [("S0", [ Ctor (Constr "Succ") [ "S1" ] ])
                                       ,("S1", [ Ctor (Constr "Succ") [ "S2" ] ])
                                       ,("S2", [ Ctor (Constr "Zero") [] ])])
          g' = grammar "S0" (M.fromList [("S0", [ Ctor (Constr "Succ") [ "S1" ] ])
                                        ,("S1", [ Ctor (Constr "Zero") [] ])])
      geval 2 (Match (Cons "Succ" ["x"]) `Seq` Match (Cons "Succ" ["y"])) (termEnv []) (Term g)
        `shouldBe` Success (termEnv [("x", Term g'), ("y", Term g')], Term g)

    it "should support linear pattern matching" $
      geval 2 (Match (Cons "Succ" ["x"]) `Seq` Match (Cons "Var" ["x"])) (termEnv []) (Term  pcf) `shouldBe`
        Fail

    it "should succeed when exploding literals" $
    --   let tenv = termEnv []; tenv' = termEnv [("x", Cons "Nil" [])]
    --   in eval (Match (T.Explode "_" "x")) M.empty tenv 1 `shouldBe`
    --        Right (tenv', 1)
      pendingWith "Explosion is not yet implemented"

    it "should handle inconsistent environments" $
    --   let t1 = C.Cons "f" []
    --       t2 = C.Cons "g" []
    --   sound' (Match "x") [(t1, [("x", t1)]), (t2, [("y", t2)])]
      pendingWith "Soundness will come later"

    it "should be sound" $
    --   [t1,t2,t3] <- C.similarTerms 3 7 2 10
    --   matchPattern <- C.similarTermPattern t1 3
    --   return $ counterexample
    --              (printf "pattern: %s\n %s ⊔ %s = %s"
    --                 (show matchPattern) (show t2) (show t3)
    --                 (showLub t2 t3))
    --          $ sound' (Match matchPattern) [(t2,[]),(t3,[])]
      pendingWith "Soundness will come later"

  describe "Build" $ do

    it "should build a builtin string literal" $
      geval 1 (Build (StringLiteral "foo")) (termEnv []) (Term empty) `shouldBe`
        Success (termEnv [], stringGrammar "foo")

    it "should build a builtin number literal" $
      geval 1 (Build (NumberLiteral 1)) (termEnv []) (Term empty) `shouldBe`
        Success (termEnv [], numberGrammar 1)

    it "a string grammar should not be build on a number literal" $
      geval 1 (Build (NumberLiteral 1)) (termEnv []) (Term empty) `shouldNotBe`
        Success (termEnv [], stringGrammar "x")

    it "a number grammar should not be build on a string literal" $
      geval 1 (Match (StringLiteral "x")) (termEnv []) (Term empty) `shouldNotBe`
        Success (termEnv [], numberGrammar 42)

    it "should build a simple constant PCF expression" $
      let zero = grammar "Start0" $ M.fromList [("Start0", [Ctor (Constr "Zero") []])]
      in geval 1 (Build (Cons "Zero" [])) (termEnv []) (Term empty) `shouldBe`
       Success (termEnv [], Term zero)

    it "should build a nested PCF expression" $
      let g = grammar "Start4" $ M.fromList [("Start4", [ Ctor (Constr "Succ") ["Start5"]])
                                            ,("Start5", [ Ctor (Constr "Zero") []])]
      in geval 1 (Build (Cons "Succ" [Cons "Zero" []])) (termEnv []) (Term empty) `shouldBe`
        Success (termEnv [], Term g)

    it "should build a constructor with more than one argument" $
      let g = grammar "Start" $ M.fromList [("Start", [ Ctor (Constr "Ifz") ["Start1", "Start2", "Start1"]])
                                           ,("Start1", [ Ctor (Constr "Zero") [] ])
                                           ,("Start2", [ Ctor (Constr "Succ") ["Start1"]])]
      in geval 1 (Build (Cons "Ifz" [Cons "Zero" [], Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) (termEnv []) (Term empty) `shouldBe`
        Success (termEnv [], Term g)

    it "build should be inverse to match" $
      let term = NumberLiteral 1
      in geval 2 (Match term `Seq` Build term) (termEnv []) (numberGrammar 1) `shouldBe`
         Success (termEnv [], numberGrammar 1)

    it "should throw away the current subject grammar if needed" $
      let tenv = termEnv [("x", numberGrammar 42)]
      in geval 1 (Build (Var "x")) tenv (stringGrammar "x") `shouldBe`
         Success (tenv, numberGrammar 42)

    it "should lookup variables" $
      let tenv = termEnv [("x", Term pcf)]
      in geval 1 (Build (Var "x")) tenv (Term empty) `shouldBe` Success (tenv, Term pcf)

    it "should merge two variables into one grammar" $
      let tenv = termEnv [("x", numberGrammar 42), ("y", stringGrammar "x")]
      in geval 1 (Build (Cons "foo" [Var "x", Var "y"])) tenv (Term empty) `shouldBe`
         Success (tenv, Term $ grammar "Start" $ M.fromList [("Start", [ Ctor (Constr "foo") ["Start1", "Start2" ]])
                                                            ,("Start1", [ Ctor (NumLit 42) [] ])
                                                            ,("Start2", [ Ctor (StringLit "x") [] ])])

    it "should support linear pattern matching" $
      let tenv = termEnv [("x", numberGrammar 42)]
      in geval 1 (Build (Cons "foo" [Var "x", Var "x"])) tenv (Term empty) `shouldBe`
         Success (tenv, Term $ grammar "Start" $ M.fromList [("Start", [ Ctor (Constr "foo") ["Start1", "Start1" ]])
                                                            ,("Start1", [ Ctor (NumLit 42) [] ])
                                                            ,("Start2", [ Ctor (NumLit 42) [] ])])

    it "should merge a variable and the given subject grammar" $
      let tenv = termEnv [("x", numberGrammar 42)]
      in geval 1 (Build (Cons "Ifz" [Var "x", Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) tenv (Term pcf) `shouldBe`
         Success (tenv, Term $ grammar "Start" $ M.fromList [("Start", [ Ctor (Constr "Ifz") ["Start1", "Start2", "Start3"]])
                                                            ,("Start2", [ Ctor (Constr "Succ") ["Start1"]])
                                                            ,("Start3", [ Ctor (Constr "Zero") []])])

    it "should be sound" $ do
      -- [t1,t2,t3] <- C.similarTerms 3 7 2 10
      -- matchPattern <- C.similarTermPattern t1 3
      -- let vars = patternVars' matchPattern
      -- buildPattern <- arbitraryTermPattern 5 2 $
      --   if not (null vars) then elements vars else arbitrary
      -- return $ counterexample
      --            (printf "match pattern: %s\nbuild pattern: %s\nt2: %s\nt3: %s\nlub t2 t3 = %s"
      --               (show matchPattern) (show buildPattern) (show t2) (show t3)
      --               (showLub t2 t3))
      --        $ sound' (Match matchPattern `Seq` Build buildPattern) [(t2,[]),(t3,[])]
      pendingWith "Soundness will come later"

  describe "Scope" $ do
    it "should hide declare variables" $ do
      let tenv = termEnv [("x", numberGrammar 42)]
          Term n = numberGrammar 42
      geval 1 (Scope ["x"] (Build "x")) tenv (numberGrammar 42) `shouldBe`
        Success (tenv, Term (wildcard (alphabet (generate n 0))))
      geval 2 (Scope ["x"] (Match "x")) tenv (numberGrammar 42) `shouldBe`
        Success (tenv, numberGrammar 42)

    it "should make non-declared variables available" $ do
      let tenv = termEnv [("x", numberGrammar 42)]
      geval 2 (Scope ["y"] (Build "x")) tenv (numberGrammar 42) `shouldBe`
        Success (tenv, numberGrammar 42)
      geval 2 (Scope ["y"] (Match "z")) tenv (numberGrammar 42) `shouldBe`
        Success (termEnv [("x", numberGrammar 42), ("z", numberGrammar 42)], numberGrammar 42)

  where
    geval :: Int -> Strat -> TermEnv -> Term -> UncertainResult (TermEnv, Term)
    geval i strat tenv (Term g) = eval i strat (alphabet (generate g 0)) LM.empty tenv (Term g)

    termEnv = TermEnv . LM.fromList

    pcf = grammar "Start" $ M.fromList [
      ("Start", [ Eps "Exp"
                , Eps "Type" ])
      , ("Exp", [ Ctor (Constr "Abs") ["String", "Type", "Exp"]
                , Ctor (Constr "App") ["Exp", "Exp"]
                , Ctor (Constr "Ifz") ["Exp", "Exp", "Exp"]
                , Ctor (Constr "Pred") ["Exp"]
                , Ctor (Constr "Succ") ["Exp"]
                , Ctor (Constr "Var") ["String"]
                , Ctor (Constr "Zero") [] ])
      , ("Type", [ Ctor (Constr "Fun") ["Type", "Type"]
                 , Ctor (Constr "Num") [] ])
      , ("String", [ Ctor (Constr "String") [] ])
      ]
