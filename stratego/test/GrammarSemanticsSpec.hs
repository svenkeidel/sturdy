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
    it "should match a builtin string literal" $
      geval 1 (Match (StringLiteral "x")) (termEnv []) stringGrammar `shouldBe`
        Success (termEnv [], stringGrammar)

    it "should match a builtin number literal" $
      geval 1 (Match (NumberLiteral 1)) (termEnv []) numberGrammar `shouldBe`
        Success (termEnv [], numberGrammar)

    it "a string grammar should not match a number literal" $
      geval 1 (Match (NumberLiteral 1)) (termEnv []) stringGrammar `shouldBe`
        Fail

    it "a number grammar should not match a string literal" $
      geval 1 (Match (StringLiteral "x")) (termEnv []) numberGrammar `shouldBe`
        Fail

    it "should match a PCF expression" $
      geval 1 (Match (Cons "Zero" [])) (termEnv []) pcf `shouldBe`
        Success (termEnv [], pcf)

    it "should match a nested PCF expression" $
      geval 1 (Match (Cons "Succ" [Cons "Zero" []])) (termEnv []) pcf `shouldBe`
        Success (termEnv [], pcf)

    it "should match a constructor with more than one argument" $
      geval 1 (Match (Cons "Ifz" [Cons "Zero" [], Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) (termEnv []) pcf `shouldBe`
        Success (termEnv [], pcf)

    it "should introduce one variable" $
      let ps = productions pcf
          pcf_exp = grammar "Exp" ps
      in geval 1 (Match (Cons "Succ" ["x"])) (termEnv []) pcf `shouldBe`
        Success (termEnv [("x", pcf_exp)], pcf)

    it "should introduce multiple variables and support linear pattern matching" $
      let ps = productions pcf
          pcf_exp = grammar "Exp" ps
          pcf_var = grammar "String" ps
      in geval 2 (Match (Cons "Succ" ["x"]) `Seq` Match (Cons "Var" ["y"])) (termEnv []) pcf `shouldBe`
         Success (termEnv [("x", pcf_exp), ("y", pcf_var)], pcf)

    it "should support linear pattern matching" $
      geval 2 (Match (Cons "Succ" ["x"]) `Seq` Match (Cons "Var" ["x"])) (termEnv []) pcf `shouldBe`
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
      geval 1 (Build (StringLiteral "foo")) (termEnv []) empty `shouldBe`
        Success (termEnv [], stringGrammar)

    it "should build a builtin number literal" $
      geval 1 (Build (NumberLiteral 1)) (termEnv []) empty `shouldBe`
        Success (termEnv [], numberGrammar)

    it "a string grammar should not be build on a number literal" $
      geval 1 (Build (NumberLiteral 1)) (termEnv []) empty `shouldNotBe`
        Success (termEnv [], stringGrammar)

    it "a number grammar should not be build on a string literal" $
      geval 1 (Match (StringLiteral "x")) (termEnv []) empty `shouldNotBe`
        Success (termEnv [], numberGrammar)

    it "should build a simple constant PCF expression" $
      let zero = grammar "Start0" $ M.fromList [("Start0", [Ctor "Zero" []])]
      in geval 1 (Build (Cons "Zero" [])) (termEnv []) empty `shouldBe`
       Success (termEnv [], zero)

    it "should build a nested PCF expression" $
      let g = grammar "Start4" $ M.fromList [("Start4", [ Ctor "Succ" ["Start5"]])
                                            ,("Start5", [ Ctor "Zero" []])]
      in geval 1 (Build (Cons "Succ" [Cons "Zero" []])) (termEnv []) empty `shouldBe`
        Success (termEnv [], g)

    it "should build a constructor with more than one argument" $
      let g = grammar "Start" $ M.fromList [("Start", [ Ctor "Ifz" ["Start1", "Start2", "Start1"]])
                                           ,("Start1", [ Ctor "Zero" [] ])
                                           ,("Start2", [ Ctor "Succ" ["Start1"]])]
      in geval 1 (Build (Cons "Ifz" [Cons "Zero" [], Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) (termEnv []) empty `shouldBe`
        Success (termEnv [], g)

    it "build should be inverse to match" $
      let term = NumberLiteral 1
      in geval 2 (Match term `Seq` Build term) (termEnv []) numberGrammar `shouldBe`
         Success (termEnv [], numberGrammar)

    it "should throw away the current subject grammar if needed" $
      let tenv = termEnv [("x", numberGrammar)]
      in geval 1 (Build (Var "x")) tenv stringGrammar `shouldBe`
         Success (tenv, numberGrammar)

    it "should lookup variables" $
      let tenv = termEnv [("x", pcf)]
      in geval 1 (Build (Var "x")) tenv empty `shouldBe` Success (tenv, pcf)

    it "should merge two variables into one grammar" $
      let tenv = termEnv [("x", numberGrammar), ("y", stringGrammar)]
      in geval 1 (Build (Cons "foo" [Var "x", Var "y"])) tenv empty `shouldBe`
         Success (tenv, grammar "Start" $ M.fromList [("Start", [ Ctor "foo" ["Start1", "Start2" ]])
                                                     ,("Start1", [ Ctor "INT" []])
                                                     ,("Start2", [ Ctor "String" []])])

    it "should support linear pattern matching" $
      let tenv = termEnv [("x", numberGrammar)]
      in geval 1 (Build (Cons "foo" [Var "x", Var "x"])) tenv empty `shouldBe`
         Success (tenv, grammar "Start" $ M.fromList [("Start", [ Ctor "foo" ["Start1", "Start1" ]])
                                                     ,("Start1", [ Ctor "INT" []])])

    it "should merge a variable and the given subject grammar" $
      let tenv = termEnv [("x", numberGrammar)]
      in geval 1 (Build (Cons "Ifz" [Var "x", Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) tenv pcf `shouldBe`
         Success (tenv, grammar "Start" $ M.fromList [("Start", [ Ctor "Ifz" ["Start1", "Start2", "Start3"]])
                                                     ,("Start1", [ Ctor "INT" [] ])
                                                     ,("Start2", [ Ctor "Succ" ["Start1"]])
                                                     ,("Start3", [ Ctor "Zero" []])])

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
      let tenv = termEnv [("x", numberGrammar)]
      geval 1 (Scope ["x"] (Build "x")) tenv numberGrammar `shouldBe`
        Success (tenv, wildcard (alphabet numberGrammar))
      geval 2 (Scope ["x"] (Match "x")) tenv numberGrammar `shouldBe`
        Success (tenv, numberGrammar)

    it "should make non-declared variables available" $ do
      let tenv = termEnv [("x", numberGrammar)]
      geval 2 (Scope ["y"] (Build "x")) tenv numberGrammar `shouldBe`
        Success (tenv, numberGrammar)
      geval 2 (Scope ["y"] (Match "z")) tenv numberGrammar `shouldBe`
        Success (termEnv [("x", numberGrammar), ("z", numberGrammar)], numberGrammar)

  where
    geval :: Int -> Strat -> TermEnv -> Term -> UncertainResult (TermEnv, Term)
    geval i strat tenv g = eval i strat (alphabet g) LM.empty tenv g

    termEnv = TermEnv . LM.fromList

    pcf = grammar "Start" $ M.fromList [
      ("Start", [ Eps "Exp"
                , Eps "Type" ])
      , ("Exp", [ Ctor "Abs" ["String", "Type", "Exp"]
                , Ctor "App" ["Exp", "Exp"]
                , Ctor "Ifz" ["Exp", "Exp", "Exp"]
                , Ctor "Pred" ["Exp"]
                , Ctor "Succ" ["Exp"]
                , Ctor "Var" ["String"]
                , Ctor "Zero" [] ])
      , ("Type", [ Ctor "Fun" ["Type", "Type"]
                 , Ctor "Num" [] ])
      , ("String", [ Ctor "String" []])
      , ("INT", [ Ctor "INT" []])
      ]
