{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module GrammarSemanticsSpec(main, spec) where

import           GrammarSemantics
import           Syntax hiding (Fail)

import           Data.ATerm
import           Data.Abstract.FreeCompletion
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
      eval 1 (Match (StringLiteral "x")) LM.empty (termEnv []) stringGrammar `shouldBe`
        Success (termEnv [], stringGrammar)

    it "should match a builtin number literal" $
      eval 1 (Match (NumberLiteral 1)) LM.empty (termEnv []) numberGrammar `shouldBe`
        Success (termEnv [], numberGrammar)

    it "a string grammar should not match a number literal" $
      eval 1 (Match (NumberLiteral 1)) LM.empty (termEnv []) stringGrammar `shouldBe`
        Fail

    it "a number grammar should not match a string literal" $
      eval 1 (Match (StringLiteral "x")) LM.empty (termEnv []) numberGrammar `shouldBe`
        Fail

    it "should match a PCF expression" $
      eval 1 (Match (Cons "Zero" [])) LM.empty (termEnv []) (Lower pcf) `shouldBe`
        Success (termEnv [], Lower pcf)

    it "should match a nested PCF expression" $
      eval 1 (Match (Cons "Succ" [Cons "Zero" []])) LM.empty (termEnv []) (Lower pcf) `shouldBe`
        Success (termEnv [], Lower pcf)

    it "should match a constructor with more than one argument" $
      eval 1 (Match (Cons "Ifz" [Cons "Zero" [], Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) LM.empty (termEnv []) (Lower pcf) `shouldBe`
        Success (termEnv [], Lower pcf)

    it "should introduce one variable" $
      let (Grammar _ ps) = pcf
          pcf_exp = Grammar "Exp" ps
      in eval 1 (Match (Cons "Succ" ["x"])) LM.empty (termEnv []) (Lower pcf) `shouldBe`
        Success (termEnv [("x", Lower pcf_exp)], Lower pcf)

    it "should introduce multiple variables and support linear pattern matching" $
      let (Grammar _ ps) = pcf
          pcf_exp = Grammar "Exp" ps
          pcf_var = Grammar "String" ps
      in eval 2 (Match (Cons "Succ" ["x"]) `Seq` Match (Cons "Var" ["y"])) LM.empty (termEnv []) (Lower pcf) `shouldBe`
         Success (termEnv [("x", Lower pcf_exp), ("y", Lower pcf_var)], Lower pcf)

    it "should support linear pattern matching" $
      eval 2 (Match (Cons "Succ" ["x"]) `Seq` Match (Cons "Var" ["x"])) LM.empty (termEnv []) (Lower pcf) `shouldBe`
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
      eval 1 (Build (StringLiteral "foo")) LM.empty (termEnv []) (Lower empty) `shouldBe`
        Success (termEnv [], stringGrammar)

    it "should build a builtin number literal" $
      eval 1 (Build (NumberLiteral 1)) LM.empty (termEnv []) (Lower empty) `shouldBe`
        Success (termEnv [], numberGrammar)

    it "a string grammar should not be build on a number literal" $
      eval 1 (Build (NumberLiteral 1)) LM.empty (termEnv []) (Lower empty) `shouldNotBe`
        Success (termEnv [], stringGrammar)

    it "a number grammar should not be build on a string literal" $
      eval 1 (Match (StringLiteral "x")) LM.empty (termEnv []) (Lower empty) `shouldNotBe`
        Success (termEnv [], numberGrammar)

    it "should build a simple constant PCF expression" $
      let zero = Grammar "Start0" $ M.fromList [("Start0", [Ctor "Zero" []])]
      in eval 1 (Build (Cons "Zero" [])) LM.empty (termEnv []) (Lower empty) `shouldBe`
       Success (termEnv [], Lower zero)

    it "should build a nested PCF expression" $
      let grammar = Grammar "Start4" $ M.fromList [("Start4", [ Ctor "Succ" ["Start5"]])
                                                  ,("Start5", [ Ctor "Zero" []])]
      in eval 1 (Build (Cons "Succ" [Cons "Zero" []])) LM.empty (termEnv []) (Lower empty) `shouldBe`
        Success (termEnv [], Lower grammar)

    it "should build a constructor with more than one argument" $
      let grammar = Grammar "Start" $ M.fromList [("Start", [ Ctor "Ifz" ["Start1", "Start2", "Start1"]])
                                                  ,("Start1", [ Ctor "Zero" [] ])
                                                  ,("Start2", [ Ctor "Succ" ["Start1"]])]
      in eval 1 (Build (Cons "Ifz" [Cons "Zero" [], Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) LM.empty (termEnv []) (Lower empty) `shouldBe`
        Success (termEnv [], Lower grammar)

    it "build should be inverse to match" $
      let term = NumberLiteral 1
      in eval 2 (Match term `Seq` Build term) LM.empty (termEnv []) numberGrammar `shouldBe`
         Success (termEnv [], numberGrammar)

    it "should throw away the current subject grammar if needed" $
      let tenv = termEnv [("x", numberGrammar)]
      in eval 1 (Build (Var "x")) LM.empty tenv stringGrammar `shouldBe`
         Success (tenv, numberGrammar)

    it "should lookup variables" $
      let term = Lower pcf
          tenv = termEnv [("x", Lower pcf)]
      in eval 1 (Build (Var "x")) LM.empty tenv (Lower empty) `shouldBe` Success (tenv,term)

    it "should merge two variables into one grammar" $
      let tenv = termEnv [("x", numberGrammar), ("y", stringGrammar)]
      in eval 1 (Build (Cons "foo" [Var "x", Var "y"])) LM.empty tenv (Lower empty) `shouldBe`
         Success (tenv, Lower (Grammar "Start" $ M.fromList [("Start", [ Ctor "foo" ["Start1", "Start2" ]])
                                                            ,("Start1", [ Ctor "INT" []])
                                                            ,("Start2", [ Ctor "String" []])]))

    it "should support linear pattern matching" $
      let tenv = termEnv [("x", numberGrammar)]
      in eval 1 (Build (Cons "foo" [Var "x", Var "x"])) LM.empty tenv (Lower empty) `shouldBe`
         Success (tenv, Lower (Grammar "Start" $ M.fromList [("Start", [ Ctor "foo" ["Start1", "Start1" ]])
                                                            ,("Start1", [ Ctor "INT" []])]))

    it "should merge a variable and the given subject grammar" $
      let tenv = termEnv [("x", numberGrammar)]
      in eval 1 (Build (Cons "Ifz" [Var "x", Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) LM.empty tenv (Lower pcf) `shouldBe`
         Success (tenv, Lower (Grammar "Start" $ M.fromList [("Start", [ Ctor "Ifz" ["Start1", "Start2", "Start3"]])
                                                  ,("Start1", [ Ctor "INT" [] ])
                                                  ,("Start2", [ Ctor "Succ" ["Start1"]])
                                                  ,("Start3", [ Ctor "Zero" []])]))

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

  where
    termEnv = TermEnv . LM.fromList

    pcf = Grammar "Start" $ M.fromList [
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
