{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module GrammarSemanticsSpec(main, spec) where

import           Syntax hiding (Fail)
import qualified GrammarSemantics as G

import           Data.ATerm
import qualified Data.Map as M
import qualified Data.Text.IO as TIO

import           TreeAutomata

import           Paths_sturdy_stratego

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "The semantics" $ do
    it "should correctly construct an RTG from a Stratego signature" $ do
      let grammar = Grammar "Start" $ M.fromList [
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
            ]
      file <- TIO.readFile =<< getDataFileName "case-studies/pcf/pcf.aterm"
      case parseModule =<< parseATerm file of
        Left e -> fail (show e)
        Right m -> G.createGrammar (signature m) `shouldBe` grammar
