{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module TreeGrammarSpec(main, spec) where

import Control.Monad

import Data.Hashable
import Data.Text(Text)

import Data.Abstract.TreeGrammar
import Data.Abstract.TreeGrammar.Terminal(Constr)
import Data.Abstract.TreeGrammar.NonTerminal(Named)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import GHC.Exts
import Text.Printf

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Inclusion" $ do
    it "should work for non-deterministic grammars" $ do
      let g :: Grammar Named Constr
          g = grammar "S" [ ("S", [ ("f",["A", "B"])
                                  , ("f",["A'", "B'"])])
                          , ("A", [ ("a",[]) ])
                          , ("B", [ ("b",[]) ])
                          , ("A'", [ ("a'",[]) ])
                          , ("B'", [ ("b'",[]) ])
                          ]
                          []
          g' :: Grammar Named Constr
          g' = grammar "S" [ ("S", [("f",["A", "B"])])
                           , ("A", [("a",[]), ("a'",[])])
                           , ("B", [("b",[]), ("b'",[])])
                           ]
                           []
      g `shouldBeSubsetOf` g'
      g' `shouldNotBeSubsetOf` g

    it "should work for recursive grammars" $ do
      pcf_sub `shouldBeSubsetOf` pcf
      pcf `shouldNotBeSubsetOf` pcf_sub

    prop "should be reflexive" $ \(g :: Grammar Named Constr) ->
      g `shouldBeSubsetOf` g

  describe "Union" $ do

    it "should work for non deterministic grammars" $
      union pcf nondet `shouldBe` pcf_nondet

    prop "is idempotent: G ∪ G = G" $ \(g :: Grammar Named Constr) ->
      union g g `shouldBe` g

    prop "is an upper bound: G1 ⊆ G1 ∪ G2" $ \(g1 :: Grammar Named Constr) (g2 :: Grammar Named Constr) -> do
      g1 `shouldBeSubsetOf` union g1 g2
      g2 `shouldBeSubsetOf` union g1 g2

  describe "Intersection" $ do
    prop "should be idempotent: G ∩ G = G" $ \(g :: Grammar Named Constr) ->
      intersection g g `shouldBe` g

    prop "is lower bound: G1 ∩ G2 ⊆ G1" $ \(g1 :: Grammar Named Constr) (g2 :: Grammar Named Constr) -> do
      intersection g1 g2 `shouldBeSubsetOf` g1
      intersection g1 g2 `shouldBeSubsetOf` g2

    prop "is the opposite of union" $ \(g1 :: Grammar Named Constr) (g2 :: Grammar Named Constr) ->
      g1 `union` (g1 `intersection` g2) `shouldBe` g1 `intersection` (g1 `union` g2)

  describe "Grammar Optimizations" $ do
    describe "Epsilon Closure" $ do
      prop "describes the same language: epsilonClosure g = g" $
        \(g :: Grammar Named Constr) -> epsilonClosure g `shouldBe` g

    describe "Dropping unreachable prductions" $ do
      prop "describes the same language: dropUnreachable g = g" $
        \(g :: Grammar Named Constr) -> dropUnreachable g `shouldBe` g

    describe "Dropping unproductive prductions" $ do
      prop "removes infinite terms from the language: dropUnproductive g ⊆ g" $
        \(g :: Grammar Named Constr) -> do
           dropUnproductive g `shouldBeSubsetOf` g

    describe "Determinization" $ do
      prop "removes relations between subterms: g ⊆ determinize g" $ do
        \(g :: Grammar Named Constr) -> do
           g `shouldBeSubsetOf` determinize g

      prop "is idempotent: determinize g = determinize (determinize g)" $ do
        \(g :: Grammar Named Constr) ->
           let g' = determinize g :: Grammar Named Constr
           in g' `shouldBe` determinize g'

  describe "Hashing" $
    prop "unequal hashes imply inequality" $
      \(g1 :: Grammar Named Constr) ->
      \(g2 :: Grammar Named Constr) ->
      hash g1 /= hash g2 ==> g1 `shouldNotBe` g2

  describe "Subterms" $ do
    prop "toSubterms and fromSubterms are inverse" $
      \(g :: Grammar Named Constr) ->
         fromSubterms (toSubterms g) `shouldBe` g

    prop "fromSubterms and toSubterms are inverse" $
      \(g :: Constr (Grammar Named Constr)) ->
         toSubterms (fromSubterms g) `shouldBe` g

  -- describe "Emptiness" $ do
  --   it "should be true on the infinite infinite grammar" $
  --     infinite `shouldSatisfy` isEmpty

  --   it "should be false on the nondeterministic grammar" $
  --     nondet `shouldNotSatisfy` isEmpty

  --   it "should be false on the PCF grammar" $
  --     pcf `shouldNotSatisfy` isEmpty

  --   it "should be false on the subset of PCF" $
  --     pcf_sub `shouldNotSatisfy` isEmpty

  -- describe "Singletoness" $ do
  --   it "should be false on the infinite infinite grammar" $
  --     infinite `shouldNotSatisfy` isSingleton

  --   it "should be false on the nondeterministic grammar" $
  --     nondet `shouldNotSatisfy` isSingleton

  --   it "should be false on the PCF grammar" $
  --     pcf `shouldNotSatisfy` isSingleton

  --   it "should be true on a singleton grammar" $
  --     let g :: Grammar Text
  --         g = grammar "Foo" (M.fromList [ ("Foo", [ Ctor "Bar" [ "Baz" ] ])
  --                                       , ("Baz", [ Ctor "Baz" [] ]) ])
  --     in g `shouldSatisfy` isSingleton

  -- describe "Widening" $ do
  --   it "wideMap should compute the depths and principal label sets of nonterminals in PCF" $ do
  --     let pcf' = evalState pcf 0
  --         pcf_wideMap = wideMap pcf'
  --         pcf_prods = productions pcf'
  --     M.lookup "PStart" pcf_wideMap `shouldBe` Just (0,S.empty,pcf_prods M.! "PStart")
  --     M.lookup "Exp" pcf_wideMap `shouldBe` Just (1,S.fromList ["App","Abs","Zero","Succ","Pred","Ifz"],pcf_prods M.! "Exp")
  --     M.lookup "String" pcf_wideMap `shouldBe` Just (2,S.singleton "String",pcf_prods M.! "String")
  --     M.lookup "Type" pcf_wideMap `shouldBe` Just (1,S.fromList ["Num","Fun"],pcf_prods M.! "Type")

  --   it "wideMap should compute the depths and principal label sets of nonterminals in the nondeterministic grammar" $ do
  --     let nondet'' = evalState nondet' 0
  --         nondet_wideMap = wideMap nondet''
  --         nondet_prods = productions nondet''
  --     M.lookup "S" nondet_wideMap `shouldBe` Just (0,S.empty,nondet_prods M.! "S")
  --     M.lookup "F" nondet_wideMap `shouldBe` Just (1,S.singleton "f",nondet_prods M.! "F")
  --     M.lookup "G" nondet_wideMap `shouldBe` Just (2,S.singleton "g",nondet_prods M.! "G")
  --     M.lookup "H" nondet_wideMap `shouldBe` Just (2,S.empty,nondet_prods M.! "H")
  --     M.lookup "A" nondet_wideMap `shouldBe` Just (3,S.singleton "a",nondet_prods M.! "A")

  --   it "should build a correspondence set on the example from the paper" $ do
  --     let cons0' = evalState (epsilonClosure cons0) 0
  --         cons1' = evalState (epsilonClosure cons1) 0
  --         cons_w0 = wideMap cons0'
  --         cons_w1 = wideMap cons1'
  --     correspondenceSet (start cons0') cons_w0 (start cons1') cons_w1 `shouldBe` M.fromList [(("T0","T3"),S.empty)
  --                                                                                           ,(("T1","T4"),S.singleton ("cons","T3",0))
  --                                                                                           ,(("T2","T5"),S.singleton ("cons","T3",1))]

  --   it "should build a correspondence set on the PCF grammar " $ do
  --     let pcf_sub' = evalState (epsilonClosure pcf_sub) 0
  --         pcf' = evalState (epsilonClosure pcf) 0
  --         pcf_sub_w = wideMap pcf_sub'
  --         pcf_w = wideMap pcf'
  --     correspondenceSet (start pcf_sub') pcf_sub_w (start pcf') pcf_w `shouldBe` M.fromList [(("PSStart","PStart"),S.empty)]

  --   it "should build a correspondence set on the arithmetic example from the paper" $ do
  --     let arith0' = evalState (epsilonClosure arith0) 0
  --         arith1' = evalState (epsilonClosure arith1) 0
  --         arith_w0 = wideMap arith0'
  --         arith_w1 = wideMap arith1'
  --     correspondenceSet (start arith0') arith_w0 (start arith1') arith_w1 `shouldBe` M.fromList [(("T0","Tn"),S.empty)
  --                                                                                               ,(("Tx","T3"),S.fromList [("Add","Tn",0)
  --                                                                                                                        ,("par","T7",0)])
  --                                                                                               ,(("T1","T6"),S.fromList [("Add","Tn",1)
  --                                                                                                                        ,("Mul","T6",0)])
  --                                                                                               ,(("T2","T7"),S.singleton ("Mul","T6",1))]

  --   it "should find a set of widening topological clashes on the example from the paper" $ do
  --     let cons0' = evalState (epsilonClosure cons0) 0
  --         cons1' = evalState (epsilonClosure cons1) 0
  --         cons_w0 = wideMap cons0'
  --         cons_w1 = wideMap cons1'
  --         corr = correspondenceSet (start cons0') cons_w0 (start cons1') cons_w1
  --     wideningClashes corr cons_w0 cons_w1 `shouldBe` M.fromList [(("T2","T5"),S.singleton ("cons","T3",1))]

  --   it "should find a set of widening topological clashes on the PCF grammar" $ do
  --     let pcf_sub' = evalState (epsilonClosure pcf_sub) 0
  --         pcf' = evalState (epsilonClosure pcf) 0
  --         pcf_sub_w = wideMap pcf_sub'
  --         pcf_w = wideMap pcf'
  --         corr = correspondenceSet (start pcf_sub') pcf_sub_w (start pcf') pcf_w
  --     wideningClashes corr pcf_sub_w pcf_w `shouldBe` M.fromList [(("PSStart","PStart"),S.empty)]

  --   it "should find a set of widening topological clashes on the arithmetic example from the paper" $ do
  --     let arith0' = evalState (epsilonClosure arith0) 0
  --         arith1' = evalState (epsilonClosure arith1) 0
  --         arith_w0 = wideMap arith0'
  --         arith_w1 = wideMap arith1'
  --         corr = correspondenceSet (start arith0') arith_w0 (start arith1') arith_w1
  --     wideningClashes corr arith_w0 arith_w1 `shouldBe` M.fromList [(("Tx","T3"),S.fromList [("Add","Tn",0)
  --                                                                                           ,("par","T7",0)])]

  --   it "should find ancestors for the example from the paper" $ do
  --     let cons1' = evalState cons1 0
  --         w = wideMap cons1'
  --     findAncestors "T5" w cons1' `shouldBe` ["T3"]

  --   it "should find ancestors for the arithmetic example from the paper" $ do
  --     let arith1' = evalState arith1 0
  --         w = wideMap arith1'
  --     findAncestors "T3" w arith1' `shouldBe` ["T6","T7","Tn"]

  --   it "should find ancestors for PCF" $ do
  --     let pcf' = evalState pcf 0
  --         w = wideMap pcf'
  --     findAncestors "PStart" w pcf' `shouldBe` []
  --     findAncestors "Exp" w pcf' `shouldBe` ["PStart"]
  --     findAncestors "String" w pcf' `shouldBe` ["Exp","PStart"]
  --     findAncestors "Type" w pcf' `shouldBe` ["Exp","PStart"]

  --   it "should find ancestors for the nondeterministic grammar" $ do
  --     let nondet'' = evalState nondet' 0
  --         w = wideMap nondet''
  --     findAncestors "G" w nondet'' `shouldBe` ["F", "H", "S"]
  --     findAncestors "A" w nondet'' `shouldBe` ["F", "G", "H", "S"]

  --   it "should find the best arc ancestor for the example from the paper" $ do
  --     let cons0' = evalState (epsilonClosure cons0) 0
  --         cons1' = evalState (epsilonClosure cons1) 0
  --         w0 = wideMap cons0'
  --         w1 = wideMap cons1'
  --         ancs = findAncestors "T5" w1 cons1'
  --     arcAncestor "T2" "T5" ancs w0 w1 cons1' `shouldBe` Just "T3"

  --   it "should find the best arc ancestor for the arithmetic example from the paper" $ do
  --     let arith0' = evalState (epsilonClosure arith0) 0
  --         arith1' = evalState (epsilonClosure arith1) 0
  --         w0 = wideMap arith0'
  --         w1 = wideMap arith1'
  --         ancs = findAncestors "T3" w1 arith1'
  --     arcAncestor "Tx" "T3" ancs w0 w1 arith1' `shouldBe` Just "Tn"

  --   it "should find the best arc ancestor for PCF" $ do
  --     let pcf' = evalState (epsilonClosure pcf) 0
  --         w1 = wideMap pcf'
  --     arcAncestor "PSStart" "PStart" (findAncestors "PStart" w1 pcf') w1 w1 pcf' `shouldBe` Nothing
  --     arcAncestor "Exp" "Exp" (findAncestors "Exp" w1 pcf') w1 w1 pcf' `shouldBe` Just "PStart"
  --     arcAncestor "String" "String" (findAncestors "String" w1 pcf') w1 w1 pcf' `shouldBe` Nothing
  --     arcAncestor "Type" "Type" (findAncestors "Type" w1 pcf') w1 w1 pcf' `shouldBe` Just "PStart"

  --   it "should find a set of arc replacements for the widening topological clashes for the example from the paper" $ do
  --     let cons0' = evalState (epsilonClosure cons0) 0
  --         cons1' = evalState (epsilonClosure cons1) 0
  --         w0 = wideMap cons0'
  --         w1 = wideMap cons1'
  --         corr = correspondenceSet (start cons0') w0 (start cons1') w1
  --         wideClashes01 = wideningClashes corr w0 w1
  --     arcReplacements cons1' wideClashes01 w0 w1 `shouldBe` M.fromList [(("T5","T3"),S.singleton ("cons","T3",1))]

  --   it "should find a set of arc replacements for the widening topological clashes for the PCF grammar" $ do
  --     let pcf_sub' = evalState (epsilonClosure pcf_sub) 0
  --         pcf' = evalState (epsilonClosure pcf) 0
  --         w0 = wideMap pcf_sub'
  --         w1 = wideMap pcf'
  --         corr = correspondenceSet (start pcf_sub') w0 (start pcf') w1
  --         wideClashes = wideningClashes corr w0 w1
  --     arcReplacements pcf' wideClashes w0 w1 `shouldBe` M.fromList []

  --   it "should find a set of arc replacements for the widening topological clashes for the arithmetic example from the paper " $ do
  --     let arith0' = evalState (epsilonClosure arith0) 0
  --         arith1' = evalState (epsilonClosure arith1) 0
  --         w0 = wideMap arith0'
  --         w1 = wideMap arith1'
  --         corr = correspondenceSet (start arith0') w0 (start arith1') w1
  --         wideClashes = wideningClashes corr w0 w1
  --     arcReplacements arith1' wideClashes w0 w1 `shouldBe` M.fromList [(("T3","Tn"),S.fromList [("Add","Tn",0)
  --                                                                                              ,("par","T7",0)])]

  --   it "should replace nonterminals with ancestors" $ do
  --     let consr :: Grammar Int Constr
  --         consr = grammar "T3" $ M.fromList [ ("T3", [ Ctor "nil" [], Ctor "cons" ["T4","T3"] ])
  --                                           , ("T4", [ Ctor "any" [] ])
  --                                           , ("T6", [ Ctor "any" [] ])
  --                                           , ("T7", [ Ctor "nil" [] ])]
  --         cons1' = evalState (epsilonClosure cons1) 0
  --     return (replaceNonterm "T5" "T3" cons1') `shouldBeLiteral` consr

  --   it "should replace an edge" $ do
  --     let consr :: Grammar Int Constr
  --         consr = grammar "T3" $ M.fromList [ ("T3", [ Ctor "nil" [], Ctor "cons" ["T4","T3"] ])
  --                                           , ("T4", [ Ctor "any" [] ])
  --                                           , ("T5", [ Ctor "nil" [], Ctor "cons" ["T6","T7"] ])
  --                                           , ("T6", [ Ctor "any" [] ])
  --                                           , ("T7", [ Ctor "nil" [] ])]
  --         cons1' = evalState (epsilonClosure cons1) 0
  --     return (replaceEdge "cons" "T3" 1 "T3" cons1') `shouldBeLiteral` consr

  --   it "should work on the examples from the paper" $ do
  --     widen' cons0 cons1 `shouldBeLiteral` cons01
  --     widen' cons1 cons2 `shouldBeLiteral` cons12
  --     widen' arith0 arith1 `shouldBe` arith01
  --     widen' arith1 (widen' arith0 arith1) `shouldBeLiteral` arith01
  --     widen' arith2 arith3 `shouldBeLiteral` arith3

  --   it "should be an upper bound" $ do
  --     let w_cons = widen' cons0 cons1
  --         w_pcf = widen' pcf_sub pcf
  --         w_arith = widen' arith0 arith1
  --     pcf `shouldSatisfy` (`subsetOf` w_pcf)
  --     pcf_sub `shouldSatisfy` (`subsetOf` w_pcf)
  --     cons0 `shouldSatisfy` subsetOf w_cons
  --     cons1 `shouldSatisfy` subsetOf w_cons
  --     arith0 `shouldSatisfy` subsetOf w_arith
  --     arith1 `shouldSatisfy` subsetOf w_arith

  where
    nondet :: Grammar Named Constr
    nondet = grammar "S" [ ("A", [ ("a",[]) ])
                         , ("G", [ ("g",[ "G" ])
                                 , ("g",[ "A" ])])
                         , ("F", [ ("f",[ "G", "G" ]) ])
                         ]
                         [ ("S", [ "F" ])]
    -- nondet' = grammar "S" $ [ ("S", [ Eps "F" ])
    --                         , ("A", [ Ctor "a" [] ])
    --                         , ("G", [ Ctor "g" [ "G" ]
    --                                 , Ctor "g" [ "A" ]])
    --                         , ("H", [ Eps "G" ])
    --                         , ("F", [ Ctor "f" [ "G", "H" ]])]
    -- infinite = grammar "EStart" $ [ ("EStart", [ Ctor "Bar" ["EStart"]])]
    pcf :: Grammar Named Constr
    pcf = grammar "PStart" [ ("Exp", [ ("App",["Exp", "Exp"])
                                     , ("Abs",["String", "Type", "Exp"])
                                     , ("Zero",[])
                                     , ("Succ",["Exp"])
                                     , ("Pred",["Exp"])
                                     , ("Ifz",["Exp", "Exp", "Exp"])
                                     ])
                           , ("Type", [ ("Num",[])
                                      , ("Fun",["Type", "Type"])
                                      ])
                           , ("String", [ ("String",[]) ])
                           ]

                           [ ("PStart", [ "Exp" , "Type" ]) ]
    pcf_sub :: Grammar Named Constr
    pcf_sub = grammar "PSStart" [ ("Exp", [ ("Succ",[ "Exp" ])
                                          , ("Pred",[ "Exp" ])
                                          , ("Zero",[])])
                                , ("Type", [ ("Num",[])
                                           , ("Fun",["Type", "Type"])])]
                                [ ("PSStart", [ "Exp" , "Type" ]) ]
    pcf_nondet :: Grammar Named Constr
    pcf_nondet = grammar "Start" [ ("A", [ ("a",[]) ])
                                 , ("G", [ ("g",[ "G" ])
                                         , ("g",[ "A" ])])
                                 , ("F", [ ("f",[ "G", "G" ])])
                                 , ("Exp", [ ("App",["Exp","Exp"])
                                           , ("Abs",["String", "Type", "Exp"])
                                           , ("Zero",[])
                                           , ("Succ",["Exp"])
                                           , ("Pred",["Exp"])
                                           , ("Ifz",["Exp", "Exp", "Exp"])])
                                 , ("Type", [ ("Num",[])
                                            , ("Fun",["Type","Type"])])
                                 , ("String", [ ("String",[]) ])]

                                 [ ("Start", [ "Exp" , "Type", "F" ]) ]

    -- cons0 :: Grammar Named Constr
    -- cons0 = grammar "T0" $ M.fromList [ ("T0", [ Ctor "nil" [], Ctor "cons" ["T1","T2"] ])
    --                                   , ("T1", [ Ctor "any" [] ])
    --                                   , ("T2", [ Ctor "nil" [] ])]
    -- cons1 :: Grammar Named Constr
    -- cons1 = grammar "T3" $ M.fromList [ ("T3", [ Ctor "nil" [], Ctor "cons" ["T4","T5"] ])
    --                                   , ("T4", [ Ctor "any" [] ])
    --                                   , ("T5", [ Ctor "nil" [], Ctor "cons" ["T6","T7"] ])
    --                                   , ("T6", [ Ctor "any" [] ])
    --                                   , ("T7", [ Ctor "nil" [] ])]
    -- cons2 :: Grammar Named Constr
    -- cons2 = grammar "T8" $ M.fromList [ ("T8", [ Ctor "nil" [], Ctor "cons" ["T9","T10"] ])
    --                                   , ("T9", [ Ctor "any" [] ])
    --                                   , ("T10", [ Ctor "nil" [], Ctor "cons" ["T11","T12"] ])
    --                                   , ("T11", [ Ctor "any" [] ])
    --                                   , ("T12", [ Ctor "nil" [], Ctor "cons" ["T13","T14"] ])
    --                                   , ("T13", [ Ctor "any" [] ])
    --                                   , ("T14", [ Ctor "nil" [] ])]
    -- cons01 :: Grammar Named Constr
    -- cons01 = grammar "T3" $ M.fromList [ ("T3", [ Ctor "nil" [], Ctor "cons" ["T4", "T3"] ])
    --                                    , ("T4", [ Ctor "any" [] ])]
    -- cons12 :: Grammar Named Constr
    -- cons12 = grammar "T8" $ M.fromList [ ("T8", [ Ctor "nil" [], Ctor "cons" ["T9", "T10"] ])
    --                                    , ("T9", [ Ctor "any" [] ])
    --                                    , ("T10", [ Ctor "nil" [], Ctor "cons" ["T11", "T10"] ])
    --                                    , ("T11", [ Ctor "any" [] ])]

    -- arith0 :: Grammar Named Constr
    -- arith0 = grammar "T0" $ M.fromList [ ("T0", [ Ctor "zero" [], Ctor "Add" ["Tx", "T1"] ])
    --                                    , ("Tx", [ Ctor "zero" [] ])
    --                                    , ("T1", [ Ctor "one" [], Ctor "Mul" ["T1","T2"] ])
    --                                    , ("T2", [ Ctor "cst" [], Ctor "par" ["Tx"], Ctor "var" [] ])]
    -- arith1 :: Grammar Named Constr
    -- arith1 = grammar "Tn" $ M.fromList [ ("Tn", [ Ctor "zero" [], Ctor "Add" ["T3","T6"] ])
    --                                    , ("T3", [ Ctor "zero" [], Ctor "Add" ["Tx","T4"] ])
    --                                    , ("Tx", [ Ctor "Zero" [] ])
    --                                    , ("T4", [ Ctor "one" [], Ctor "Mul" ["T4","T5"] ])
    --                                    , ("T5", [ Ctor "cst" [], Ctor "par" ["Tx"], Ctor "var" [] ])
    --                                    , ("T6", [ Ctor "one" [], Ctor "Mul" ["T6","T7"] ])
    --                                    , ("T7", [ Ctor "cst" [], Ctor "par" ["T3"], Ctor "var" [] ])]
    -- arith2 :: Grammar Named Constr
    -- arith2 = grammar "T0" $ M.fromList [ ("T0", [ Ctor "cst" [], Ctor "var" [] ]) ]
    -- arith3 :: Grammar Named Constr
    -- arith3 = grammar "Tn" $ M.fromList [ ("Tn", [ Ctor "cst" [], Ctor "par" ["Tx"], Ctor "var" [] ])
    --                                    , ("Tx", [ Ctor "zero" [] ]) ]
    -- arith01 :: Grammar Named Constr
    -- arith01 = grammar "Tn" $ M.fromList [ ("Tn", [ Ctor "zero" [], Ctor "Add" ["Tn","T6"] ])
    --                                     , ("T6", [ Ctor "one" [], Ctor "Mul" ["T6","T7"] ])
    --                                     , ("T7", [ Ctor "var" [], Ctor "par" ["Tn"], Ctor "cst" [] ])]

    shouldBeSubsetOf :: (Show n, Show (t n), IsGrammar n t) => Grammar n t -> Grammar n t -> Expectation
    shouldBeSubsetOf b1 b2 = unless (b1 `subsetOf` b2) $
      expectationFailure (printf "Grammar %s is not subset of %s" (show b1) (show b2))

    shouldNotBeSubsetOf :: (Show n, Show (t n), IsGrammar n t) => Grammar n t -> Grammar n t -> Expectation
    shouldNotBeSubsetOf b1 b2 = when (b1 `subsetOf` b2) $
      expectationFailure (printf "Grammar %s is subset of %s" (show b1) (show b2))


type NonTerminals = [String]
type Alphabet = [(Text,Int)]

instance Arbitrary (Grammar Named Constr) where
  arbitrary = arbitraryGrammar
              ["A","B","C","D","E","F"]
              [("a",0),("b",0),("c",0),("f",1),("g",2),("h",0),("h",1),("h",2)]

arbitraryGrammar :: NonTerminals -> Alphabet -> Gen (Grammar Named Constr)
arbitraryGrammar ns alph = do
  grammar <$> elements ns <*> genCons <*> genEps
  where

    genCons :: Gen [(String,Constr String)]
    genCons = forM ns $ \n -> do
      cs <- arbitraryConstr alph 1 3 (elements ns)
      return (n,cs)

    genEps :: Gen [(String,[String])]
    genEps = do
      dom <- listRange 0 2 (elements ns)
      forM dom $ \x -> do
        cod <- listRange 1 2 (elements ns)
        return (x,cod)

instance (Eq a, Hashable a, Arbitrary a) => Arbitrary (Constr a) where
  arbitrary = arbitraryConstr
              [("a",0),("b",0),("c",0),("f",1),("g",2),("h",0),("h",1),("h",2)]
              1 3
              arbitrary

arbitraryConstr :: (Eq n, Hashable n) => Alphabet -> Int -> Int -> Gen n -> Gen (Constr n)
arbitraryConstr alph n m ns = do
  constrs <- listRange n m (elements alph)
  cs <- forM constrs $ \(con,arity) -> do
    ts <- vectorOf arity ns
    return (con,ts)
  return (fromList cs)

listRange :: Int -> Int -> Gen a -> Gen [a]
listRange n m xs = do
  i <- choose (n,m)
  vectorOf i xs
