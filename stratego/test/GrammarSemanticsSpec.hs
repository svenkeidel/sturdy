{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
module GrammarSemanticsSpec(main, spec) where

import           Prelude hiding (error)
import qualified Prelude as P

import qualified ConcreteSemantics as C
import           GrammarSemantics
import           SharedSemantics
import           Syntax hiding (Fail)
import           Syntax as T

import           Control.Arrow

import           Data.Abstract.FreeCompletion
import           Data.Abstract.Except as E
import           Data.Abstract.Error as F
import           Data.Abstract.DiscretePowerset(Pow)
import qualified Data.Abstract.WeakMap as S
import           Data.Abstract.Terminating (fromTerminating)
import qualified Data.Concrete.Powerset as C
import           Data.GaloisConnection
import qualified Data.HashMap.Lazy as LM
import           Data.Set (Set)
import           Data.Term hiding (wildcard)

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (Success)

import           Text.Printf
import           GHC.Exts(IsString(..),IsList(..))

import           Data.Abstract.TreeGrammar
import qualified CaseStudy

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  it "" $
    pendingWith "work in progress"
  describe "Utilities" $ do
    it "convertToList should work" $
      convertToList [ stringLit "foo", stringLit "bar", stringLit "baz" ]
        `shouldBe` Term (grammar "S0" [("S0", Constr [("Cons",[ "S1", "S2" ])] )
                                      ,("S1", StringLit "foo" )
                                      ,("S2", Constr [("Cons",[ "S3", "S4" ])] )
                                      ,("S3", StringLit "bar" )
                                      ,("S4", Constr [("Cons",[ "S5", "S6" ])] )
                                      ,("S5", StringLit "baz" )
                                      ,("S6", Constr [("Nil",[])]  )] []
                        )

    it "convertToList should work on an empty list" $
      convertToList [] `shouldBe` Term (grammar "S0" [("S0", Constr [("Nil",[])] )] [])

  describe "Match" $ do
    it "should match an identical builtin string literal" $
      geval 1 (Match (StringLiteral "x")) (termEnv []) (stringLit "x") `shouldBe`
        success (termEnv [], stringLit "x")

    it "should not match another builtin string literal" $
      geval 1 (Match (StringLiteral "y")) (termEnv []) (stringLit "x") `shouldBe`
        uncaught ()

    it "should match an equal builtin number literal" $
      geval 1 (Match (NumberLiteral 42)) (termEnv []) (numLit 42) `shouldBe`
        success (termEnv [], numLit 42)

    it "should not match another builtin number literal" $
      geval 1 (Match (NumberLiteral 1)) (termEnv []) (numLit 42) `shouldBe`
        uncaught ()

    it "a string grammar should not match a number literal" $
      geval 1 (Match (NumberLiteral 1)) (termEnv []) (stringLit "x") `shouldBe`
        uncaught ()

    it "a number grammar should not match a string literal" $
      geval 1 (Match (StringLiteral "x")) (termEnv []) (numLit 42) `shouldBe`
        uncaught ()

    it "should match a PCF expression" $
      geval 1 (Match (Cons "Zero" [])) (termEnv []) pcf `shouldBe`
        successOrFail () (termEnv [], constr "Zero" [])

    it "should match a nested PCF expression" $
      geval 1 (Match (Cons "Succ" [Cons "Zero" []])) (termEnv []) pcf `shouldBe`
        successOrFail () (termEnv [], Term (grammar "S0" [("S0", Constr [("Succ",["S1"])] )
                                                                ,("S1", Constr [("Zero",[])] )]
                                                   []))

    it "should match a constructor with more than one argument" $
      geval 1 (Match (Cons "Ifz" [Cons "Zero" [], Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) (termEnv []) pcf `shouldBe`
        successOrFail () (termEnv [], Term (grammar "S0" [("S0", Constr [("Ifz",["S1","S2","S3"])])
                                                                ,("S1", Constr [("Zero",[])])
                                                                ,("S2", Constr [("Succ",["S3"])])
                                                                ,("S3", Constr [("Zero",[])])
                                                                ,("S4", Constr [("Zero",[])])]
                                                                []))

    it "should introduce one variable" $
      let g = grammar "S0" [("S0", Constr [("Succ",[ "S1" ])] )
                           ,("S1", Constr [("Zero",[])] )] []
          g' = grammar "S0" [("S0", Constr [("Zero",[])] )] []

      in geval 1 (Match (Cons "Succ" ["x"])) (S.delete "x" (termEnv [])) (Term g) `shouldBe`
        success (termEnv [("x", Term g')], Term g)

    it "should introduce one variable 2" $ do
      pendingWith "unsupported"
      let g = grammar "S0" [("S0", Constr [("Succ",[ "S1" ])] )
                           ,("S1", Constr [("Succ",[ "S2" ])] )
                           ,("S2", Constr [("Zero",[])] )]
                           []
          g' = grammar "S0" [("S0", Constr [("Succ",[ "S1" ])] )
                            ,("S1", Constr [("Zero",[])] )]
                            []
      geval 10 (Match (Cons "Succ" ["x"])) (termEnv []) (Term g) `shouldBe`
        success (termEnv [("x", Term g')], Term g)

    it "should introduce multiple variables and support linear pattern matching" $ do
      let g = grammar "S0" [("S0", Constr [("Succ",[ "S1" ])] )
                           ,("S1", Constr [("Succ",[ "S2" ])] )
                           ,("S2", Constr [("Zero",[])] )]
                           []
          g' = grammar "S0" [("S0", Constr [("Succ",[ "S1" ])] )
                            ,("S1", Constr [("Zero",[])] )]
                            []
          tenv = S.delete' (["x","y"] :: [TermVar]) (termEnv [])
      geval 2 (Match (Cons "Succ" ["x"]) `Seq` Match (Cons "Succ" ["y"])) tenv (Term g)
        `shouldBe` success (termEnv [("x", Term g'), ("y", Term g')], Term g)

    it "should support linear pattern matching" $
      geval 2 (Match (Cons "Succ" ["x"]) `Seq` Match (Cons "Var" ["x"])) (termEnv []) pcf `shouldBe`
        uncaught ()

    it "should succeed when exploding literals" $
    --   let tenv = termEnv []; tenv' = termEnv [("x", Cons "Nil" [])]
    --   in eval (Match (T.Explode "_" "x")) M.empty tenv 1 `shouldBe`
    --        Right (tenv', 1)
      pendingWith "Explosion is not yet implemented"

    it "should handle inconsistent environments" $ do
      let t1 = C.Cons "f" []
          t2 = C.Cons "g" []
      sound' 5 (Match "x") [(t1, [("x", t1)]), (t2, [("y", t2)])]

    prop "should be sound" $ do
      ~[t1,t2,t3] <- C.similarTerms 3 7 2 10
      matchPattern <- C.similarTermPattern t1 3
      return $ counterexample
                 (printf "pattern: %s\n %s ⊔ %s = %s"
                    (show matchPattern) (show t2) (show t3)
                    (showLub t2 t3))
             $ sound' 5 (Match matchPattern) [(t2,[]),(t3,[])]

  describe "Build" $ do

    it "should build a builtin string literal" $
      geval 1 (Build (StringLiteral "foo")) (termEnv []) (Term empty) `shouldBe`
        success (termEnv [], stringLit "foo")

    it "should build a builtin number literal" $
      geval 1 (Build (NumberLiteral 1)) (termEnv []) (Term empty) `shouldBe`
        success (termEnv [], numLit 1)

    it "should build a simple constant PCF expression" $
      let zero = grammar "S0" [("S0", Constr [("Zero",[])])] []
      in geval 1 (Build (Cons "Zero" [])) (termEnv []) (Term empty) `shouldBe`
          success (termEnv [], Term zero)

    it "should build a nested PCF expression" $
      let g = grammar "S0" [("S0", Constr [("Succ",["S1"])])
                           ,("S1", Constr [("Zero",[])])]
                           []
      in geval 1 (Build (Cons "Succ" [Cons "Zero" []])) (termEnv []) (Term empty) `shouldBe`
          success (termEnv [], Term g)

    it "should build a constructor with more than one argument" $
      let g = grammar "S" [("S",  Constr [("Ifz",["S1", "S2", "S1"])])
                          ,("S1", Constr [("Zero",[])] )
                          ,("S2", Constr [("Succ",["S1"])])]
                          []
      in geval 1 (Build (Cons "Ifz" [Cons "Zero" [], Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) (termEnv []) (Term empty) `shouldBe`
          success (termEnv [], Term g)

    it "build should be inverse to match" $
      let term = NumberLiteral 1
      in geval 2 (Match term `Seq` Build term) (termEnv []) (numLit 1) `shouldBe`
          success (termEnv [], numLit 1)

    it "build should be inverse to match with a more complicated term" $
      let term = Cons "Cons" [Var "x", Var "xs"]
          g = convertToList [numLit 1]
          tenv = S.delete' (["x","xs"] :: [TermVar]) $ termEnv []
      in geval 2 (Match term `Seq` Build term) tenv g `shouldBe`
          success (termEnv [("x", numLit 1), ("xs", Term (grammar "S" [("S", Constr [("Nil",[])])] []))], g)

    it "should throw away the current subject grammar if needed" $
      let tenv = termEnv [("x", numLit 42)]
      in geval 1 (Build (Var "x")) tenv (stringLit "x") `shouldBe`
          success (tenv, numLit 42)

    it "should lookup variables" $
      let tenv = termEnv [("x", pcf)]
      in geval 1 (Build (Var "x")) tenv (Term empty) `shouldBe` success (tenv, pcf)

    it "should merge two variables into one grammar" $
      let tenv = termEnv [("x", numLit 42), ("y", stringLit "x")]
      in geval 1 (Build (Cons "foo" [Var "x", Var "y"])) tenv (Term empty) `shouldBe`
         success (tenv, Term $ grammar "S" [("S",  Constr [("foo",["S1", "S2" ])])
                                           ,("S1", NumLit 42 )
                                           ,("S2", StringLit "x" )] [])

    it "should support linear pattern matching" $
      let tenv = termEnv [("x", numLit 42)]
      in geval 1 (Build (Cons "foo" [Var "x", Var "x"])) tenv (Term empty) `shouldBe`
         success (tenv, Term $ grammar "S" [("S",  Constr [("foo",["S1", "S1" ])])
                                           ,("S1", NumLit 42 )
                                           ,("S2", NumLit 42 )] [])

    it "should merge a variable and the given subject grammar" $
      let tenv = termEnv [("x", numLit 42)]
      in geval 1 (Build (Cons "Ifz" [Var "x", Cons "Succ" [Cons "Zero" []], Cons "Zero" []])) tenv pcf `shouldBe`
         success (tenv, Term $ grammar "S" [("S",  Constr [("Ifz",["S1", "S2", "S3"])])
                                           ,("S1", NumLit 42 )
                                           ,("S2", Constr [("Succ",["S3"])])
                                           ,("S3", Constr [("Zero",[])])] [])

    prop "should be sound" $ do
      ~[t1,t2,t3] <- C.similarTerms 3 7 2 10
      matchPattern <- C.similarTermPattern t1 3
      let vars = termVars matchPattern :: Set TermVar
      buildPattern <- arbitraryTermPattern 5 2 (if not (null vars) then elements (toList vars) else arbitrary)
      return $ counterexample
               (printf "match pattern: %s\nbuild pattern: %s\nt2: %s\nt3: %s\nlub t2 t3 = %s"
                 (show matchPattern) (show buildPattern) (show t2) (show t3)
                 (showLub t2 t3))
             $ sound' 5 (Match matchPattern `Seq` Build buildPattern) [(t2,[]),(t3,[])]

  describe "Scope" $ do
    it "should hide declared variables" $ do
      let tenv = termEnv [("x", numLit 42)]
      geval 1 (Scope ["x"] (Build "x")) tenv (numLit 42) `shouldBe`
        error "unbound term variable x in build statement !x"
      geval 2 (Scope ["x"] (Match "x")) tenv (numLit 42) `shouldBe`
        success (tenv, numLit 42)

    it "should make non-declared variables available" $ do
      let tenv = termEnv [("x", numLit 42)]
      geval 2 (Scope ["y"] (Build "x")) tenv (numLit 42) `shouldBe`
        success (S.delete "y" tenv, numLit 42)
      geval 2 (Scope ["y"] (Match "z")) (S.delete "z" tenv) (numLit 42) `shouldBe`
        success (S.delete "y" $ termEnv [("x", numLit 42), ("z", numLit 42)], numLit 42)

    it "should hide variables bound in a choice's test from the else branch" $
      let or1 = Build (T.Cons "Zero" []) `Seq` Match "x" `Seq` T.Fail in
      let or2 = Match "x" in
      geval 0 (or1 `leftChoice` or2) (S.delete "x" (termEnv [])) (Term zeroOne) `shouldBe`
        success (termEnv [("x", Term zeroOne)], Term zeroOne)

  describe "Let" $ do
    it "should apply a single function call" $ do
      let t = grammar "S" ([("S",  Constr [("Tuple",["F", "G"])] )
                           ,("F",  StringLit "foo" )
                           ,("G",  StringLit "bar" )])
                          []
          t' = grammar "S" ([("S",  Constr [("Tuple",["G", "F"])] )
                            ,("F",  StringLit "foo" )
                            ,("G",  StringLit "bar" )])
                           []
          tenv = termEnv []
      geval 6 (Let [("swap", swap')] (Call "swap" [] [])) tenv (Term t)
        `shouldBe` success (delete swap' tenv, Term t')

    it "should support recursion" $ do
      let t = convertToList (map numLit [2, 3, 4])
          t' = convertToList (map numLit [1, 1, 1])
          tenv = termEnv []
      geval 13 (Let [("map", map')] (Scope ["x"] $ Match "x" `Seq` Call "map" [Build (NumberLiteral 1)] ["x"])) tenv t
        `shouldBe` success (delete map' tenv, t')

  describe "Call" $ do
    it "should support a singleton list in recursive applications" $ do
      let senv = LM.fromList [("map", Closure map' LM.empty)]
          t = convertToList (map numLit [2])
          t' = convertToList (map numLit [1])
          tenv = termEnv []
      -- The extra Nil constructor comes from the fact that the match
      -- on "x" introduces top', because "x" is not found in the
      -- initially empty environment. The guarded choice, then, has to take the least
      -- upper bound, which results in the extra Nil constructor.
      geval' 13 (Scope ["x"] $ Match "x" `Seq` Call "map" [Build (NumberLiteral 1)] ["x"]) senv tenv t
        `shouldBe` success (delete map' tenv, t')

    it "should support recursion on a list of numbers" $ do
      let senv = LM.fromList [("map", Closure map' LM.empty)]
          t = convertToList (map numLit [2, 3, 4])
          t' = convertToList (map numLit [1, 1, 1])
          tenv = termEnv []
      geval' 12 (Scope ["x"] $ Match "x" `Seq` Call "map" [Build (NumberLiteral 1)] ["x"]) senv tenv t
        `shouldBe` success (delete map' tenv, t')

    prop "should be sound" $ do
      i <- choose (0,10)
      j <- choose (0,10)
      l <- C.similarTerms i 7 2 10
      let (l1,l2) = splitAt j l
      let t1 = convertToList l1
      let t2 = convertToList l2
      return $ counterexample (printf "t: %s\n" (showLub t1 t2))
             $ sound' 10 (Let [("map", map')] (Match "x" `Seq` Call "map" [Build 1] ["x"])) [(t1,[]),(t2,[])]

    -- describe "Construction" $ do
    --   before CaseStudy.pcf $ do
    --     it "should correctly construct an RTG from a Stratego signature" $ \pcfModule ->
    --       fromContext (signature pcfModule) `shouldBe` pcf

  where
    geval' :: Int -> Strat -> StratEnv -> TermEnv -> Term -> Error (Pow String) (Except () (TermEnv, Term))
    geval' i strat senv tenv g = fromCompletion (P.error "completion.top")
                               $ fromTerminating (P.error "non-Terminating")
                               $ eval i strat senv tenv g

    geval :: Int -> Strat -> TermEnv -> Term -> Error (Pow String) (Except () (TermEnv, Term))
    geval i strat tenv g = geval' i strat LM.empty tenv g

    sound' :: Int -> Strat -> [(C.Term,[(TermVar,C.Term)])] -> Property
    sound' i s xs = sound i LM.empty pow (eval' s) (eval' s) where
      pow = C.fromFoldable $ fmap (second fromList) xs

    termEnv = S.fromList

    delete :: TermVars s => s -> TermEnv -> TermEnv
    delete s = S.delete' (termVars s :: Set TermVar)

    showLub :: C.Term -> C.Term -> String
    showLub t1 t2 = show (alpha ([t1,t2] :: C.Pow C.Term) :: Term)

    empty :: Grammar Int Constr
    empty = grammar "empty" [ ("empty", mempty) ] []

    pcf :: Term
    pcf = Term $ grammar "PStart" [ ("Exp", Constr
                                     [ ("App",["Exp", "Exp"])
                                     , ("Abs",["String", "Type", "Exp"])
                                     , ("Zero",[])
                                     , ("Succ",["Exp"])
                                     , ("Pred",["Exp"])
                                     , ("Ifz",["Exp", "Exp", "Exp"])
                                     ] )
                           , ("Type",Constr
                                      [ ("Num",[])
                                      , ("Fun",["Type", "Type"])
                                      ])
                           , ("String", Constr [ ("String",[]) ] )
                           ]
                           [ ("PStart", [ "Exp" , "Type" ]) ]

    zeroOne = grammar "S" [
        ("S",  Constr [("Zero",[]),("One",[])])
      ] []

    swap' = Strategy [] [] (Scope ["x","y"] (
                               Match (Cons "Tuple" ["x","y"])
                               `Seq`
                               Build (Cons "Tuple" ["y","x"]))
                                                  )
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

    error :: String -> Error (Pow String) a
    error = F.Fail . fromString

    success :: a -> Error (Pow String) (Except () a)
    success a = F.Success $ E.Success a
    
    successOrFail :: () -> a -> Error (Pow String) (Except () a)
    successOrFail () a = F.Success $ E.SuccessOrFail () a
    
    uncaught :: () -> Error (Pow String) (Except () a)
    uncaught = F.Success . E.Fail
