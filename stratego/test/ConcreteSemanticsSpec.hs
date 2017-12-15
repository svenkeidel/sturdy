{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ConcreteSemanticsSpec(main, spec) where

import           Prelude hiding (map)
import qualified Prelude as P

import           ConcreteSemantics
import           Syntax hiding (Fail,TermPattern(..))
import qualified Syntax as T

import           Paths_system_s

import           Control.Monad

import           Data.ATerm
import           Data.Term (TermUtils(..))
import           Data.Result hiding (map)
import qualified Data.HashMap.Lazy as M

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (Result(..))

import qualified Data.Text.IO as TIO

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  
  describe "scope" $ do
    it "should hide declare variables" $ do
      let tenv = termEnv [("x", term1)]
      eval (Scope ["x"] (Build "x")) M.empty tenv term2 
        `shouldBe` Fail
      eval (Scope ["x"] (Match "x")) M.empty tenv term2
        `shouldBe` Success (term2,tenv)

    it "should make non-declared variables available" $ do
      let tenv = termEnv [("x", term1)]
      eval (Scope ["y"] (Build "x")) M.empty tenv term2 `shouldBe`
        Success (term1,tenv)
      eval (Scope ["y"] (Match "z")) M.empty tenv term2 `shouldBe`
        Success (term2,termEnv [("x", term1), ("z", term2)])

  describe "let" $
    it "should support recursion" $ do
      let t = convertToList (NumberLiteral <$> [2, 3, 4])
          tenv = termEnv []; tenv' = termEnv [("x",t)]
      eval (Let [("map", map)] (Match "x" `Seq` Call "map" [Build 1] ["x"])) M.empty tenv t
        `shouldBe`
           Success (convertToList [1, 1, 1], tenv')

  describe "call" $
    it "should support recursion" $ do
      let senv = M.fromList [("map", Closure map M.empty)]
          t = convertToList [2, 3, 4]
          tenv = termEnv []; tenv' = termEnv [("x",t)]
      eval (Match "x" `Seq` Call "map" [Build (T.NumberLiteral 1)] ["x"]) senv tenv t
        `shouldBe`
           Success (convertToList [1, 1, 1], tenv')

  describe "match" $ do
    prop "should introduce variables" $ \t ->
      let tenv = termEnv [] 
      in eval (Match "x" `Seq` Match "y") M.empty tenv t `shouldBe`
           Success (t, termEnv [("x", t), ("y", t)])

    prop "should support linear pattern matching" $ \t1 t2 ->
      let t' = Cons "f" [t1,t2]
          tenv = termEnv []; tenv' = termEnv [("x",t1)]
      in eval (Match (T.Cons "f" ["x","x"])) M.empty tenv t' `shouldBe`
           if t1 == t2 then Success (t', tenv') else Fail

    prop "should match deep" $ \t -> do
      p <- similarTermPattern t 3
      return $ counterexample (show p) $ when (linear p) $
        fmap fst (eval (Match p) M.empty (termEnv []) t) `shouldBe`
          Success t

    it "should succeed when exploding literals" $
      let tenv = termEnv []; tenv' = termEnv [("x", Cons "Nil" [])]
      in eval (Match (T.Explode "_" "x")) M.empty tenv 1 `shouldBe`
           Success (1, tenv')

  describe "build" $ do
    prop "build should be inverse to match" $ \t -> do
      p <- similarTermPattern t 3
      return $ counterexample (show p) $ when (linear p) $
        fmap fst (eval (Match p `Seq` Build p) M.empty (termEnv []) t) `shouldBe`
          Success t

    prop "build should lookup variables" $ \t -> do
      let tenv = termEnv [("x", t)]
      eval (Build (T.Var "x")) M.empty tenv t `shouldBe`
        Success (t,tenv)

  describe "Case Studies" $ describe "Haskell Arrows" $ beforeAll parseArrowCaseStudy $ do

    it "union should work" $ \module_ -> do
      let l1 = convertToList [1,2,3,4]
          l2 = convertToList [2,4]
          t = tup l1 l2
          tenv = termEnv []
      eval (Call "union_0_0" [] []) (stratEnv module_) tenv t
        `shouldBe`
           Success (convertToList [1,3,2,4], tenv)

    it "concat should work" $ \module_ ->
      let l = convertToList (fmap convertToList [[1,2,3],[4,5],[],[6]])
          tenv = termEnv []
      in eval (Call "concat_0_0" [] []) (stratEnv module_) tenv l
        `shouldBe`
           Success (convertToList [1,2,3,4,5,6], tenv)

    it "free-pat-vars should work" $ \module_ ->
      let var x = Cons "Var" [x]
          tuple x y = Cons "Tuple" [x,convertToList y]
          t = tuple (tuple (var "a") [var "b"])
                    [tuple (var "c") [var "a"]]
          tenv = termEnv []
      in eval (Call "free_pat_vars_0_0" [] []) (stratEnv module_) tenv t
          `shouldBe`
             Success (convertToList [var "b", var "c", var "a"], tenv)

  where

    map = Strategy ["f"] ["l"] (Scope ["x","xs","x'","xs'"] (
            Build "l" `Seq`
            GuardedChoice
              (Match (T.Cons "Cons" ["x","xs"]))
              (Build "x" `Seq`
               Call "f" [] [] `Seq`
               Match "x'" `Seq`
               Call "map" ["f"] ["xs"] `Seq`
               Match "xs'" `Seq`
               Build (T.Cons "Cons" ["x'", "xs'"]))
              (Build (T.Cons "Nil" []))))

    termEnv = TermEnv . M.fromList
    
    term1 = NumberLiteral 1
    term2 = NumberLiteral 2

    parseArrowCaseStudy = do
      file <- TIO.readFile =<< getDataFileName "case-studies/arrows/arrows.aterm"
      case parseModule =<< parseATerm file of
        Left e -> P.fail (show e)
        Right module_ -> return module_

    tup x y = Cons "" [x,y]


