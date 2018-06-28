{-# LANGUAGE FlexibleContexts #-}
module Soundness where

import Syntax

import Control.Monad (unless)
import Control.Arrow (second)

import Data.GaloisConnection
import Data.Hashable
import Data.Order

import Data.Concrete.Powerset

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)

shouldBeApproximated :: (HasCallStack,Galois (Pow c) a,Show a,Show c) => c -> a -> Expectation
c `shouldBeApproximated` a = unless (ca ⊑ a) (expectationFailure msg)
  where ca = alpha (singleton c)
        msg = "soundness check failed: " ++ show ca ++ " ⊑ " ++ show a

soundImmediate :: (Arbitrary a,Show a,
                   Arbitrary b,Show b,
                   Galois (Pow vc) va,
                   Galois (Pow rc) ra,Complete ra,Eq rc,Hashable rc,Show rc,Show ra) =>
  Int -> String ->
  (a -> [(String,vc)]) -> (b -> Immediate) ->
  ([(String,vc)] -> Immediate -> rc) ->
  ([(String,va)] -> Immediate -> ra) ->
  Spec
soundImmediate n desc genMem genImmediate runConcrete runAbstract =
  modifyMaxSuccess (const n) $ it ("sound value approximation " ++ desc) $ property $ \(a,b) -> do
    let mem = genMem a
    let immediate = genImmediate b
    let rc = runConcrete mem immediate
    let ra = runAbstract (absMem mem) immediate
    rc `shouldBeApproximated` ra

soundBoolExpr :: (Arbitrary a,Show a,
                  Arbitrary b,Show b,
                  Galois (Pow vc) va,
                  Galois (Pow rc) ra,Complete ra,Eq rc,Hashable rc,Show rc,Show ra) =>
  Int -> String ->
  (a -> [(String,vc)]) -> (b -> BoolExpr) ->
  ([(String,vc)] -> BoolExpr -> rc) ->
  ([(String,va)] -> BoolExpr -> ra) ->
  Spec
soundBoolExpr n desc genMem genBool runConcrete runAbstract =
  modifyMaxSuccess (const n) $ it ("sound value approximation " ++ desc) $ property $ \(a,b) -> do
    let mem = genMem a
    let boolExpr = genBool b
    let rc = runConcrete mem boolExpr
    let ra = runAbstract (absMem mem) boolExpr
    rc `shouldBeApproximated` ra

soundExpr :: (Arbitrary a,Show a,
              Arbitrary b,Show b,
              Galois (Pow vc) va,
              Galois (Pow rc) ra,Complete ra,Eq rc,Hashable rc,Show rc,Show ra) =>
  Int -> String ->
  (a -> [(String,vc)]) -> (b -> Expr) ->
  ([(String,vc)] -> Expr -> rc) ->
  ([(String,va)] -> Expr -> ra) ->
  Spec
soundExpr n desc genMem genExpr runConcrete runAbstract =
  modifyMaxSuccess (const n) $ it ("sound value approximation " ++ desc) $ property $ \(a,b) -> do
    let mem = genMem a
    let expr = genExpr b
    let rc = runConcrete mem expr
    let ra = runAbstract (absMem mem) expr
    rc `shouldBeApproximated` ra

soundStatements :: (Arbitrary a,Show a,
                    Arbitrary b,Show b,
                    Galois (Pow vc) va,
                    Galois (Pow rc) ra,Complete ra,Eq rc,Hashable rc,Show rc,Show ra) =>
  Int -> String ->
  (a -> [(String,vc)]) -> (b -> [Statement]) ->
  ([(String,vc)] -> [Statement] -> rc) ->
  ([(String,va)] -> [Statement] -> ra) ->
  Spec
soundStatements n desc genMem genStatements runConcrete runAbstract =
  modifyMaxSuccess (const n) $ it ("sound value approximation " ++ desc) $ property $ \(a,b) -> do
    let mem = genMem a
    let stmts = genStatements b
    let rc = runConcrete mem stmts
    let ra = runAbstract (absMem mem) stmts
    rc `shouldBeApproximated` ra

soundProgram :: (Arbitrary a,Show a,
                 Galois (Pow rc) ra,Complete ra,Eq rc,Hashable rc,Show rc,Show ra) =>
  Int -> String ->
  CompilationUnit -> (a -> [Immediate]) ->
  (CompilationUnit -> [Immediate] -> rc) ->
  (CompilationUnit -> [Immediate] -> ra) ->
  Spec
soundProgram n desc units genParams runConcrete runAbstract =
  modifyMaxSuccess (const n) $ it ("sound value approximation " ++ desc) $ property $ \a -> do
    let params = genParams a
    let rc = runConcrete units params
    let ra = runAbstract units params
    rc `shouldBeApproximated` ra

absMem :: Galois (Pow vc) va => [(String,vc)] -> [(String,va)]
absMem = map $ second (alpha . singleton)
