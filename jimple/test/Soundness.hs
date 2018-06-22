{-# LANGUAGE FlexibleContexts #-}
module Soundness where

import Syntax

import Control.Monad (unless)

import Data.GaloisConnection
import Data.Hashable
import Data.Order

import Data.Concrete.Error as Con
import Data.Abstract.HandleError as Abs
import Data.Concrete.Exception as Con
import Data.Abstract.Exception as Abs
import Data.Concrete.Powerset as Con

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)

shouldBeApproximated :: (HasCallStack,Galois (Con.Pow c) a,Show a,Show c) => c -> a -> Expectation
c `shouldBeApproximated` a = unless (ca ⊑ a) (expectationFailure msg)
  where ca = alpha (singleton c)
        msg = "soundness check failed: " ++ show ca ++ " ⊑ " ++ show a

soundImmediate :: (Arbitrary a,Show a,
                   Arbitrary b,Show b,
                   Galois (Con.Pow vc) va,Complete va,Eq vc,Hashable vc,Show vc,Show va) =>
  Int -> String ->
  (a -> [(String,vc)]) -> (b -> Immediate) ->
  ([(String,vc)] -> Immediate -> Con.Error (Con.Exception vc) vc) ->
  ([(String,va)] -> Immediate -> Abs.Error (Abs.Exception va) va) ->
  Spec
soundImmediate n desc genMem genImmediate runConcrete runAbstract =
  modifyMaxSuccess (const n) $ it ("sound value approximation " ++ desc) $ property $ \(a,b) -> do
    let mem = genMem a
    let mema = map (\(l,vc) -> (l,alpha (singleton vc))) mem
    let immediate = genImmediate b
    let rc = runConcrete mem immediate
    let ra = runAbstract mema immediate
    rc `shouldBeApproximated` ra

soundBoolExpr :: (Arbitrary a,Show a,
                  Arbitrary b,Show b,
                  Galois (Con.Pow vc) va,Complete va,Eq vc,Hashable vc,Show vc,Show va,
                  Galois (Con.Pow bc) ba,Complete ba,Eq bc,Hashable bc,Show bc,Show ba) =>
  Int -> String ->
  (a -> [(String,vc)]) -> (b -> BoolExpr) ->
  ([(String,vc)] -> BoolExpr -> Con.Error (Con.Exception vc) bc) ->
  ([(String,va)] -> BoolExpr -> Abs.Error (Abs.Exception va) ba) ->
  Spec
soundBoolExpr n desc genMem genBool runConcrete runAbstract =
  modifyMaxSuccess (const n) $ it ("sound value approximation " ++ desc) $ property $ \(a,b) -> do
    let mem = genMem a
    let mema = map (\(l,vc) -> (l,alpha (singleton vc))) mem
    let boolExpr = genBool b
    let rc = runConcrete mem boolExpr
    let ra = runAbstract mema boolExpr
    rc `shouldBeApproximated` ra

soundExpr :: (Arbitrary a,Show a,
              Arbitrary b,Show b,
              Galois (Con.Pow vc) va,Complete va,Eq vc,Hashable vc,Show vc,Show va) =>
  Int -> String ->
  (a -> [(String,vc)]) -> (b -> Expr) ->
  ([(String,vc)] -> Expr -> Con.Error (Con.Exception vc) vc) ->
  ([(String,va)] -> Expr -> Abs.Error (Abs.Exception va) va) ->
  Spec
soundExpr n desc genMem genExpr runConcrete runAbstract =
  modifyMaxSuccess (const n) $ it ("sound value approximation " ++ desc) $ property $ \(a,b) -> do
    let mem = genMem a
    let mema = map (\(l,vc) -> (l,alpha (singleton vc))) mem
    let expr = genExpr b
    let rc = runConcrete mem expr
    let ra = runAbstract mema expr
    rc `shouldBeApproximated` ra

soundStatements :: (Arbitrary a,Show a,
                    Arbitrary b,Show b,
                    Galois (Con.Pow vc) va,Complete va,Eq vc,Hashable vc,Show vc,Show va) =>
  Int -> String ->
  (a -> [(String,vc)]) -> (b -> [Statement]) ->
  ([(String,vc)] -> [Statement] -> Con.Error (Con.Exception vc) (Maybe vc)) ->
  ([(String,va)] -> [Statement] -> Abs.Error (Abs.Exception va) (Maybe va)) ->
  Spec
soundStatements n desc genMem genStatements runConcrete runAbstract =
  modifyMaxSuccess (const n) $ it ("sound value approximation " ++ desc) $ property $ \(a,b) -> do
    let mem = genMem a
    let mema = map (\(l,vc) -> (l,alpha (singleton vc))) mem
    let stmts = genStatements b
    let rc = runConcrete mem stmts
    let ra = runAbstract mema stmts
    rc `shouldBeApproximated` ra

soundProgram :: (Arbitrary a,Show a,
                 Galois (Con.Pow vc) va,Complete va,Eq vc,Hashable vc,Show vc,Show va) =>
  Int -> String ->
  CompilationUnit -> (a -> [Immediate]) ->
  (CompilationUnit -> [Immediate] -> Con.Error (Con.Exception vc) (Maybe vc)) ->
  (CompilationUnit -> [Immediate] -> Abs.Error (Abs.Exception va) (Maybe va)) ->
  Spec
soundProgram n desc units genParams runConcrete runAbstract =
  modifyMaxSuccess (const n) $ it ("sound value approximation " ++ desc) $ property $ \a -> do
    let params = genParams a
    let rc = runConcrete units params
    let ra = runAbstract units params
    rc `shouldBeApproximated` ra
