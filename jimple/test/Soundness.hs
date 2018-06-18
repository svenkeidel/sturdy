{-# LANGUAGE FlexibleContexts #-}
module Soundness where

import Syntax

import Control.Monad (unless)

import Data.Exception
import Data.GaloisConnection
import Data.Hashable
import Data.Order

import Data.Concrete.Error as Con
import Data.Abstract.HandleError as Abs
import Data.Concrete.Powerset as Con

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)

withSize :: Spec -> Spec
withSize = modifyMaxSuccess (const 1000)

shouldBeApproximated :: (HasCallStack,Galois (Con.Pow c) a,Show a,Show c) => c -> a -> Expectation
c `shouldBeApproximated` a = unless (ca ⊑ a) (expectationFailure msg)
  where ca = alpha (singleton c)
        msg = "soundness check failed: " ++ show ca ++ " ⊑ " ++ show a

soundImmediate :: (Arbitrary a,Show a,Galois (Con.Pow vc) va,Complete va,Eq vc,Hashable vc,Show vc,Show va) =>
  String ->
  [(String,vc)] -> (a -> Immediate) ->
  ([(String,vc)] -> Immediate -> Con.Error (Exception vc) vc) ->
  ([(String,va)] -> Immediate -> Abs.Error (Exception va) va) ->
  Spec
soundImmediate desc mem gen runConcrete runAbstract =
  it ("sound value approximation " ++ desc) $ property $ \a -> do
    let mema = map (\(l,vc) -> (l,alpha (singleton vc))) mem
    let i = gen a
    let rc = runConcrete mem i
    let ra = runAbstract mema i
    rc `shouldBeApproximated` ra

soundBoolExpr :: (Galois (Con.Pow vc) va,Complete va,Eq vc,Hashable vc,Show vc,Show va,
                  Galois (Con.Pow bc) ba,Complete ba,Eq bc,Hashable bc,Show bc,Show ba) =>
  String ->
  [(String,vc)] -> BoolExpr ->
  ([(String,vc)] -> BoolExpr -> Con.Error (Exception vc) bc) ->
  ([(String,va)] -> BoolExpr -> Abs.Error (Exception va) ba) ->
  Spec
soundBoolExpr desc mem x runConcrete runAbstract =
  it ("sound value approximation " ++ desc) $ do
    let mema = map (\(l,vc) -> (l,alpha (singleton vc))) mem
    let rc = runConcrete mem x
    let ra = runAbstract mema x
    rc `shouldBeApproximated` ra

soundExpr :: (Arbitrary a,Show a,Galois (Con.Pow vc) va,Complete va,Eq vc,Hashable vc,Show vc,Show va) =>
  String ->
  [(String,vc)] -> (a -> Expr) ->
  ([(String,vc)] -> Expr -> Con.Error (Exception vc) vc) ->
  ([(String,va)] -> Expr -> Abs.Error (Exception va) va) ->
  Spec
soundExpr desc mem gen runConcrete runAbstract =
  it ("sound value approximation " ++ desc) $ property $ \a -> do
    let mema = map (\(l,vc) -> (l,alpha (singleton vc))) mem
    let expr = gen a
    let rc = runConcrete mem expr
    let ra = runAbstract mema expr
    rc `shouldBeApproximated` ra

soundStatements :: (Galois (Con.Pow vc) va,Complete va,Eq vc,Hashable vc,Show vc,Show va) =>
  String ->
  [(String,vc)] -> [Statement] ->
  ([(String,vc)] -> [Statement] -> Con.Error (Exception vc) (Maybe vc)) ->
  ([(String,va)] -> [Statement] -> Abs.Error (Exception va) (Maybe va)) ->
  Spec
soundStatements desc mem x runConcrete runAbstract =
  it ("sound value approximation " ++ desc) $ do
    let mema = map (\(l,vc) -> (l,alpha (singleton vc))) mem
    let rc = runConcrete mem x
    let ra = runAbstract mema x
    rc `shouldBeApproximated` ra

soundProgram :: (Galois (Con.Pow vc) va,Complete va,Eq vc,Hashable vc,Show vc,Show va) =>
  String ->
  [CompilationUnit] -> (Method,[Immediate]) ->
  ([CompilationUnit] -> (Method,[Immediate]) -> Con.Error (Exception vc) (Maybe vc)) ->
  ([CompilationUnit] -> (Method,[Immediate]) -> Abs.Error (Exception va) (Maybe va)) ->
  Spec
soundProgram desc units x runConcrete runAbstract =
  it ("sound value approximation " ++ desc) $ do
    let rc = runConcrete units x
    let ra = runAbstract units x
    rc `shouldBeApproximated` ra
