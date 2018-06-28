{-# LANGUAGE FlexibleContexts #-}
module JimpleSoundness where

import Soundness
import Syntax

import Data.GaloisConnection
import Data.Hashable
import Data.Order

import Data.Concrete.Powerset as Con
import Data.Concrete.Error as Con
import Data.Abstract.HandleError as Abs
import Data.Concrete.Exception as Con
import Data.Abstract.Exception as Abs

import Classes.ArrayFieldExample

import Test.Hspec
import Test.QuickCheck

jimpleSoundness :: (Galois (Con.Pow vc) va,Complete va,Eq vc,Hashable vc,Show vc,Show va,
                    Galois (Con.Pow bc) ba,Complete ba,Eq bc,Hashable bc,Show bc,Show ba,
                    Arbitrary vc) =>
  ([(String,vc)] -> Immediate -> Con.Error (Con.Exception vc) vc) ->
  ([(String,va)] -> Immediate -> Abs.Error (Abs.Exception va) va) ->
  ([(String,vc)] -> BoolExpr -> Con.Error (Con.Exception vc) bc) ->
  ([(String,va)] -> BoolExpr -> Abs.Error (Abs.Exception va) ba) ->
  ([(String,vc)] -> Expr -> Con.Error (Con.Exception vc) vc) ->
  ([(String,va)] -> Expr -> Abs.Error (Abs.Exception va) va) ->
  ([(String,vc)] -> [Statement] -> Con.Error (Con.Exception vc) (Maybe vc)) ->
  ([(String,va)] -> [Statement] -> Abs.Error (Abs.Exception va) (Maybe va)) ->
  (CompilationUnit -> [Immediate] -> Con.Error (Con.Exception vc) (Maybe vc)) ->
  (CompilationUnit -> [Immediate] -> Abs.Error (Abs.Exception va) (Maybe va)) ->
  Spec
jimpleSoundness
  evalImmediateCon evalImmediateAbs
  evalBoolCon      evalBoolAbs
  evalCon          evalAbs
  runStatementsCon runStatementsAbs
  runProgramCon    runProgramAbs = do
    soundImmediate 50 "Literals" emptyMem id evalImmediateCon evalImmediateAbs
    soundImmediate 10 "Localvar literal"
      toMem (\() -> Local "a")
      evalImmediateCon evalImmediateAbs

    soundBoolExpr 100 "Boolean expressions" emptyMem id evalBoolCon evalBoolAbs

    soundExpr 100 "Unary operations" emptyMem (uncurry UnopExpr) evalCon evalAbs
    soundExpr 100 "Binary operations" emptyMem (uncurry3 BinopExpr) evalCon evalAbs
    soundExpr 1000 "General expressions" emptyMem id evalCon evalAbs

    soundStatements 1000 "Single statements" toMem (:[]) runStatementsCon runStatementsAbs

    soundProgram 10 "Simple program"
      arrayFieldExampleFile
      (\x -> [IntConstant x])
      runProgramCon runProgramAbs

  where
    uncurry3 :: (a -> b -> c -> d) -> ((a,b,c) -> d)
    uncurry3 f (a,b,c) = f a b c
    emptyMem :: () -> [(String,vc)]
    emptyMem = const []
    toMem :: [vc] -> [(String,vc)]
    toMem = zip $ map show ['a'..]
