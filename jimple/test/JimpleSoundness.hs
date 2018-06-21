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
  ([CompilationUnit] -> (MethodSignature,[Immediate]) -> Con.Error (Con.Exception vc) (Maybe vc)) ->
  ([CompilationUnit] -> (MethodSignature,[Immediate]) -> Abs.Error (Abs.Exception va) (Maybe va)) ->
  Spec
jimpleSoundness
  evalImmediateCon evalImmediateAbs
  evalBoolCon      evalBoolAbs
  evalCon          evalAbs
  runStatementsCon runStatementsAbs
  runProgramCon    runProgramAbs = do
    soundImmediate "Literals" emptyMem id evalImmediateCon evalImmediateAbs
    soundImmediate "Localvar literal"
      toMem
      (\() -> Local "a")
      evalImmediateCon evalImmediateAbs

    soundBoolExpr "Boolean expressions" emptyMem id evalBoolCon evalBoolAbs

    soundExpr "Unary operations" emptyMem (uncurry UnopExpr) evalCon evalAbs
    soundExpr "Binary operations" emptyMem (\(x,op,y) ->
        BinopExpr (IntConstant x) op (IntConstant y)
      ) evalCon evalAbs

    soundExpr "General expressions" emptyMem id evalCon evalAbs

  where
    emptyMem :: () -> [(String,vc)]
    emptyMem = const []
    toMem :: [vc] -> [(String,vc)]
    toMem = zip $ map show ['a'..]
