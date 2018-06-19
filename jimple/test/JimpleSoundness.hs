{-# LANGUAGE FlexibleContexts #-}
module JimpleSoundness where

import Soundness
import Syntax

import Data.Exception
import Data.GaloisConnection
import Data.Hashable
import Data.Order

import Data.Concrete.Powerset as Con
import Data.Concrete.Error as Con
import Data.Abstract.HandleError as Abs

import Test.Hspec

jimpleSoundness :: (Galois (Con.Pow vc) va,Complete va,Eq vc,Hashable vc,Show vc,Show va,
                    Galois (Con.Pow bc) ba,Complete ba,Eq bc,Hashable bc,Show bc,Show ba) =>
  ([(String,vc)] -> Immediate -> Con.Error (Exception vc) vc) ->
  ([(String,va)] -> Immediate -> Abs.Error (Exception va) va) ->
  ([(String,vc)] -> BoolExpr -> Con.Error (Exception vc) bc) ->
  ([(String,va)] -> BoolExpr -> Abs.Error (Exception va) ba) ->
  ([(String,vc)] -> Expr -> Con.Error (Exception vc) vc) ->
  ([(String,va)] -> Expr -> Abs.Error (Exception va) va) ->
  ([(String,vc)] -> [Statement] -> Con.Error (Exception vc) (Maybe vc)) ->
  ([(String,va)] -> [Statement] -> Abs.Error (Exception va) (Maybe va)) ->
  ([CompilationUnit] -> (MethodSignature,[Immediate]) -> Con.Error (Exception vc) (Maybe vc)) ->
  ([CompilationUnit] -> (MethodSignature,[Immediate]) -> Abs.Error (Exception va) (Maybe va)) ->
  Spec
jimpleSoundness
  evalImmediateCon evalImmediateAbs
  evalBoolCon      evalBoolAbs
  evalCon          evalAbs
  runStatementsCon runStatementsAbs
  runProgramCon    runProgramAbs = do
    soundImmediate "Integer literal" [] IntConstant evalImmediateCon evalImmediateAbs

    soundExpr "x+y" [] (\(x,y) ->
      BinopExpr (IntConstant x) Plus (IntConstant y)
      ) evalCon evalAbs
