{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module PropertySemantics.ReachingDefinitions where

import           Prelude hiding (and,or,not,div)

import           Syntax
import           SharedSemantics
import qualified SharedSemantics as Shared
import           ValueSemantics.ControlFlow

import           Data.Text (Text)
import           Data.Label
import qualified Data.List as L
import           Data.Ord (comparing)

import           Data.Abstract.Terminating
import           Data.Abstract.Error
import qualified Data.Abstract.Store as S

import           Control.Arrow.Fix 
import           Control.Arrow.Lift
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Writer
import           Control.Arrow.Transformer.Abstract.ReachingDefinitions
import           Control.Arrow.Transformer.Abstract.Fix

run :: [Statement] -> ReachingDefs Text Label -> [(Statement,(ReachingDefs Text Label,ReachingDefs Text Label))]
run stmts defs =
  L.sortBy (comparing (label.fst)) $
  S.toList $
  S.map (\((_,(entry,ss)),v) ->
    case ss of
      (s:_) | s `elem` blocks stmts ->
         let exit = fst (snd (fromError (error "error") (fromTerminating (error "non terminating") v)))
         in Just (s,(entry,exit))
      _ -> Nothing;
        ) $
  fst $
  runFix'
    (runInterp
       (runReachingDefinitions
        (Shared.run :: Fix [Statement] () (ReachingDefinitions Text Label (Interp (~>))) [Statement] ())))
    (S.empty,(defs,stmts))


instance (IsVal val c) => IsVal val (ReachingDefinitions v l c) where
  boolLit = lift boolLit
  and = lift and
  or = lift or
  not = lift not
  numLit = lift numLit
  randomNum = lift randomNum
  add = lift add
  sub = lift sub
  mul = lift mul
  div = lift div
  eq = lift eq
  lt = lift lt

instance (Conditional val (ReachingDefs v l,x) (ReachingDefs v l,y) (ReachingDefs v l,(ReachingDefs v l,z)) c)
  => Conditional val x y z (ReachingDefinitions v l c) where
  if_ (ReachingDefinitions (State (Writer f1))) (ReachingDefinitions (State (Writer f2))) =
    ReachingDefinitions $ State $ Writer $ proc (defs,(v,(x,y))) -> if_ f1 f2 -< (v,((defs,x),(defs,y)))
