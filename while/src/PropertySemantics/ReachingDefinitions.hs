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
import           ValueSemantics.Unit

import           Data.Label
import qualified Data.List as L
import           Data.Ord (comparing)
import           Data.Order

import qualified Data.Abstract.Environment as E
import           Data.Abstract.Error
import qualified Data.Abstract.Store as S
import           Data.Abstract.Terminating

import           Control.Arrow.Fix 
import           Control.Arrow.Lift
import           Control.Arrow.Transformer.Abstract.ReachingDefinitions
import           Control.Arrow.Transformer.Abstract.LeastFixPoint

run :: [Statement] -> ReachingDefs Addr Label
    -> [(Statement,(ReachingDefs Addr Label, ReachingDefs Addr Label))]
run stmts defs =
  L.sortBy (comparing (label.fst)) $
  S.toList $
  S.map (\((_,(entry,ss)),v) ->
    case ss of
      (s:_) | s `elem` blocks stmts ->
         let exit = fst (snd (fromError (error "error")
                             (fromTerminating (error "non terminating") v)))
         in Just (s,(entry,exit))
      _ -> Nothing;
        ) $
  fst $
  runLeastFixPoint'
    (runInterp
       (runReachingDefs
        (Shared.run :: Fix [Statement] ()
                         (ReachingDefinitions Addr Label
                            (Interp
                               (~>))) [Statement] ())))
    ((S.empty,bottom),(E.empty,(defs,stmts)))


instance (IsVal val addr c) => IsVal val addr (ReachingDefinitions v l c) where
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
  freshAddr = lift freshAddr
  ref = lift ref
  getAddr = lift getAddr

instance (Conditional val (ReachingDefs v l,x) (ReachingDefs v l,y) (ReachingDefs v l,(ReachingDefs v l,z)) c)
  => Conditional val x y z (ReachingDefinitions v l c) where
  if_ (ReachingDefs f1) (ReachingDefs f2) =
    ReachingDefs $ proc (defs,(v,(x,y))) -> if_ f1 f2 -< (v,((defs,x),(defs,y)))
