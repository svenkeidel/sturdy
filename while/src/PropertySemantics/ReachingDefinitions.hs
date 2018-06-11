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

import           Data.Text (Text)
import           Data.Label
import qualified Data.List as L
import           Data.Ord (comparing)

import qualified Data.Abstract.Environment as E
import qualified Data.Abstract.Store as S

import           Control.Arrow
import           Control.Arrow.Fix 
import           Control.Arrow.Lift
import           Control.Arrow.Alloc
import           Control.Arrow.Transformer.Abstract.ReachingDefinitions
import           Control.Arrow.Transformer.Abstract.LeastFixPoint

-- | Calculates the entry sets of which definitions may be reached for each statment.
run :: [Statement] -> ReachingDefs Text Label -> [(Statement,ReachingDefs Text Label)]
run stmts defs =
  L.sortBy (comparing (label . fst)) $
  S.toList $

  -- Joins the reaching definitions for each statement for all call context.
  -- Filters out statements created during execution that are not part
  -- of the input program.
  S.map (\((_,(env,(entry,st))),_) ->
    case st of
      stmt:_ | stmt `elem` blocks stmts -> Just (stmt, S.compose (E.toList env) entry)
      _ -> Nothing) $
  
  -- get the fixpoint cache
  fst $

  -- Run the computation
  runLeastFixPoint'
    (runInterp
       (runReachingDefs
        (Shared.run :: Fix [Statement] ()
                         (ReachingDefinitions Addr Label
                           (Interp
                             (~>))) [Statement] ())))
    (S.empty,(E.empty,(S.empty,stmts)))

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

instance ArrowChoice c => ArrowAlloc (Text,Val,Label) Addr (ReachingDefinitions v l (Interp c)) where
  alloc = lift alloc

instance (Conditional val (ReachingDefs v l,x) (ReachingDefs v l,y) (ReachingDefs v l,z) c)
  => Conditional val x y z (ReachingDefinitions v l c) where
  if_ f1 f2 =
    reachingDefs $ proc (defs,(v,(x,y))) -> if_ (runReachingDefs f1) (runReachingDefs f2) -< (v,((defs,x),(defs,y)))

