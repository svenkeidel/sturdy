{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module PropertySemantics.ReachingDefinitions where

import           Syntax
import           SharedSemantics
import qualified SharedSemantics as Shared
import           ValueSemantics.Abstract
import           ValueSemantics.Unit

import           Data.Text (Text)
import           Data.Label
import qualified Data.List as L
import           Data.Ord (comparing)

import qualified Data.Abstract.Environment as E
import           Data.Abstract.Store (Store)
import qualified Data.Abstract.Store as S
import           Data.Abstract.DiscretePowerset(Pow)

import           Control.Arrow.Fix 
import           Control.Arrow.Transformer.Abstract.ReachingDefinitions
import           Control.Arrow.Transformer.Abstract.LeastFixPoint

-- | Calculates the entry sets of which definitions may be reached for each statment.
run :: [Statement] -> [(Statement,Store Text (Pow Label))]
run stmts =
  L.sortBy (comparing (label . fst)) $
  S.toList $

  -- Joins the reaching definitions for each statement for all call context.
  -- Filters out statements created during execution that are not part
  -- of the input program.
  S.map (\((store,(env,st)),_) ->
    case st of
      stmt:_ | stmt `elem` blocks stmts -> Just (stmt, S.compose (E.toList env) (S.mapValues snd store))
      _ -> Nothing) $
  
  -- get the fixpoint cache
  fst $

  -- Run the computation
  runLeastFixPoint'
    (runInterp
       (runReachingDefs
        (Shared.run :: Fix [Statement] ()
                         (ReachingDefinitions
                           (Interp Addr (Val,Pow Label)
                             (~>))) [Statement] ())))
    (S.empty,(E.empty,stmts))

deriving instance IsVal val c => IsVal val (ReachingDefinitions c)
