{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-type-signatures #-}
module PropertySemantics.ReachingDefinitions where

import           Syntax
import qualified GenericInterpreter as Generic
import           ValueSemantics.Unit

import           Data.Text (Text)
import           Data.Label
import qualified Data.List as L

import           Data.Abstract.Map (Map)
import qualified Data.Abstract.Map as M
import           Data.Abstract.DiscretePowerset(Pow)
import qualified Data.Abstract.StackWidening as SW
import qualified Data.Abstract.Widening as W

import           Control.Arrow.Fix 
import           Control.Arrow.Transformer.Abstract.ReachingDefinitions
import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Arrow.Transformer.Abstract.Failure
import           Control.Arrow.Transformer.Abstract.Fixpoint

-- | Calculates the entry sets of which definitions may be reached for each statment.
run :: [Statement] -> [(Statement,Map Text (Pow Label))]
run stmts =
  L.sortOn ((label :: Statement -> Label) . fst) $
  M.toList $

  -- Joins the reaching definitions for each statement for all call context.
  -- Filters out statements created during execution that are not part
  -- of the input program.
  M.mapMaybe (\((store,(env,st)),_) -> case st of
     stmt:_ | stmt `elem` blocks stmts -> Just (stmt, M.compose (M.toList env) (M.map snd store))
     _ -> Nothing) $
  
  -- get the fixpoint cache
  fst $

  -- Run the computation
  runFixT'' SW.finite W.finite
    (runFailureT
      (runStoreT
        (runReachingDefsT'
          (runEnvT
            (runUnitT
              (Generic.run ::
                Fix [Statement] ()
                 (UnitT
                   (EnvT Text Addr
                     (ReachingDefsT Label
                       (StoreT Addr (Val, Pow Label)
                         (FailureT String
                           (FixT _ () () (->))))))) [Statement] ()))))))
    (M.empty,(M.empty,stmts))

instance HasLabel (x,[Statement]) Label where
  label (_,ss) = label (head ss)
-- deriving instance IsVal val c => IsVal val (ReachingDefsT Label c)
