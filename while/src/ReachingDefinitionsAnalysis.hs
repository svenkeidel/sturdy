{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-type-signatures #-}
-- | Reaching Definition Analysis for the While language.
module ReachingDefinitionsAnalysis where

import           Prelude hiding (pred)

import           Syntax
import qualified GenericInterpreter as Generic
import           IntervalAnalysis

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import           Data.Identifiable
import           Data.Label
import           Data.Lens (iso')
import qualified Data.List as L
import           Data.Maybe
import           Data.Order
import           Data.Text (Text)

import           Data.Abstract.DiscretePowerset(Pow)
import qualified Data.Abstract.Error as E
import qualified Data.Abstract.Except as Exc
import qualified Data.Abstract.Interval as I
import qualified Data.Abstract.Map as M
import qualified Data.Abstract.StrongMap as SM
import qualified Data.Abstract.Terminating as T
import qualified Data.Abstract.Widening as W
import           Data.Abstract.Stable
import           Data.Abstract.CallString(CallString)

import           Control.Arrow.Fix
import           Control.Arrow.Fix.Combinator as Fix
import           Control.Arrow.Trans as Trans
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.ReachingDefinitions
import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Chaotic
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Cache
import           Control.Arrow.Transformer.Abstract.Fix.Stack

type FixCache lab = Context (Proj2 (CtxCache (CallString lab))) (Group Cache)

-- | Calculates the entry sets of which definitions may be reached for
-- each statment.  The analysis instantiates the generic interpreter
-- 'Generic.run' with analysis components for fixpoint computation
-- ('FixT'), termination ('TerminatingT'), failure ('ErrorT'), store
-- ('StoreT'), environments ('EnvT'), and values ('IntervalT')
run :: (?bound :: I.Interval Int) => Int -> [LStatement] -> [(Label, M.Map Text (Pow Label))]
run k lstmts =
  L.sortOn fst $

  Map.toList $

  -- Joins the reaching definitions for each statement for all call context.
  -- Filters out statements created during execution that are not part
  -- of the input program.
  joinOnKey (\(st,(env,store)) _ -> case st of
    stmt:_ | stmt `elem` blocks stmts ->
      Just (label stmt, dropValues (combineMaps env store))
    _ -> Nothing) $

  -- get the fixpoint cache
  toMap $ fst $

  -- Run the computation
  Trans.run
    (Generic.run ::
      Fix'
       (ValueT Val
         (EnvT SM.Map Text Addr
           (ReachingDefsT
             (StoreT M.Map Addr (Val, Pow Label)
               (ExceptT Exception
                 (ErrorT (Pow String)
                   (TerminatingT
                     (FixT _ _
                       (ChaoticT _
                        (StackT Stack _
                          (CacheT (FixCache lab) (_,_) _
                            (ContextT (CallString _)
                               (->))))))))))))) [Statement] ())
    iterationStrategy
    (widenEnvStore, widenResult)
    (M.empty,(SM.empty,stmts))

  where
    stmts = generate (sequence lstmts)
    iterationStrategy = Fix.transform (iso' (\(store,(env,stmt)) -> (stmt,(env,store)))
                                            (\(stmt,(env,store)) -> (store,(env,stmt))))
                      $ Fix.reuseExact
                      . Fix.callsiteSensitive' @([Statement],(_,_)) k (statementLabel . fst)
                      . Fix.iterateInner

    statementLabel st = case st of (s:_) -> Just (label s); [] -> Nothing
    widenEnvStore = SM.widening W.finite W.** M.widening (widenVal W.** W.finite)
    widenVal = widening (I.bounded ?bound)
    widenExc (Exception m1) (Exception m2) = Exception <$> M.widening widenVal m1 m2
    widenResult = T.widening $ E.widening W.finite (Exc.widening widenExc (M.widening (widenVal W.** W.finite) W.** W.finite))

combineMaps :: (Identifiable k, Identifiable a) => SM.Map k a -> M.Map a v -> M.Map k v
combineMaps env store = M.fromList [ (a,c) | (a,b) <- fromJust (SM.toList env)
                                           , Just c <- [M.unsafeLookup b store]]

dropValues :: M.Map a (v,l) -> M.Map a l
dropValues = M.map snd

toMap :: (Identifiable k, Identifiable a) => FixCache lab (k,a) b -> HashMap (k,a) (Stable,b)
toMap (Context _ (Groups groups)) =
  Map.fromList [ ((k,a),(s,b)) | (k,Cache cache) <- Map.toList groups
                               , (a,(s,b)) <- Map.toList cache ]

joinOnKey :: (Identifiable k',Complete v') => (k -> v -> Maybe (k',v')) -> HashMap k v -> HashMap k' v'
joinOnKey pred = Map.foldlWithKey' (\m k v -> case pred k v of
                                              Just (k',v') -> Map.insertWith (⊔) k' v' m
                                              Nothing -> m
                                   ) Map.empty

instance HasLabel (x,[Statement]) where
  label (_,ss) = label (head ss)
