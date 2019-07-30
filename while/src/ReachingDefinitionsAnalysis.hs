{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-type-signatures #-}
-- | Reaching Definition Analysis for the While language.
module ReachingDefinitionsAnalysis where

import           Prelude hiding (pred)

import           Syntax
import qualified GenericInterpreter as Generic
import           IntervalAnalysis

import           Data.Coerce
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import           Data.Identifiable
import           Data.Label
import qualified Data.Lens as L
import qualified Data.List as L
import           Data.Maybe
import           Data.Order
import           Data.Text (Text)

import           Data.Abstract.Cache (Cache(..))
import           Data.Abstract.DiscretePowerset(Pow)
import qualified Data.Abstract.Error as E
import qualified Data.Abstract.Except as Exc
import qualified Data.Abstract.Interval as I
import qualified Data.Abstract.Map as M
import qualified Data.Abstract.StackWidening as SW
import qualified Data.Abstract.StrongMap as SM
import qualified Data.Abstract.Terminating as T
import qualified Data.Abstract.Widening as W

import           Control.Arrow.Fix
import           Control.Arrow.Trans as Trans
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.ReachingDefinitions
import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Fix
import qualified Control.Arrow.Transformer.Abstract.Fix.IterationStrategy as S

-- | Calculates the entry sets of which definitions may be reached for
-- each statment.  The analysis instantiates the generic interpreter
-- 'Generic.run' with analysis components for fixpoint computation
-- ('FixT'), termination ('TerminatingT'), failure ('ErrorT'), store
-- ('StoreT'), environments ('EnvT'), and values ('IntervalT')
run :: (?bound :: IV) => Int -> [LStatement] -> [(Label, M.Map Text (Pow Label))]
run k lstmts =
  L.sortOn fst $

  Map.toList $

  -- Joins the reaching definitions for each statement for all call context.
  -- Filters out statements created during execution that are not part
  -- of the input program.
  joinOnKey (\(store,(env,st)) _ -> case st of
    stmt:_ | stmt `elem` blocks stmts ->
      Just (label stmt, dropValues (combineMaps env store)) 
    _ -> Nothing) $

  -- get the fixpoint cache
  toMap $ fst $

  -- Run the computation
  Trans.run
    (Generic.run ::
      Fix [Statement] ()
       (ValueT Val
         (EnvT Text Addr
           (ReachingDefsT
             (StoreT Addr (Val, Pow Label)
               (ExceptT Exception
                 (ErrorT (Pow String)
                   (TerminatingT
                     (FixT _ _
                       (S.StackWideningT _ _
                         (S.ChaoticT Cache _ _
                           (->))))))))))) [Statement] ())
    iterationStrategy
    (M.empty,(SM.empty,stmts))

  where
                

    stmts = generate (sequence lstmts)

    iterationStrategy = S.stackWidening stackWiden
                      $ S.chaotic widenResult

    stackWiden = SW.groupBy (L.iso (\(store,(ev,sts)) -> (sts,(ev,store)))
                                   (\(sts,(ev,store)) -> (store,(ev,sts))))
               $ SW.maxSize k
               $ SW.reuseFirst
               $ SW.fromWidening (SM.widening W.finite W.** M.widening (widenVal W.** W.finite))
               $ SW.finite
    
    widenVal = widening (W.bounded ?bound I.widening)
    widenExc (Exception m1) (Exception m2) = Exception <$> (M.widening widenVal m1 m2)
    widenResult = T.widening $ E.widening W.finite (Exc.widening widenExc (M.widening (widenVal W.** W.finite) W.** W.finite))

combineMaps :: (Identifiable k, Identifiable a) => SM.Map k a -> M.Map a v -> M.Map k v
combineMaps env store = M.fromList [ (a,c) | (a,b) <- fromJust (SM.toList env)
                                           , Just c <- [M.unsafeLookup b store]]

dropValues :: M.Map a (v,l) -> M.Map a l
dropValues = M.map snd

toMap :: Cache a b -> HashMap a (W.Stable,b)
toMap = coerce

joinOnKey :: (Identifiable k',Complete v') => (k -> v -> Maybe (k',v')) -> HashMap k v -> HashMap k' v'
joinOnKey pred = Map.foldlWithKey' (\m k v -> case pred k v of
                                              Just (k',v') -> Map.insertWith (⊔) k' v' m
                                              Nothing -> m
                                   ) Map.empty

instance HasLabel (x,[Statement]) Label where
  label (_,ss) = label (head ss)
