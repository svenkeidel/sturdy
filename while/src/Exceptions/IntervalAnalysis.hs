{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-type-signatures #-}
-- | Interval Analysis for the While language.
module Exceptions.IntervalAnalysis where

import           Prelude hiding (Bool(..),Bounded(..),(/),fail)

import           IntervalAnalysis
import           Exceptions.Syntax
import           Exceptions.GenericInterpreter
import qualified Exceptions.GenericInterpreter as Generic

import           Data.Abstract.Except (Except(..))
import qualified Data.Abstract.Except as Exc
import           Data.Abstract.Error (Error(..))
import qualified Data.Abstract.Error as E
import qualified Data.Abstract.Interval as I
import qualified Data.Abstract.StrongMap as SM
import qualified Data.Abstract.Map as M
import           Data.Abstract.Map (Map)
import           Data.Abstract.Terminating (Terminating)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.DiscretePowerset (Pow)
import qualified Data.Abstract.Widening as W
import qualified Data.Abstract.StackWidening as SW
import qualified Data.Abstract.Maybe as AM

import qualified Data.Lens as L
import           Data.Order
import           Data.Label
import           Data.Text (Text)

import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Order
import qualified Control.Arrow.Trans as Trans

import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Arrow.Transformer.Abstract.Terminating
import qualified Control.Arrow.Transformer.Abstract.Fix.IterationStrategy as S

-- | Abstract values are either abstract booleans or intervals.
newtype Exception = Exception (Map Text Val) deriving (PreOrd,Complete)

-- | The interval analysis instantiates the generic interpreter
-- 'Generic.run' with the components for fixpoint computation
-- ('FixT'), termination ('TerminatingT'), failure ('ErrorT'), store
-- ('StoreT'), environments ('EnvT'), and values ('IntervalT').
run :: (?bound :: IV) => Int -> [(Text,Addr)] -> [LStatement]
    -> Terminating (Error (Pow String) (Except Exception (M.Map Addr Val)))
run k env ss =
  fmap (fmap fst) <$>
    Trans.run (Generic.run ::
      Fix [Statement] ()
        (IntervalT
          (EnvT Text Addr
            (StoreT Addr Val
              (ExceptT Exception
                (ErrorT (Pow String)
                  (TerminatingT
                    (FixT _ _
                      (S.StackWideningT _ _
                        (S.ChaoticT _ _
                          (->)))))))))) [Statement] ())
      iterationStrategy
      (M.empty,(SM.fromList env, generate (sequence ss)))

  where
    iterationStrategy = S.filter (L.second (L.second whileLoops))
                      $ S.stackWidening stackWiden 
                      $ S.chaotic widenResult

    stackWiden = SW.groupBy (L.iso (\(store,(ev,stmts)) -> (stmts,(ev,store)))
                                   (\(stmts,(ev,store)) -> (store,(ev,stmts))))
               $ SW.maxSize k
               $ SW.reuseFirst
               $ SW.fromWidening (SM.widening W.finite W.** M.widening widenVal)
               $ SW.finite

    widenVal = widening (W.bounded ?bound I.widening)
    widenExc (Exception m1) (Exception m2) = Exception <$> (M.widening widenVal m1 m2)
    widenResult = T.widening $ E.widening W.finite (Exc.widening widenExc (M.widening widenVal W.** W.finite))

instance (ArrowChoice c, ArrowComplete c) => IsException Exception Val (IntervalT c) where
  type JoinExc (IntervalT c) x y = Complete y
  namedException = proc (name,val) -> returnA -< Exception (M.singleton name val)
  matchException f g = proc (name,Exception m,x) -> case M.lookup name m of
    AM.Just v        -> f -< (v,x)
    AM.Nothing       -> g -< x
    AM.JustNothing v -> (f -< (v,x)) <⊔> (g -< x)
