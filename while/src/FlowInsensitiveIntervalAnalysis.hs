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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-type-signatures #-}
-- | Interval Analysis for the While language.
module FlowInsensitiveIntervalAnalysis where

import           Prelude hiding (Bool(..),Bounded(..),(/),fail,(.))

import           Syntax
import qualified GenericInterpreter as Generic
import           IntervalAnalysis(Val(..),Exception(..),Addr,IV,widening)

import           Control.Arrow.Fix
import qualified Control.Arrow.Trans as Trans

import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Fix
import qualified Control.Arrow.Transformer.Abstract.Fix.IterationStrategy as S

import           Control.Arrow.Transformer.Abstract.FlowInsensitive.Store hiding (Store)
import           Control.Arrow.Transformer.Abstract.FlowInsensitive.Except
import           Control.Arrow.Transformer.Abstract.FlowInsensitive.Error

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Identifiable
import           Data.Label
import qualified Data.Lens as L
import           Data.Text (Text)
import           Data.Utils
import           Data.Coerce
import           Data.OrderMonoid

import           Data.Abstract.Cache (Cache)
import           Data.Abstract.DiscretePowerset (Pow)
import qualified Data.Abstract.Interval as I
import qualified Data.Abstract.Map as AM
import qualified Data.Abstract.StackWidening as SW
import qualified Data.Abstract.StrongMap as SM
import           Data.Abstract.Widening (Widening)
import qualified Data.Abstract.Widening as W

run :: (?bound :: IV) => Int -> [(Text,Addr)] -> [LStatement] -> HashMap Addr Val
run k env ss = snd $ snd $ run' k env ss

 -- cache0 (Store, (Env, ((Expr, Statement, Label), [Statement])))
 --        (Pow String, (Exception, (Store, ())))

type Errors = Pow String
type Store = HashMap Addr Val

run' :: (?bound :: IV) => Int -> [(Text,Addr)] -> [LStatement] -> (Errors, (Exception, Store))
run' k env ss = coerce $ fmap (fmap fst) <$> snd $
  Trans.run
    (Generic.run ::
      Fix [Statement] ()
        (ValueT Val
          (EnvT Text Addr
            (StoreT Addr Val
              (ExceptT Exception
                (ErrorT (Pow String)
                  (FixT _ _
                    (S.StackWideningT _ _
                      (S.ParallelT Cache _ _
                        (->))))))))) [Statement] ())
      iterationStrategy
      (empty,(SM.fromList env, generate (sequence ss)))

  where
    iterationStrategy = S.trace
                      $ S.filter (L.second (L.second whileLoops))
                      $ S.stackWidening stackWiden 
                      $ S.parallel (W.finite W.** (coerce widenExc W.** (widenStore W.** W.finite)))

    stackWiden = SW.groupBy (L.iso (\(store,(ev,stmts)) -> (stmts,(ev,store)))
                                   (\(stmts,(ev,store)) -> (store,(ev,stmts))))
               $ SW.maxSize k
               $ SW.reuseFirst
               $ SW.fromWidening (SM.widening W.finite W.** widenMap widenVal)
               $ SW.finite
    
    widenVal = widening (W.bounded ?bound I.widening)
    widenExc (Exception m1) (Exception m2) = Exception <$> (AM.widening widenVal m1 m2)
    widenStore = widenMap widenVal
    widenMap :: Identifiable a => Widening b -> Widening (HashMap a b)
    widenMap widen m1 m2 = sequenceA (M.intersectionWith widen m1 m2 <> M.difference (M.map (W.Instable,) m2) m1)
