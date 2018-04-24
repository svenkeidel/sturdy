{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module PropertySemantics.LiveVariables where

import           Prelude hiding (and,or,not,div)

import           Syntax
import           SharedSemantics
import qualified SharedSemantics as Shared
import           ValueSemantics.Interval

import           Data.Text (Text)
import           Data.Label
import           Data.Identifiable
import qualified Data.List as L
import           Data.Ord

import           Data.Abstract.Terminating
import           Data.Abstract.Error
import qualified Data.Abstract.Store as S

import           Control.Arrow.Fix 
import           Control.Arrow.Lift
import           Control.Arrow.Transformer.Writer
import           Control.Arrow.Transformer.Abstract.LiveVariables
import qualified Control.Arrow.Transformer.Abstract.LiveVariables as L
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Monad.State(State)

run :: (?bound :: IV) => [State Label Statement] -> [(Statement,(LiveVars Text,LiveVars Text))]
run statements =
  L.sortBy (comparing (label.fst)) $
  S.toList $
  S.map (\((_,ss),v) ->
    case ss of
      [] -> Nothing;
      (s:_) ->
         let trans = fst (snd (fromError (error "error") (fromTerminating (error "non terminating") v)))
         in Just (s,(L.entry trans, L.exit trans))) $
  fst $
  runFix'
    (runInterp ?bound
       (runLiveVariables (Shared.run :: Fix [Statement] () (LiveVariables Text (Interp (~>))) [Statement] ())))
    (S.empty,generate (sequence statements))


instance (Identifiable v, IsVal val c) => IsVal val (LiveVariables v c) where
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

instance (Identifiable v, Conditional val x y (LiveVarsTrans v,z) c) => Conditional val x y z (LiveVariables v c) where
  if_ (LiveVariables (Writer f1)) (LiveVariables (Writer f2)) = LiveVariables $ Writer $ proc (v,(x,y)) -> if_ f1 f2 -< (v,(x,y))

