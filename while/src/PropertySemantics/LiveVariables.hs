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

import           Data.Abstract.Terminating
import           Data.Abstract.Error (Error)
import           Data.Abstract.Store (Store)
import qualified Data.Abstract.Store as S

import           Control.Arrow
import           Control.Arrow.Fix 
import           Control.Arrow.Lift
import           Control.Arrow.Transformer.Writer
import           Control.Arrow.Transformer.Abstract.LiveVariables (LiveVarsTrans, LiveVariables(..),runLiveVariables)
import qualified Control.Arrow.Transformer.Abstract.LiveVariables as L
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Monad.State(State)

run :: (?bound :: IV) => [State Label Statement] -> Store (Store Text Val, [Statement]) (Terminating (Error String (Store Text Val, L.LiveVarsTrans Text)))
run ss = fmap (fmap (fmap (second fst))) $ fst $
  runFix'
    (runInterp ?bound
       (runLiveVariables (Shared.run :: Fix [Statement] () (LiveVariables Text (Interp (~>))) [Statement] ())))
    (S.empty,generate (sequence ss))

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

