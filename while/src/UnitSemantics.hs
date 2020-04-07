{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-type-signatures #-}
module UnitSemantics where

import           Prelude hiding (Bool(..),Bounded(..),(.),filter,id)

import           Syntax
import           GenericInterpreter
import qualified GenericInterpreter as Generic

import           Data.Label
import           Data.Text (Text)
import           Data.Profunctor
import qualified Data.Lens as L

import           Data.Abstract.Error (Error(..))
import           Data.Abstract.Except (Except(..))
import qualified Data.Abstract.Map as M
import qualified Data.Abstract.StrongMap as SM
import           Data.Abstract.Terminating
import           Data.Abstract.FreeCompletion(FreeCompletion)
import           Data.Abstract.DiscretePowerset(Pow)
import qualified Data.Abstract.Widening as W

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Fix.Chaotic
import           Control.Arrow.Random
import           Control.Arrow.Order
import           Control.Arrow.Trans as Trans

import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Component
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable
import           Control.Arrow.Transformer.Abstract.Fix.Stack
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Arrow.Transformer.Abstract.Terminating

-- Value semantics for the while language that does not approximate values at all.
type Addr = FreeCompletion Label
type Env = SM.Map Text Addr
type Store = M.Map Addr Val
type Val = ()
type Exception = ()

run :: [(Text,Addr)] -> [LStatement] -> Terminating (Error (Pow String) (Except Exception (M.Map Addr Val)))
run env0 ss =
  let ?cacheWidening = W.finite in
  let ?fixpointAlgorithm =
        transform (L.iso' (\(store,(env,stmts)) -> ((env,stmts),store))
                          (\((env,stmts),store) -> (store,(env,stmts))))
                  (L.iso' id id) $
        fixpointAlgorithm $ filter isWhileLoop $ chaotic innermost in

  fmap (fmap (fmap fst)) <$>
    snd $
    Trans.run
      (Generic.run ::
        (ValueT Val
          (EnvT Env
            (StoreT Store
              (ExceptT Exception
                (ErrorT (Pow String)
                  (TerminatingT
                    (FixT
                      (ComponentT Component _
                        (StackT Stack _
                          (CacheT Cache _ _
                             (->))))))))))) [Statement] ())
      (M.empty,(SM.fromList env0,generate <$> ss))

instance (ArrowChoice c,Profunctor c) => ArrowAlloc Addr (ValueT Val c) where
  alloc = arr $ \(_,_,l) -> return l

instance (ArrowChoice c, ArrowComplete Val c) => IsVal Val (ValueT Val c) where
  type JoinVal y (ValueT Val c) = ArrowComplete y c
  boolLit = arr (const ())
  and = arr (const ())
  or = arr (const ())
  not = arr (const ())
  numLit = arr (const ())
  add = arr (const ())
  sub = arr (const ())
  mul = arr (const ())
  div = arr (const ())
  eq = arr (const ())
  lt = arr (const ())
  if_ f1 f2 = proc (_,(x,y)) -> (f1 -< x) <⊔> (f2 -< y)

instance (ArrowChoice c) => IsException Exception Val (ValueT Val c) where
  type JoinExc y (ValueT Val c) = ArrowComplete y c
  namedException = proc (_,_) -> returnA -< ()
  matchException f g = proc (_,(),x) -> (f -< ((),x)) <⊔> (g -< x)

instance (ArrowChoice c,Profunctor c) => ArrowRand Val (ValueT Val c) where
  random = arr (const ())

deriving instance (ArrowComplete y c) => ArrowComplete y (ValueT Val c)
