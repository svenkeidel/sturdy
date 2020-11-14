{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
-- | This file instantiates the generic interpreter with abstract
-- values to implement the abstract interpreter.  All we have to do is
-- to implement the `IsValue` interface, everything else is provided
-- by the sturdy standard library.
module SturdyStyle.AbstractInterpreter where

import           Prelude hiding (Bool(..),Bounded(..),(/),fail,(.),filter,id)
import qualified Prelude as P

import           Syntax
import           SturdyStyle.GenericInterpreter(IsValue, ArrowAlloc)
import qualified SturdyStyle.GenericInterpreter as Generic

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fail as Fail
import           Control.Arrow.Fix
import           Control.Arrow.Fix.CallCount(unroll)
import           Control.Arrow.Fix.Chaotic(innermost)
import           Control.Arrow.Order
import qualified Control.Arrow.Trans as Trans

import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.CallCount
import           Control.Arrow.Transformer.Abstract.Fix.Component
import           Control.Arrow.Transformer.Abstract.Fix.Context as Ctx
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable
import           Control.Arrow.Transformer.Abstract.Fix.Stack
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Arrow.Transformer.Abstract.Terminating

import           Data.Profunctor
import qualified Data.Boolean as B
import           Data.Hashable
import           Data.Order
import           Data.Label
import qualified Data.Lens as L
import           Data.Text.Prettyprint.Doc

import           Data.Abstract.Boolean (Bool)
import qualified Data.Abstract.Boolean as B
import           Data.Abstract.DiscretePowerset (Pow)
import           Data.Abstract.Error (Error(..))
import qualified Data.Abstract.Error as E
import           Data.Abstract.FreeCompletion(FreeCompletion(..))
import           Data.Abstract.InfiniteNumbers
import           Data.Abstract.Interval (Interval)
import qualified Data.Abstract.Interval as I
import           Data.Abstract.Map (Map)
import qualified Data.Abstract.Map as M
import qualified Data.Abstract.Ordering as O
import qualified Data.Abstract.StrongMap as SM
import           Data.Abstract.Terminating (Terminating)
import qualified Data.Abstract.Terminating as T
import           Data.Abstract.There(There(..))
import           Data.Abstract.Stable
import           Data.Abstract.Widening (Widening)
import qualified Data.Abstract.Widening as W

import           GHC.Exts(IsString(..))
import           GHC.Generics



type Addr = FreeCompletion Label
data AbsVal = BoolVal Bool | NumVal IV | TopVal deriving (Show, Eq,Generic)
type IV = Interval (InfiniteNumber Integer)
type Env = SM.Map String Addr
type Store = Map Addr AbsVal
type Errors = Pow String

type In = ((Env,[Statement]),Store)
type Out = Terminating (Error Errors (Store, ()))

instance (IsString e, ArrowChoice c, ArrowFail e c, Fail.Join AbsVal c,
          ArrowComplete AbsVal c)
         => IsValue AbsVal (ValueT AbsVal c) where

  type JoinVal z (ValueT AbsVal c) =
    (ArrowComplete z (ValueT AbsVal c), Fail.Join z c)

  numLit = proc x -> returnA -< NumVal (I.Interval (Number $ toInteger x) (Number $ toInteger x))

  add = proc (v1,v2) -> case (v1,v2) of
    -- When adding all numbers within two intervals, the smallest
    -- number that can occur is the addition of the lower interval
    -- bounds and the largest number that can occur is the addition of
    -- the upper interval bounds.
    (NumVal n1,NumVal n2) ->
      returnA -< NumVal (n1 + n2)

    (TopVal, TopVal) ->
      (returnA -< NumVal top)
        <⊔> (fail -< "Expected two numbers as arguments for 'add'")
    (TopVal, NumVal _) ->
      (returnA -< NumVal top)
        <⊔> (fail -< "Expected two numbers as arguments for 'add'")

    (NumVal _, TopVal) ->
      (returnA -< NumVal top)
        <⊔> (fail -< "Expected two numbers as arguments for 'add'")

    _ ->
      fail -< "Expected two numbers as arguments for 'add'"

  lt = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1, NumVal n2) -> returnA -< BoolVal (n1 O.< n2)
    (TopVal,TopVal)        -> (returnA -< BoolVal top) <⊔> (fail -< "Expected two numbers as arguments for 'lt'")
    (TopVal, NumVal _)     -> (returnA -< BoolVal top) <⊔> (fail -< "Expected two numbers as arguments for 'lt'")
    (NumVal _, TopVal)     -> (returnA -< BoolVal top) <⊔> (fail -< "Expected two numbers as arguments for 'lt'")
    _                      -> fail -< "Expected two numbers as arguments for 'lt'"

  boolLit = arr $ \b -> case b of
    P.True -> BoolVal B.True
    P.False -> BoolVal B.False

  and = proc (v1,v2) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 `B.and` b2)
    (TopVal, TopVal)        -> (returnA -< BoolVal top) <⊔> (fail -< "Expected two booleans as arguments for 'and'")
    (TopVal, BoolVal _)     -> (returnA -< BoolVal top) <⊔> (fail -< "Expected two booleans as arguments for 'and'")
    (BoolVal _, TopVal)     -> (returnA -< BoolVal top) <⊔> (fail -< "Expected two booleans as arguments for 'and'")
    _                       -> fail -< "Expected two booleans as arguments for 'and'"

  if_ f g = proc (cond,(s1,s2)) -> case cond of
    BoolVal B.True -> f -< s1
    BoolVal B.False -> g -< s2

    -- If the condition could evaluate to true or false, we have to
    -- evaluate both branches and join the results.
    BoolVal B.Top -> (f -< s1) <⊔> (g -< s2)
    NumVal _ -> fail -< "Expected a boolean expression as condition for an if"
    TopVal -> (f -< s1) <⊔> (g -< s2) <⊔> (fail -< "Expected a boolean expression as condition for an if")

instance (Profunctor c, ArrowChoice c) => ArrowAlloc Addr (ValueT AbsVal c) where
  alloc = proc (_,_,l) -> returnA -< return l

-- | The interval analysis instantiates the generic interpreter
-- 'Generic.run' with the components for fixpoint computation
-- ('FixT'), termination ('TerminatingT'), failure ('ErrorT'), store
-- ('StoreT'), environments ('EnvT'), and values ('IntervalT').
run :: (?bound :: IV) => Int -> Env -> Store -> [Statement] -> Terminating (Error (Pow String) Store)
run k initEnv initStore prog =
  let ?contextWidening = M.widening widenVal in
  let ?cacheWidening = widenResult in
  let ?fixpointAlgorithm =
        transform (L.iso' (\(store,(env,stmts)) -> ((env,stmts),store))
                          (\((env,stmts),store) -> (store,(env,stmts))))
                  (L.iso' id id) $
          fixpointAlgorithm $
            filter isWhileLoop
              -- $ callsiteSensitive' k (\((_,stmts),_) -> case stmts of (stmt:_) -> Just (label stmt); [] -> Nothing)
              $ unroll k (\((_,stmts),_) -> case stmts of (stmt:_) -> label stmt; [] -> -1)
              . innermost
  in

  fmap (fmap fst) <$> snd $
  Trans.run
    (Generic.run ::
      (ValueT AbsVal
        (EnvT Env
          (StoreT Store
              (ErrorT Errors
                (TerminatingT
                  (FixT
                    (ComponentT Component In
                      (StackT Stack In
                        (CacheT Cache In Out
                          (CallCountT Label
                            (ContextT (Ctx.Second Context) Label In
                             (->)))))))))))) [Statement] ())
      (initStore,(initEnv, prog))
  where
    widenVal = widening (I.bounded ?bound)
    widenResult = T.widening $ E.widening W.finite (M.widening widenVal W.** W.finite)

runWithInitVals :: (?bound :: IV) => Int -> [(String,AbsVal)] -> [LStatement]
                     -> (Terminating (Error (Pow String) Store), Env)
runWithInitVals k initVals stmts =
  (run k initEnv initStore prog, initEnv)
  where
    (prog, start) = generateState (sequence stmts)
    labels = map Lower [start..]
    (strings,vals) = unzip initVals
    initEnv = SM.fromList $ zipWith (,) strings labels
    initStore = M.fromThereList $ zipWith (,) labels (map (\v -> (Must, v)) vals)


-- Orderings ------------------------------------------------------------
-- The ordering on abstract values defines which values are more
-- precise than others.
instance PreOrd AbsVal where
  NumVal i1 ⊑ NumVal i2 = i1 ⊑ i2
  BoolVal b1 ⊑ BoolVal b2 = b1 ⊑ b2
  _ ⊑ TopVal = P.True
  _ ⊑ _ = P.False

-- The least upper bound v1 ⊔ v2 calculates the smallest value that is
-- greater than v1 and v2.
instance Complete AbsVal where    
  (⊔) = W.toJoin1 widening (⊔)
--  NumVal i1 ⊔ NumVal i2 = NumVal (i1 ⊔ i2)
--  BoolVal b1 ⊔ BoolVal b2 = BoolVal (b1 ⊔ b2)
--  _ ⊔ _ = TopVal

widening :: Widening IV -> Widening AbsVal
widening w v1 v2 = case (v1,v2) of
  (BoolVal b1,BoolVal b2) -> second BoolVal (B.widening b1 b2)
  (NumVal n1,NumVal n2) -> second NumVal (n1 `w` n2)
  (NumVal _,BoolVal _) -> (Unstable, TopVal)
  (BoolVal _,NumVal _) -> (Unstable, TopVal) 
  (TopVal,TopVal) -> (Stable,TopVal)
  (_,TopVal) -> (Unstable,TopVal)
  (TopVal,_) -> (Unstable,TopVal)


-- Arrow Instances ------------------------------------------------------ 
deriving instance ArrowComplete () c => ArrowComplete () (ValueT AbsVal c)
deriving instance ArrowComplete AbsVal c => ArrowComplete AbsVal (ValueT AbsVal c)

instance Hashable AbsVal

instance Pretty AbsVal where
  pretty = viaShow

whileLoops :: L.Prism' [Statement] ((Expr,[Statement],Label),[Statement])
whileLoops = L.prism' (\((c,b,l),ss) -> While c b l:ss)
                (\s -> case s of
                   While c b l:ss -> Just ((c,b,l),ss)
                   _ -> Nothing)
