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
module SturdyStyle.AbstractInterpreter where

import           Prelude hiding (Bool(..),lookup,and,fail)
import qualified Prelude as P

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Environment
import           Control.Arrow.Store
import           Control.Arrow.Fail
import           Control.Arrow.Alloc
import           Control.Arrow.Abstract.Join
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Fix

import           Data.Abstract.FreeCompletion (FreeCompletion)
import           Data.Abstract.Error (Error)
import qualified Data.Abstract.Error as E
import           Data.Abstract.Terminating (Terminating)
import qualified Data.Abstract.StrongMap as SM
import qualified Data.Abstract.Map as M
import           Data.Profunctor
import           Data.Label
import           Data.Hashable
import           Data.Abstract.DiscretePowerset(Pow)
import qualified Data.Lens as L
import           Data.Order
import           Data.Abstract.Widening(Widening)
import qualified Data.Abstract.Widening as W
import qualified Data.Abstract.Terminating as T
import qualified Data.Abstract.StackWidening as S

import           GHC.Exts
import           GHC.Generics

import           SturdyStyle.GenericInterpreter(IsValue(..))
import qualified SturdyStyle.GenericInterpreter as Generic
import           Syntax


type Addr = FreeCompletion Label
data AbsVal = BoolVal AbsBool | NumVal Interval | TopVal deriving (Eq,Generic)
data AbsBool = True | False | TopBool deriving (Eq,Generic)
data Interval = Interval Int Int deriving (Eq,Generic)

run :: [LStatement] -> Terminating (Error (Pow String) (M.Map Addr AbsVal))
run stmts = fmap fst <$>
  runFixT stackWiden widenResult
    (runTerminatingT
      (runErrorT
        (runStoreT
          (runEnvT
            (runAbstractT
              (Generic.run ::
                Fix [Statement] ()
                  (AbstractT
                    (EnvT String Addr
                      (StoreT Addr AbsVal
                        (ErrorT (Pow String)
                          (TerminatingT
                            (FixT _ () () (->))))))) [Statement] ()))))))
      (M.empty,(SM.empty,generate <$> stmts))
  where

    widenResult = T.widening $ E.widening W.finite (M.widening widenVal W.** W.finite)

    stackWiden = S.filter' (L.second (L.second whileLoops))
               $ S.groupBy (L.iso (\(store,(ev,stmt)) -> (stmt,(ev,store))) (\(stmt,(ev,store)) -> (store,(ev,stmt))))
               $ S.stack
               $ S.maxSize 1
               $ S.reuseFirst
               $ S.fromWidening (SM.widening W.finite W.** M.widening widenVal)

newtype AbstractT c x y = AbstractT { runAbstractT :: c x y }
  deriving (Category,Profunctor,Arrow,ArrowChoice,ArrowJoin,ArrowFail e,ArrowEnv var addr env,ArrowStore addr val)
deriving instance ArrowFix x y c => ArrowFix x y (AbstractT c)
type instance Fix x y (AbstractT c) = AbstractT (Fix x y c)

instance (Profunctor c, Arrow c) => ArrowAlloc (String, AbsVal, Label) Addr (AbstractT c) where
  alloc = proc (_,_,l) -> returnA -< return l
           
instance (IsString e, ArrowChoice c, ArrowJoin c, ArrowFail e c) => IsValue AbsVal (AbstractT c) where
  numLit = arr $ \n -> NumVal (Interval n n)

  add = proc (v1,v2) -> case (v1,v2) of
    -- When adding all numbers within two intervals, the smallest
    -- number that can occur is the addition of the lower interval
    -- bounds and the largest number that can occur is the addition of
    -- the upper interval bounds.
    (NumVal (Interval x1 y1),NumVal (Interval x2 y2)) -> returnA -< NumVal (Interval (x1 + x2) (y1 + y2))
    (TopVal,TopVal) -> (returnA -< TopVal) <⊔> (fail -< "Expected two numbers as arguments for 'add'")
    _ -> fail -< "Expected two numbers as arguments for 'add'"


  lt = proc (v1,v2) -> case (v1,v2) of
    (NumVal (Interval x1 y1),NumVal (Interval x2 y2))
      -- When the largest number in the first interval is smaller than
      -- the smallest number in the second interval, then the `<`
      -- check succeeds.
      | y1 <= x2  -> returnA -< BoolVal True

      -- When the largest number in the second interval is smaller than
      -- the smallest number in the first interval, then the `<`
      -- check fails.
      | y2 <  x1  -> returnA -< BoolVal False

      -- When the intervals overlap, we don't know if the `<` check succeeds or fails.
      | otherwise -> returnA -< BoolVal TopBool
    (TopVal,TopVal) -> (returnA -< BoolVal TopBool) <⊔> (fail -< "Expected two numbers as arguments for 'lt'")
    _ -> fail -< "Expected two numbers as arguments for 'lt'"


  boolLit = arr $ \b -> case b of
    P.True -> BoolVal True
    P.False -> BoolVal False

  and = proc (v1,v2) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> case b1 of
      False   -> returnA -< BoolVal False
      True    -> returnA -< BoolVal b2
      TopBool -> returnA -< BoolVal (False ⊔ b2)
    (TopVal,TopVal)         -> (returnA -< BoolVal TopBool) <⊔> (fail -< "Expected two booleans as arguments for 'and'")
    _                       -> fail -< "Expected two booleans as arguments for 'and'"


  if_ f g = proc (cond,s1,s2) -> case cond of
    BoolVal True -> f -< s1
    BoolVal False -> g -< s2

    -- If the condition could evaluate to true or false, we have to
    -- evaluate both branches and join the results.
    BoolVal TopBool -> (f -< s1) <⊔> (g -< s2)
    NumVal _ -> fail -< "Expected a boolean expression as condition for an if"
    TopVal -> (f -< s1) <⊔> (g -< s2) <⊔> (fail -< "Expected a boolean expression as condition for an if")


-- The ordering on abstract values defines which values are more
-- precise than others.
instance PreOrd AbsVal where
  NumVal i1 ⊑ NumVal i2 = i1 ⊑ i2
  BoolVal b1 ⊑ BoolVal b2 = b1 ⊑ b2
  _ ⊑ TopVal = P.True
  _ ⊑ _ = P.False

instance PreOrd Interval where
  Interval x1 y1 ⊑ Interval x2 y2 = x2 <= x1 && y1 <= y2

instance PreOrd AbsBool where
  True ⊑ True = P.True
  False ⊑ False = P.True
  _ ⊑ TopBool=  P.True
  _ ⊑ _ = P.False

-- The least upper bound v1 ⊔ v2 calculates the smallest value that is
-- greater than v1 and v2.
instance Complete AbsVal where
  NumVal i1 ⊔ NumVal i2 = NumVal (i1 ⊔ i2)
  BoolVal b1 ⊔ BoolVal b2 = BoolVal (b1 ⊔ b2)
  _ ⊔ _ = TopVal

instance Complete Interval where
  Interval x1 y1 ⊔ Interval x2 y2 = Interval (min x1 y1) (max x2 y2)

instance Complete AbsBool where
  b1 ⊔ b2 = if b1 == b2 then b1 else TopBool

-- Widening also calculates an upper bound of two values, however, it
-- additionally ensures that an infinite ascending chain of values
-- becomes finite.
widenVal :: Widening AbsVal
widenVal (NumVal (Interval x1 y1)) (NumVal (Interval x2 y2))
  | x2 < x1 || y1 < y2 = (W.Instable,TopVal)
  | otherwise = (W.Stable,NumVal (Interval x2 y2))
widenVal (BoolVal b1) (BoolVal b2) = if b1 == b2 then (W.Stable,BoolVal b1) else (W.Instable,BoolVal TopBool)
widenVal v1 v2
  | v1 == v2 = (W.Stable,v1)
  | otherwise = (W.Instable,TopVal)


instance Hashable AbsVal
instance Hashable AbsBool
instance Hashable Interval

whileLoops :: L.Prism' [Statement] ((Expr,[Statement],Label),[Statement])
whileLoops = L.prism' (\((c,b,l),ss) -> While c b l:ss)
                (\s -> case s of
                   While c b l:ss -> Just ((c,b,l),ss)
                   _ -> Nothing)
