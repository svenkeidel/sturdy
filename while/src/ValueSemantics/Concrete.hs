{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module ValueSemantics.Concrete where

import           Prelude
import qualified Prelude as P

import           Syntax
import           SharedSemantics hiding (run)
import qualified SharedSemantics as Shared

import           Data.Concrete.Error
import qualified Data.Concrete.Store as S
import           Data.Concrete.Store (Store)
import qualified Data.Concrete.Environment as E
import           Data.Hashable
import           Data.Text (Text)
import           Data.Label

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fail
import           Control.Arrow.State
import           Control.Arrow.Fix
import           Control.Arrow.Environment
import           Control.Arrow.Store
import           Control.Arrow.Try
import           Control.Arrow.Transformer.State
import           Control.Arrow.Transformer.Concrete.Environment
import           Control.Arrow.Transformer.Concrete.Except
import           Control.Arrow.Transformer.Concrete.Store
import           Control.Arrow.Transformer.Concrete.LeastFixPoint(runLeastFixPoint)

import           System.Random

import           GHC.Generics (Generic)

type Addr = Int
data Val = BoolVal Bool | NumVal Int | RefVal Addr deriving (Eq, Show, Generic)
type Env = [(Text,Addr)]
type ProgState = (StdGen,Addr)

-- Interp c x y ~= c (Store Addr Val,(Env Text Addr,(ProgState,x))) (Error String (Store Addr Val,(ProgState,y)))
newtype Interp c x y = Interp (State ProgState (Environment Text Addr (StoreArrow Addr Val (Except String c))) x y)

runInterp :: ArrowChoice c => Interp c x y -> c (Store Addr Val, (Env, (ProgState,x))) (Error String (Store Addr Val, (ProgState,y)))
runInterp (Interp f) = runExcept (runStore (runEnvironment (runState f)))

run :: [Statement] -> Error String (Store Addr Val)
run ss = fst <$> runLeastFixPoint (runInterp (Shared.run :: Fix [Statement] () (Interp (->)) [Statement] ())) (S.empty,([],((mkStdGen 0,0),ss)))

instance ArrowChoice c => IsVal Val Addr (Interp c) where
  boolLit = arr (\(b,_) -> BoolVal b)
  and = proc (v1,v2,_) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 && b2)
    _ -> failA -< "Expected two booleans as arguments for 'and'"
  or = proc (v1,v2,_) -> case (v1,v2) of
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 || b2)
    _ -> failA -< "Expected two booleans as arguments for 'or'"
  not = proc (v,_) -> case v of
    BoolVal b -> returnA -< BoolVal (Prelude.not b)
    _ -> failA -< "Expected a boolean as argument for 'not'"
  numLit = arr (\(d,_) -> NumVal d)
  randomNum = proc _ -> do
    (gen,loc) <- getA -< ()
    let (r, gen') = random gen
    putA -< (gen',loc)
    returnA -< NumVal r
  add = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 + n2)
    _ -> failA -< "Expected two numbers as arguments for 'add'"
  sub = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 - n2)
    _ -> failA -< "Expected two numbers as arguments for 'sub'"
  mul = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 * n2)
    _ -> failA -< "Expected two numbers as arguments for 'mul'"
  div = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 `Prelude.div` n2)
    _ -> failA -< "Expected two numbers as arguments for 'mul'"
  eq = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2)   -> returnA -< BoolVal (n1 P.== n2)
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 P.== b2)
    _ -> failA -< "Expected two values of the same type as arguments for 'eq'"
  lt = proc (v1,v2,_) -> case (v1,v2) of
    (NumVal n1,NumVal n2)   -> returnA -< BoolVal (n1 P.< n2)
    _ -> failA -< "Expected two numbers as arguments for 'lt'"
  freshAddr = proc _ -> do
    (gen,a) <- getA -< ()
    putA -< (gen,a+1)
    returnA -< a
  ref = arr RefVal
  getAddr = proc (r,_) -> case r of
    RefVal a -> returnA -< a
    v -> failA -< "Expected reference but found " ++ show v

instance ArrowChoice c => Conditional Val x y z (Interp c) where
  if_ f1 f2 = proc (v,(x,y)) -> case v of
    BoolVal True -> f1 -< x
    BoolVal False -> f2 -< y
    _ -> failA -< "Expected boolean as argument for 'if'"

deriving instance ArrowChoice c => Category (Interp c)
deriving instance ArrowChoice c => Arrow (Interp c)
deriving instance ArrowChoice c => ArrowChoice (Interp c)
deriving instance ArrowChoice c => ArrowFail String (Interp c)
deriving instance ArrowChoice c => ArrowState ProgState (Interp c)
deriving instance ArrowChoice c => ArrowStore Addr Val Label (Interp c)
deriving instance ArrowChoice c => ArrowEnv Text Addr (E.Env Text Addr) (Interp c)
deriving instance ArrowChoice c => ArrowTry (Text,Label) Addr Addr (Interp c)
type instance Fix x y (Interp c) = Interp (Fix (Store Addr Val,(E.Env Text Addr,(ProgState,x))) (Error String (Store Addr Val,(ProgState,y))) c)
deriving instance (ArrowFix (Store Addr Val,(E.Env Text Addr,(ProgState,x))) (Error String (Store Addr Val,(ProgState,y))) c, ArrowChoice c) => ArrowFix x y (Interp c)

instance Hashable Val
