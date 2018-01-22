{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
module IntervalAnalysisDeadWrites where

import Prelude
import qualified Prelude as Prelude

import WhileLanguage

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Interval
import Data.Order
import Data.Error

import Control.Monad.State
import Control.Monad.Except
import Control.Arrow
import Control.Arrow.Fail
import Control.Arrow.Utils

data Val = BoolVal Bool | NumVal (Interval Double) | Top

instance PreOrd Val where
  _ ⊑ Top = True
  BoolVal b1 ⊑ BoolVal b2 = b1 == b2
  NumVal n1 ⊑ NumVal n2 = n1 ⊑ n2
  _ ⊑ _ = False

instance Complete Val where
  BoolVal b1 ⊔ BoolVal b2 = if b1 == b2 then BoolVal b1 else Top
  NumVal n1 ⊔ NumVal n2 = NumVal $ n1 ⊔ n2
  _ ⊔ _ = Top

type Store = Map Text Val
initStore :: Store
initStore = M.empty

data DeadWrites = DeadWrites {
  maybeDead :: Set Text, -- dead unless followed by a read
  mustDead :: Set Text -- definitely dead (e.g., because of double write)
}
finalize :: DeadWrites -> Set Text
finalize (DeadWrites may must) = may `Set.union` must
instance PreOrd DeadWrites where
  (DeadWrites may1 must1) ⊑ (DeadWrites may2 must2) = (must1 ⊑ must2) && (maymay ⊑ may2)
    where maymay = Set.filter (\x -> Prelude.not $ Set.member x must2) may1
instance Complete DeadWrites where
  (DeadWrites may1 must1) ⊔ (DeadWrites may2 must2) = DeadWrites (may1 ⊔ may2) (must1 ⊔ must2)

type Prop = DeadWrites
initProp :: Prop
initProp = DeadWrites Set.empty Set.empty

type M = StateT (Store,Prop) (Except String)
runM :: [Statement] -> Error String ((), (Store,Prop))
runM ss = fromEither $ runExcept $ runStateT (runKleisli run ss) (initStore,initProp)

runAbstract :: [Statement] -> Error String Store
runAbstract ss = fmap (fst.snd) $ runM ss

propAbstract :: [Statement] -> Error String (Set Text)
propAbstract ss = fmap (finalize . snd . snd) $ runM ss

getStore :: M Store
getStore = get >>= return . fst

putStore :: Store -> M ()
putStore env = modify (\(x,y) -> (env,y))

modifyStore :: (Store -> Store) -> M ()
modifyStore f = modify (\(x,y) -> (f x, y))

putProp :: Prop -> M ()
putProp prop = modify (\(x,y) -> (x,prop))

modifyProp :: (Prop -> Prop) -> M ()
modifyProp f = modify (\(x,y) -> (x, f y))

instance Run (Kleisli M) Val where
  fixRun f = voidA $ mapA $ f (fixRun f)

  store = Kleisli $ \(x,v,_) -> do
    modifyStore (M.insert x v)
    modifyProp (\(DeadWrites maybe must) ->
          if x `Set.member` maybe
            then DeadWrites maybe (Set.insert x must)
            else DeadWrites (Set.insert x maybe)  must)

  if_ f1 f2 = proc (v,(x,y),_) -> case v of
    BoolVal True -> f1 -< x
    BoolVal False -> f2 -< y
    Top -> (f1 -< x) ⊔ (f2 -< y)
    _ -> failA -< "Expected boolean as argument for 'if'"

instance Eval (Kleisli M) Val where
  lookup = Kleisli $ \x -> do
    modifyProp (\(DeadWrites maybe must) -> DeadWrites (Set.delete x maybe) must)
    env <- getStore
    case M.lookup x env of
      Just v -> return v
      Nothing -> throwError "variable not found"

  boolLit = arr BoolVal

  and = proc (v1,v2) -> case (v1,v2) of
    (BoolVal False,_) -> returnA -< BoolVal False
    (_,BoolVal False) -> returnA -< BoolVal False
    (BoolVal True,BoolVal True) -> returnA -< BoolVal True
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two booleans as arguments for 'and'"

  or = proc (v1,v2) -> case (v1,v2) of
    (BoolVal True,_) -> returnA -< BoolVal True
    (_,BoolVal True) -> returnA -< BoolVal True
    (BoolVal False,BoolVal False) -> returnA -< BoolVal False
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two booleans as arguments for 'or'"
  
  not = proc v -> case v of
    BoolVal True -> returnA -< BoolVal False
    BoolVal False -> returnA -< BoolVal True
    Top -> returnA -< Top
    _ -> failA -< "Expected a boolean as argument for 'not'"

  numLit = arr $ \x -> NumVal (IV (x,x))

  add = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 + n2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two numbers as arguments for 'add'"

  sub = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 - n2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two numbers as arguments for 'sub'"

  mul = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 * n2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two numbers as arguments for 'mul'"

  div = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2) -> returnA -< NumVal (n1 / n2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two numbers as arguments for 'mul'"

  eq = proc (v1,v2) -> case (v1,v2) of
    (NumVal n1,NumVal n2)   -> returnA -< BoolVal (n1 == n2)
    (BoolVal b1,BoolVal b2) -> returnA -< BoolVal (b1 == b2)
    (Top,_) -> returnA -< Top
    (_,Top) -> returnA -< Top
    _ -> failA -< "Expected two values of the same type as arguments for 'eq'"

  fixEval f = f (fixEval f)
