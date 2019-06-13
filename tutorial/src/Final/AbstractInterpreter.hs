{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Final.AbstractInterpreter where

import           Prelude hiding (lookup,and,fail)

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Fix
import           Control.Arrow.Environment
import           Control.Arrow.Store
import           Control.Arrow.Fail
import           Control.Arrow.Alloc
import           Control.Arrow.Transformer.Abstract.Error
import           Control.Arrow.Transformer.Abstract.Environment
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Transformer.Abstract.Fix

import           Data.Abstract.FreeCompletion (FreeCompletion)
import           Data.Abstract.Error (Error)
import           Data.Abstract.Terminating (Terminating)
import qualified Data.Abstract.StrongMap as SM
import qualified Data.Abstract.Map as M
import           Data.Profunctor
import           Data.Label
import           Data.Hashable
import           Data.Abstract.DiscretePowerset(Pow)

import           GHC.Generics

import           Final.GenericInterpreter(IsValue(..))
import qualified Final.GenericInterpreter as Generic
import           Syntax


type Addr = FreeCompletion Label
data AbsVal = BoolVal AbsBool | NumVal Interval | TopVal deriving (Eq,Generic)
data AbsBool = True | False | TopBool deriving (Eq,Generic)
data Interval = Interval Int Int deriving (Eq,Generic)

run :: [LStatement] -> Terminating (Error (Pow String) (M.Map Addr AbsVal))
run stmts = _ $
  runFixT _ _
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

newtype AbstractT c x y = AbstractT { runAbstractT :: c x y }
  deriving (Category,Profunctor,Arrow,ArrowChoice,ArrowFail e,ArrowEnv var addr env,ArrowStore addr val)
deriving instance ArrowFix x y c => ArrowFix x y (AbstractT c)
type instance Fix x y (AbstractT c) = AbstractT (Fix x y c)

instance (Profunctor c, Arrow c) => ArrowAlloc (String, AbsVal, Label) Addr (AbstractT c) where
  alloc = proc (_,_,l) -> returnA -< return l
           
instance (ArrowChoice c, ArrowFail String c) => IsValue AbsVal (AbstractT c) where
  numLit = _
  add = _

  lt = _

  boolLit = _
  and = _

  if_ f g = _

-- eval e = case e of
--   Var x -> do
--     st <- get
--     case Store.lookup x st of
--       Just v -> return v
--       Nothing -> throw "Variable not in scope"
--   NumLit n -> return (NumVal (Interval n n))
--   Add e1 e2 -> do
--     v1 <- eval e1
--     v2 <- eval e2
--     case (v1,v2) of
--       (NumVal (Interval i1 j1), NumVal (Interval i2 j2)) ->
--         return (NumVal (Interval (i1 + i2) (j1 + j2)))
--       (_,_) -> throw "Expected two numbers as arguments for +"
--   BoolLit b -> case b of
--     P.True -> return (BoolVal True)
--     P.False -> return (BoolVal False)
--   And e1 e2 -> do
--     v1 <- eval e1
--     v2 <- eval e2
--     case (v1,v2) of
--       (BoolVal b1, BoolVal b2) -> return $ BoolVal $ case (b1,b2) of
--         (False,_) -> False
--         (_,False) -> False
--         (True,_) -> b2
--         (_,True) -> b1
--         (TopBool,_) -> TopBool
--         (_,TopBool) -> TopBool
--       (TopVal, _) -> return TopVal
--       (_,TopVal) -> return TopVal
--       (_,_) -> throw "Expected two booleans as arguments for &&"
--   Lt e1 e2 -> do
--     v1 <- eval e1
--     v2 <- eval e2
--     case (v1,v2) of
--       (NumVal (Interval i1 j1), NumVal (Interval i2 j2))
--         | j1 < i2   -> return (BoolVal True) 
--         | j2 < i1   -> return (BoolVal False) 
--         | otherwise -> return (BoolVal TopBool) 
--       (TopVal, _) -> return TopVal
--       (_,TopVal) -> return TopVal
--       (_,_) -> throw "Expected two numbers as arguments for +"

-- -- run :: Store -> [Statement] -> Either String Store
-- run :: [Statement] -> M ()
-- run stmts = case stmts of
--   (Assign x e : rest) -> do
--     v <- eval e
--     st <- get
--     put (Store.insert x v st)
--     run rest
--   (If cond ifBranch elseBranch : rest) -> do
--     v <- eval cond
--     case v of
--       BoolVal True -> run ifBranch
--       BoolVal False -> run elseBranch
--       BoolVal TopBool -> run ifBranch ⊔ run elseBranch
--       TopVal -> top
--       NumVal _ -> throw "Expected a boolean expression as condition for an if"
--     run rest
--   (While cond body : rest) ->
--     run (If cond (body ++ [While cond body]) [] : rest)
--   [] ->
--     return ()

instance Hashable AbsVal
instance Hashable AbsBool
instance Hashable Interval
