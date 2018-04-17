{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module LiveVariablesSpec where

import           Prelude hiding (read,sequenceA,Bounded,(.))

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Store
import           Control.Arrow.Fix
import           Control.Arrow.Transformer.Abstract.LiveVariables
import qualified Control.Arrow.Transformer.Abstract.LiveVariables as L
import           Control.Arrow.Transformer.Abstract.Store
import           Control.Arrow.Transformer.Abstract.Except
import           Control.Arrow.Transformer.Abstract.Fix

import           Data.Text (Text)
import           Data.Hashable
import           Data.Order
import           Data.Ord
import qualified Data.List as L

import           Data.Abstract.FreeCompletion
import           Data.Abstract.Error hiding (fromError)
import qualified Data.Abstract.Store as S
import           Data.Abstract.Terminating hiding (fromTerminating)
import           Data.Abstract.Widening

import           Test.Hspec
import           GHC.Generics

main :: IO ()
main = hspec spec

newtype Interp x y = Interp {runInterp :: Fix [(Int,Statement)] () (LiveVariables Text (StoreArrow Text (FreeCompletion Expr) (Except String (~>)))) x y}

data Statement = Text := Expr | IfZero Expr [(Int,Statement)] [(Int,Statement)] | WhileZero Expr [(Int,Statement)] deriving (Show,Eq,Generic)
data Expr = Lit Int | Var Text deriving (Show,Eq,Generic)

run :: (ArrowFix [(Int,Statement)] () c, ArrowChoice c, ArrowStore Text v c, IsVal v c) => c [(Int,Statement)] ()
run = fixA $ \run' -> proc stmts -> case stmts of
  (_,x := e) : rs -> do
    v <- eval -< e
    write -< (x,v)
    run' -< rs
  (_,IfZero e ifB elseB) : rs -> do
    v <- eval -< e
    ifZero run' run' -< (v,ifB,elseB)
    run' -< rs
  (i,WhileZero e body) : rs -> do
    run' -< ((i, IfZero e (body ++ [(i,WhileZero e body)]) []) : rs)
  [] -> do
    returnA -< ()

class IsVal v c | c -> v where
  lit :: c Int v
  ifZero :: c [(Int,Statement)] () -> c [(Int,Statement)] () -> c (v,[(Int,Statement)],[(Int,Statement)]) ()

instance IsVal (FreeCompletion Expr) Interp where
  lit = arr (Lower . Lit)
  ifZero f g = proc (_,x,y) -> joined f g -< (x,y)

eval :: (IsVal v c, ArrowChoice c, ArrowStore Text v c) => c Expr v
eval = proc e -> case e of
  Lit n -> lit -< n
  Var x -> read -< x

spec :: Spec
spec = do
  it "x:=1" $ do
    runAnalysis  [ (0,"x" := Lit 1) ]
      `shouldBe` [ (0,([],[]))      ]

  it "x:=1; y:=x" $ do
    runAnalysis  [ (0,"x" := Lit 1), (1,"y" := Var "x") ]
      `shouldBe` [ (0,([],["x"])),   (1,(["x"],[]))     ]

  it "x:=1; if(2) {y:=x} {y:=2}" $ do
    runAnalysis [ (0,"x" := Lit 1)
                , (1,IfZero (Lit 2)
                        [(2,"y" := Var "x")]
                        [(3,"y" := Lit 2)])
                ]
      `shouldBe` [ (0,([],["x"]))
                 , (1,(["x"],["x"]))
                 ,      (2,(["x"],[]))
                 ,      (3,([],[]))
                 ]

  it "x:=1; while(2){y:=x}; z:=y" $ do
    runAnalysis   [ (0,"x" := Lit 1)
                  , (1,WhileZero (Lit 2)
                          [(2,"y" := Var "x")])
                  , (3,"z":= Var "y")
                  ]
      `shouldBe`  [ (0,(["y"],["x","y"]))
                  , (1,(["x","y"],["x","y"]))
                  ,       (2,(["x"],["x"]))
                  , (3,(["y"],[]))
                  ]

runAnalysis :: [(Int,Statement)] -> [(Int,(LiveVars Text,LiveVars Text))]
runAnalysis ss =
  L.sortBy (comparing fst) $
  S.toList $
  S.map (\((_,l),q) -> case l of
                         [] -> Nothing;
                         ((i,_):_) ->
                            let trans = (fst (snd (fromError (fromTerminating q))))
                            in Just (i,(L.entry trans, L.exit trans))) $
  fst $ runFix' (runExcept (runStore (runLiveVariables (runInterp (run :: Interp [(Int,Statement)] ()))))) (S.empty,ss)
  where
    fromTerminating :: Terminating a -> a
    fromTerminating (Terminating a) = a
    fromTerminating NonTerminating = error "non terminating"

    fromError :: Error String a -> a
    fromError (Success x) = x
    fromError (Fail e) = error e

instance Hashable Statement
instance Hashable Expr
instance PreOrd Expr where
  (⊑) = (==)
instance Widening (FreeCompletion Expr)

deriving instance Category Interp
deriving instance Arrow Interp
deriving instance ArrowChoice Interp
deriving instance ArrowFix [(Int,Statement)] () Interp
deriving instance ArrowStore Text (FreeCompletion Expr) Interp
deriving instance PreOrd (Fix [(Int,Statement)] () (LiveVariables Text (StoreArrow Text (FreeCompletion Expr) (Except String (~>)))) x y) => PreOrd (Interp x y)
deriving instance Complete (Fix [(Int,Statement)] () (LiveVariables Text (StoreArrow Text (FreeCompletion Expr) (Except String (~>)))) x y) => Complete (Interp x y)
