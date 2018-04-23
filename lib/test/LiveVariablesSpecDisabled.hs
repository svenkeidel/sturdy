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
import           Control.Arrow.Transformer.Abstract.LeastFixPoint

import           Data.Text (Text)
import           Data.Hashable
import           Data.Order
import           Data.Ord
import qualified Data.List as L

import           Data.Abstract.FreeCompletion
import           Data.Abstract.Error
import qualified Data.Abstract.Store as S
import           Data.Abstract.Terminating
import           Data.Abstract.Widening

import           Test.Hspec
import           GHC.Generics

main :: IO ()
main = hspec spec

newtype Interp x y = Interp {runInterp :: Fix [(Int,Statement)] () (LiveVariables Text (StoreArrow Text (FreeCompletion Expr) (Except String (~>)))) x y}

data Statement = Text := Expr | IfZero Expr [(Int,Statement)] [(Int,Statement)] | WhileZero Expr [(Int,Statement)] deriving (Show,Eq,Generic)
data Expr = Lit Int | Var Text | Add Expr Expr deriving (Show,Eq,Generic)

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
  add :: c (v,v) v
  ifZero :: c [(Int,Statement)] () -> c [(Int,Statement)] () -> c (v,[(Int,Statement)],[(Int,Statement)]) ()

instance IsVal (FreeCompletion Expr) Interp where
  lit = arr (Lower . Lit)
  add = arr (\(e1,e2) -> Add <$> e1 <*> e2)
  ifZero (Interp f) (Interp g) = Interp $ proc (_,x,y) -> joined f g -< (x,y)

eval :: (IsVal v c, ArrowChoice c, ArrowStore Text v c) => c Expr v
eval = proc e -> case e of
  Lit n -> lit -< n
  Var x -> read -< x
  Add e1 e2 -> do
    v1 <- eval -< e1
    v2 <- eval -< e2
    add -< (v1,v2)

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

  it "x:=1; z:=2; if(2) {y:=x} {y:=x}; x:=y+z" $ do
    runAnalysis [ (0,"x" := Lit 1)
                , (1,"z" := Lit 2)
                , (2,IfZero (Lit 2)
                        [(3,"y" := Var "x")]
                        [(4,"y" := Var "x")])
                , (5,"x" := Add (Var "y") (Var "z"))
                ]
      `shouldBe`
                [ (0,([],["x"]))
                , (1,(["x"],["x","z"]))
                , (2,(["x","z"],["x","z"]))
                ,    (3,(["x","z"],["y","z"]))
                ,    (4,(["x","z"],["y","z"]))
                , (5,(["y","z"],[]))
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
                            let trans = (fst (snd (fromError (error "error") (fromTerminating (error "non terminating") q))))
                            in Just (i,(L.entry trans, L.exit trans))) $
  fst $ runLeastFixPoint' (runExcept (runStore (runLiveVariables (runInterp (run :: Interp [(Int,Statement)] ()))))) (S.empty,ss)

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
