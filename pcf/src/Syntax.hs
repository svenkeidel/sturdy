{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Syntax where

import           Data.Text(Text,unpack)
import           Data.Hashable
import           Data.Label
import           Data.String
import           Data.Lens (Prism')
import qualified Data.Lens as L
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.HashSet(HashSet)
import qualified Data.HashSet as H

import Control.Monad.State

-- | Expressions of PCF. Each expression has a label, with which the
-- expression can be uniquely identified.
data Expr
  = Var Text Label
  | Lam Text Expr Label
  | App Expr Expr Label
  | Zero Label
  | Succ Expr Label
  | Pred Expr Label
  | IfZero Expr Expr Expr Label
  | Y Expr Label
  | Apply Expr Label
  deriving (Eq)

-- Smart constructors that build labeled PCF expressions.

var :: Text -> State Label Expr
var x = Var x <$> fresh

lam :: Text -> State Label Expr -> State Label Expr
lam x e = Lam x <$> e <*> fresh

app :: State Label Expr -> State Label Expr -> State Label Expr
app e1 e2 = App <$> e1 <*> e2 <*> fresh

zero :: State Label Expr
zero = Zero <$> fresh

succ :: State Label Expr -> State Label Expr
succ e = Succ <$> e <*> fresh

pred :: State Label Expr -> State Label Expr
pred e = Pred <$> e <*> fresh

ifZero :: State Label Expr -> State Label Expr -> State Label Expr -> State Label Expr
ifZero e1 e2 e3 = IfZero <$> e1 <*> e2 <*> e3 <*> fresh

fix :: State Label Expr -> State Label Expr
fix e = Y <$> e <*> fresh


instance Show Expr where
  showsPrec d e0 = case e0 of
    Var x _ -> showString (unpack x)
    Zero _ -> showString "zero"
    Succ e _ -> showParen (d > app_prec) $ showString "succ " . showsPrec (app_prec + 1) e
    Pred e _ -> showParen (d > app_prec) $ showString "pred " . showsPrec (app_prec + 1) e
    Y e _ -> showParen (d > app_prec) $ showString "Y " . showsPrec (app_prec + 1) e
    Apply e _ -> showParen (d > app_prec) $ showsPrec (app_prec + 1) e
    IfZero e1 e2 e3 _ -> showParen (d > app_prec)
      $ showString "ifZero "
      . showsPrec (app_prec + 1) e1
      . showString " "
      . showsPrec (app_prec + 1) e2
      . showString " "
      . showsPrec (app_prec + 1) e3
    App e1 e2 _ -> showParen (d > app_prec)
      $ showsPrec (app_prec + 1) e1
      . showString " "
      . showsPrec (app_prec + 1) e2
    Lam x e2 _ -> showParen (d > lam_prec)
      $ showString "λ"
      . showString (unpack x)
      . showString ". "
      . shows e2
    where
      app_prec = 10
      lam_prec = 9

instance HasLabel Expr where
  label e = case e of
    Var _ l -> l
    Lam _ _ l -> l
    App _ _ l -> l
    Zero l -> l
    Succ _ l -> l
    Pred _ l -> l
    IfZero _ _ _ l -> l
    Y _ l -> l
    Apply _ l -> l

instance IsString (State Label Expr) where
  fromString = var . fromString

instance Hashable Expr where
  hashWithSalt s (Var x _) = s `hashWithSalt` (0::Int) `hashWithSalt` x
  hashWithSalt s (Lam x e _) = s `hashWithSalt` (1::Int) `hashWithSalt` x `hashWithSalt` e
  hashWithSalt s (App e1 e2 _) = s `hashWithSalt` (2::Int) `hashWithSalt` e1 `hashWithSalt` e2
  hashWithSalt s (Zero _) = s `hashWithSalt` (3::Int)
  hashWithSalt s (Succ e _) = s `hashWithSalt` (4::Int) `hashWithSalt` e
  hashWithSalt s (Pred e _) = s `hashWithSalt` (5::Int) `hashWithSalt` e
  hashWithSalt s (IfZero e1 e2 e3 _) = s `hashWithSalt` (6::Int) `hashWithSalt` e1 `hashWithSalt` e2 `hashWithSalt` e3
  hashWithSalt s (Y e _) = s `hashWithSalt` (7::Int) `hashWithSalt` e
  hashWithSalt s (Apply e _) = s `hashWithSalt` (8::Int) `hashWithSalt` e

apply :: Prism' (env,Expr) ((Expr,Label),env)
apply = L.prism' (\((e',l),env) -> (env,Apply e' l))
                 (\(env,e) -> case e of
                      Apply e' l -> Just ((e',l),env)
                      _ -> Nothing)

freeVars :: Expr -> HashMap Expr (HashSet Text)
freeVars e0 = execState (go e0) M.empty
  where
    go :: Expr -> State (HashMap Expr (HashSet Text)) (HashSet Text)
    go e = case e of
      Var x _ -> return (H.singleton x)
      Lam x e1 _ -> save $ H.delete x <$> go e1
      App e1 e2 _ -> H.union <$> go e1 <*> go e2
      Zero _ -> return H.empty
      Succ e1 _ -> go e1
      Pred e1 _ -> go e1
      IfZero e1 e2 e3 _ -> do
          m1 <- go e1
          m2 <- go e2
          m3 <- go e3
          return (m1 <> m2 <> m3)
      Y e1 _ -> go e1
      Apply e1 _ -> go e1
      where
        save m = do
          fv <- m
          modify (M.insert e fv)
          return fv
