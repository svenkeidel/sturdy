{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Syntax where

import           Data.Text(Text,unpack)
import           Data.Hashable
import           Data.Label
import           Data.String
import           Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.HashSet(HashSet)
import qualified Data.HashSet as H
import           Data.Text.Prettyprint.Doc

import Control.Monad.State

-- | Expressions of PCF. Each expression has a label, with which the
-- expression can be uniquely identified.
data Expr
  = Var Text Label
  | Lam [Text] Expr Label
  | App Expr [Expr] Label
  | Zero Label
  | Mult Expr Expr Label
  | Succ Expr Label
  | Pred Expr Label
  | IfZero Expr Expr Expr Label
  | Let [(Text,Expr)] Expr Label
  | Apply Expr Label
  deriving (Eq)

-- Smart constructors that build labeled PCF expressions.

var :: Text -> State Label Expr
var x = Var x <$> fresh

lam :: [Text] -> State Label Expr -> State Label Expr
lam xs e = Lam xs <$> e <*> fresh

app :: State Label Expr -> [State Label Expr] -> State Label Expr
app e1 e2 = App <$> e1 <*> sequence e2 <*> fresh

zero :: State Label Expr
zero = Zero <$> fresh

succ :: State Label Expr -> State Label Expr
succ e = Succ <$> e <*> fresh

pred :: State Label Expr -> State Label Expr
pred e = Pred <$> e <*> fresh

mult :: State Label Expr -> State Label Expr -> State Label Expr
mult e1 e2 = Mult <$> e1 <*> e2 <*> fresh

ifZero :: State Label Expr -> State Label Expr -> State Label Expr -> State Label Expr
ifZero e1 e2 e3 = IfZero <$> e1 <*> e2 <*> e3 <*> fresh

let_ :: [(Text, State Label Expr)] -> State Label Expr -> State Label Expr
let_ bnds body = Let <$> sequence [ (v,) <$> e | (v,e) <- bnds ] <*> body <*> fresh


instance Show Expr where
  showsPrec d e0 = case e0 of
    Var x _ -> showString (unpack x)
    Zero _ -> showString "zero"
    Succ e _ -> showParen (d > app_prec) $ showString "succ " . showsPrec (app_prec + 1) e
    Pred e _ -> showParen (d > app_prec) $ showString "pred " . showsPrec (app_prec + 1) e
    Let bnds e _ -> showParen (d > app_prec) $ showString "let " . shows bnds . showString " in " . showsPrec (app_prec + 1) e
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
    Mult e1 e2 _ -> showParen (d > mult_prec)
      $ showsPrec (mult_prec + 1) e1
      . showString " * "
      . showsPrec (mult_prec + 1) e2
    Lam x e2 _ -> showParen (d > lam_prec)
      $ showString "λ"
      . showString (unwords (map unpack x))
      . showString ". "
      . shows e2
    where
      app_prec = 10
      lam_prec = 9
      mult_prec = 8

instance Pretty Expr where
  pretty = viaShow

instance HasLabel Expr where
  label e = case e of
    Var _ l -> l
    Lam _ _ l -> l
    App _ _ l -> l
    Zero l -> l
    Succ _ l -> l
    Pred _ l -> l
    Mult _ _ l -> l
    IfZero _ _ _ l -> l
    Let _ _ l -> l
    Apply _ l -> l

instance IsString (State Label Expr) where
  fromString = var . fromString

instance Hashable Expr where
  hashWithSalt s e = s `hashWithSalt` label e

isFunctionBody :: (store,(env,Expr)) -> Bool
isFunctionBody (_,(_,e)) = case e of
  Apply {} -> True
  _ -> False
{-# INLINE isFunctionBody #-}

freeVars :: Expr -> HashMap Expr (HashSet Text)
freeVars e0 = execState (go e0) M.empty
  where
    go :: Expr -> State (HashMap Expr (HashSet Text)) (HashSet Text)
    go e = case e of
      Var x _ -> return (H.singleton x)
      Lam xs e1 _ -> save $ flip (foldr H.delete) xs <$> go e1
      App e1 e2 _ -> H.union <$> go e1 <*> (H.unions <$> mapM go e2)
      Zero _ -> return H.empty
      Succ e1 _ -> go e1
      Pred e1 _ -> go e1
      Mult e1 e2 _ -> H.union <$> go e1 <*> go e2
      IfZero e1 e2 e3 _ -> do
          m1 <- go e1
          m2 <- go e2
          m3 <- go e3
          return (m1 <> m2 <> m3)
      Let bnds e1 _ -> do
        vars <- go e1
        return (vars `H.difference` H.fromList [ v | (v,_) <- bnds])
      Apply e1 _ -> go e1
      where
        save m = do
          fv <- m
          modify (M.insert e fv)
          return fv
