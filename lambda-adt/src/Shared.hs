{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module Shared where

import Prelude hiding (lookup)

import Control.Arrow
import Control.Arrow.Fix
import Control.Arrow.Reader
import Control.Arrow.Utils

import Data.String
import Data.Text(Text,unpack)

data Expr
  = Var Text
  | Lam Text Expr
  | App Expr Expr
  | Con Text [Expr]
  | Case Expr [(Pat,Expr)]
  deriving (Eq)

data Pat
  = ConP Text [Pat]
  | VarP Text
  deriving (Eq)

eval :: (Show env, ArrowReader env c, ArrowChoice c, ArrowFix c, IsVal v c, IsClosure Expr v c) => c Expr v
eval = fixA $ \ev -> proc e0 -> case e0 of
  Var x -> lookup -< x
  Lam x body -> closure -< (x,body)
  App e1 e2 -> do
    fun <- ev -< e1
    arg <- ev -< e2
    applyClosure ev -< (fun, arg)
  Con c es -> do
    vs <- mapA ev -< es
    con -< (c,vs)
  Case e cases -> do
    v <- ev -< e
    match ev -< (v,cases)

class Arrow c => IsVal v c | c -> v where
  lookup :: c Text v
  con :: c (Text,[v]) v
  match :: c Expr v -> c (v,[(Pat,Expr)]) v

class Arrow c => IsClosure exp v c where
  closure :: c (Text,exp) v
  applyClosure :: c exp v -> c (v, v) v

instance IsString Expr where
  fromString = Var . fromString

instance Show Expr where
  showsPrec d e0 = case e0 of
    Var x -> showString $ unpack x
    Lam x e -> showParen (d > appPrec) $ showString ("λ" ++ unpack x ++ ". ") . shows e
    App e1 e2 -> showParen (d > appPrec) $ showsPrec (appPrec + 1) e1 . showString " " . showsPrec (appPrec + 1) e2
    Con x t -> showParen (d > appPrec) $ showString (unpack x) . showString " " . showsPrec d t
    Case e cases -> showString "case " . shows e . showString " of " . showList cases
    where appPrec = 10

instance Show Pat where
  showsPrec d e0 = case e0 of
    VarP x -> showString $ unpack x
    ConP x t -> showParen (d > appPrec) $ showString (unpack x) . showString " " . showsPrec d t
    where appPrec = 10

instance IsString Pat where
  fromString = VarP . fromString
