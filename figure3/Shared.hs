{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module Shared where

import Prelude hiding (lookup)
import Control.Arrow

data Expr = Var String | Lit Int | Add Expr Expr | IfZero Expr Expr Expr | TryZero Expr Expr Expr

class ArrowFix x y c where
  fixA :: (c x y -> c x y) -> c x y

class Arrow c => IsVal v c | c -> v where
  lookup :: c String v
  lit :: c Int v
  add :: c (v,v) v
  ifZero :: c x v -> c y v -> c (v,(x,y)) v
  try :: c x y -> c y v -> c x v -> c x v

eval' :: (ArrowChoice c, ArrowFix Expr v c, IsVal v c) => c Expr v
eval' = fixA $ \ev -> proc e -> case e of
  Var x -> lookup -< x
  Lit n -> lit -< n
  Add e1 e2 -> do
    v1 <- ev -< e1
    v2 <- ev -< e2
    add -< (v1,v2)
  IfZero e1 e2 e3 -> do
    v <- ev -< e1
    ifZero ev ev -< (v,(e2,e3))
  TryZero e1 e2 e3 -> do
    try (proc (e1,x) -> do
           v <- ev -< e1
           returnA -< (v,x))
        (ifZero ev ev)
        (proc (_,(_,e3)) -> ev -< e3)
      -< (e1,(e2,e3))
