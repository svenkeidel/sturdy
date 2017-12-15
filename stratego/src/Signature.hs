{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Signature(Signature, HasSignature(..), Sort(..), Fun(..), SortId(..),
                 empty, insertType, lookupType, insertSubtype, subtype, inhabitants) where

import           Prelude hiding (lookup)

import           Sort
import           SubtypeRelation (SubtypeRelation)
import qualified SubtypeRelation as R

import           Data.Constructor
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Maybe

import           Control.Arrow

type Context = (HashMap Constructor Fun, HashMap Sort [(Constructor,Fun)])
data Signature = Signature Context SubtypeRelation deriving (Show,Eq)
data Fun = Fun [Sort] Sort deriving (Show,Eq)

empty :: Signature
empty = Signature (M.empty, M.empty) R.empty

insertType :: Constructor -> Fun -> Signature -> Signature
insertType con ty@(Fun _ sort) (Signature (cons,sorts) sub) =
  Signature (M.insert con ty cons, M.insertWith (\[v] l -> v:l) sort [(con,ty)] sorts) sub

insertSubtype :: Sort -> Sort -> Signature -> Signature
insertSubtype ty1 ty2 (Signature ctx sub) = Signature ctx (R.insert ty1 ty2 sub)

lookupType :: Constructor -> Signature -> Maybe Fun
lookupType c (Signature (cons,_) _) = M.lookup c cons

subtype :: Signature -> Sort -> Sort -> Bool
subtype (Signature _ rel) = R.subtype rel

inhabitants :: Signature -> Sort -> [(Constructor,Fun)]
inhabitants sig@(Signature (_,sorts) rel) s0 = do
  s <- R.lower rel s0
  case s of
    Bottom -> []
    Top -> error "Calculating inhabitants from sort top is not allowed"
    Coproduct a b -> inhabitants sig a ++ inhabitants sig b
    List a -> [("Cons", Fun [a, List a] (List a)), ("Nil", Fun [] (List a))]
    Option a -> [("Some", Fun [a] (Option a)), ("None", Fun [] (Option a))]
    Tuple as -> [("", Fun as (Tuple as))]
    Sort "String" -> [("", Fun [] "String")]
    Sort "INT" -> [("", Fun [] "INT")]
    Sort x -> fromMaybe (error $ "Sort not found: " ++ show x)
                        (M.lookup s sorts)

class Arrow c => HasSignature c where
  getSignature :: c () Signature

-- instance HasSignature c => PreOrd Sort c where
--   (⊑) = proc (s1,s2) -> do
--     sig <- getSignature -< ()
--     returnA -< subtype sig s1 s2
