{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SortContext(
  Context, HasContext(..), Sort(..), Signature(..), SortId(..), empty, signatures, sorts,
  fromList, insertSignature, insertSubtype, subtype, lookupSort, lookupCons,
  lub, glb, isLexical, isList, isSingleton
) where

import           Sort
import           SubtypeRelation (SubtypeRelation)
import qualified SubtypeRelation as R

import           Data.Constructor
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Maybe
import           Data.Foldable

import           Control.Arrow

data Signature = Signature [Sort] Sort deriving (Show,Eq)
data Context = Context
  { signatures :: HashMap Constructor [Signature]
  , sorts :: HashMap Sort [(Constructor,Signature)]
  , subtypes :: SubtypeRelation
  } deriving (Show,Eq)

empty :: Context
empty = Context M.empty M.empty R.empty

fromList :: [(Constructor,[Sort],Sort)] -> Context
fromList = foldl (\ctx (c,ss,s) -> insertSignature c (Signature ss s) ctx) empty

insertSignature :: Constructor -> Signature -> Context -> Context
insertSignature con sig@(Signature _ sort) (Context cons sorts sub) =
  Context { signatures = M.insertWith (\[v] l -> v:l) con [sig] cons
          , sorts = M.insertWith (\[v] l -> v:l) sort [(con,sig)] sorts
          , subtypes = sub
          }

insertSubtype :: Sort -> Sort -> Context -> Context
insertSubtype ty1 ty2 ctx = ctx { subtypes = R.insert ty1 ty2 (subtypes ctx) }

subtype :: Context -> Sort -> Sort -> Bool
subtype Context {..} = R.subtype subtypes

lub :: Context -> Sort -> Sort -> Sort
lub Context {..} = R.lub subtypes

glb :: Context -> Sort -> Sort -> Sort
glb Context {..} = R.glb subtypes

lookupCons :: Context -> Constructor -> [Signature]
lookupCons Context {..} c = fold $ M.lookup c signatures

lookupSort :: Context -> Sort -> [(Constructor,Signature)]
lookupSort Context {..} s0 = do
  s <- R.lower subtypes s0
  case s of
    Bottom -> []
    Top -> error "Calculating inhabitants from sort top is not allowed"
    List a -> [("Cons", Signature [a, List a] (List a)), ("Nil", Signature [] (List a))]
    Option a -> [("Some", Signature [a] (Option a)), ("None", Signature [] (Option a))]
    Tuple as -> [("", Signature as (Tuple as))]
    Lexical -> [("", Signature [] Lexical)]
    Numerical -> [("", Signature [] Numerical)]
    Sort _ -> fromMaybe [] (M.lookup s sorts)
              
isLexical :: Context -> Sort -> Bool
isLexical ctx = subtype ctx Lexical

isList :: Context -> Sort -> Bool
isList ctx = subtype ctx (List Bottom)

isSingleton :: Context -> Sort -> Bool
isSingleton ctx s = case s of
  Bottom -> True
  Numerical -> False
  Lexical -> False
  Sort _ -> length (lookupSort ctx s) == 1
  Option _ -> False
  List _ -> False
  Tuple ss -> all (isSingleton ctx) ss
  Top -> False


class Arrow c => HasContext c where
  getContext :: c () Context
