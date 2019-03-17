{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SortContext(
  Context, HasContext(..), Sort(..), Signature(..), SortId(..), empty, signatures, sorts,
  fromList, insertSignature, insertSubtype, subtype, lookupSort, lookupCons,
  lub, glb, isLexical, isList, getListElem, isSingleton, isNumerical, isTuple, filterInconsistentConstructors
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
    Top -> error "Calculating inhabitants of sort top is not allowed"
    List a -> [("Cons", Signature [a, List a] (List a)), ("Nil", Signature [] (List a))]
    Option a -> [("Some", Signature [a] (Option a)), ("None", Signature [] (Option a))]
    Tuple as -> [("", Signature as (Tuple as))]
    Lexical -> [("", Signature [] Lexical)]
    Numerical -> [("", Signature [] Numerical)]
    Sort _ -> fromMaybe [] (M.lookup s sorts)
              
isLexical :: Context -> Sort -> Bool
isLexical ctx = subtype ctx Lexical

isNumerical :: Context -> Sort -> Bool
isNumerical ctx = subtype ctx Numerical

isList :: Context -> Sort -> Bool
isList ctx = subtype ctx (List Bottom)

getListElem :: Context -> Sort -> Sort
getListElem _ (List s) = s
getListElem ctx s@(Sort _) = foldl (lub ctx) Bottom $ map (getListElem ctx) $ filter (not . isSort) $ tail $ R.lower (subtypes ctx) s
getListElem _ Bottom = Bottom
getListElem _ Numerical = Bottom
getListElem _ Lexical = Bottom
getListElem _ (Option _) = Bottom
getListElem _ (Tuple _) = Bottom
getListElem _ Top = Top

isSort :: Sort -> Bool
isSort (Sort _) = True
isSort _ = False

isTuple :: Context -> Int -> Sort -> Bool
isTuple ctx i = subtype ctx (Tuple (replicate i Bottom))

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

filterInconsistentConstructors :: Context -> HashMap Constructor [Signature]
filterInconsistentConstructors ctx = M.filter (\sigs -> any (\(Signature _ r1) -> any (\(Signature _ r2) -> r1 /= r2) sigs) sigs) (signatures ctx)

class Arrow c => HasContext c where
  getContext :: c () Context
