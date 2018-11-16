module SubtypeRelation(SubtypeRelation,empty,insert,subtype,lower,lub,glb) where

import           Sort
import           Utils

import           Data.Graph.Inductive (Node)
import qualified Data.Graph.Inductive as G
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Maybe

data SubtypeRelation = SubtypeRelation Node (HashMap Sort Node) (Gr Sort ()) deriving (Show,Eq)

empty :: SubtypeRelation
empty = SubtypeRelation 0 M.empty G.empty

insert :: Sort -> Sort -> SubtypeRelation -> SubtypeRelation
insert s1 s2 rel0 =
  let (n1, rel1) = insertSort s1 rel0
      (n2, rel2) = insertSort s2 rel1
  in withGraph (G.tc . G.insEdge (n1,n2,())) rel2

withGraph :: (Gr Sort () -> Gr Sort ()) -> SubtypeRelation -> SubtypeRelation
withGraph f (SubtypeRelation m nodes rel) = SubtypeRelation m nodes (f rel)
{-# INLINE withGraph #-}

insertSort :: Sort -> SubtypeRelation -> (Node,SubtypeRelation)
insertSort s rel@(SubtypeRelation m nodes gr) =
  case M.lookup s nodes of
    Just n -> (n,rel)
    Nothing -> (m, SubtypeRelation (m+1) (M.insert s m nodes) (G.insNode (m,s) gr)) 

lookupSort :: SubtypeRelation -> Sort -> Maybe Node
lookupSort (SubtypeRelation _ nodes _) s = M.lookup s nodes

subtype :: SubtypeRelation -> Sort -> Sort -> Bool
subtype rel@(SubtypeRelation _ _ gr) s1 s2 = case (s1,s2) of
  (Bottom,_) -> True
  (_,Top) -> True
  (Lexical,Lexical) -> True
  (Numerical,Numerical) -> True
  (List x,List y) -> subtype rel x y
  (Option x,Option y) -> subtype rel x y
  (Tuple xs,Tuple ys)
    | eqLength xs ys -> and (zipWith (subtype rel) xs ys)
    | otherwise -> False
  (x,Sort y)
    | x == Sort y -> True
    | otherwise -> fromMaybe False $ do
        n1 <- lookupSort rel x 
        n2 <- lookupSort rel (Sort y)
        return $ G.hasEdge gr (n1, n2)
  (_,_) -> False

lub :: SubtypeRelation -> Sort -> Sort -> Sort
lub rel s1 s2 = case (s1,s2) of
  (Bottom,_) -> s2
  (_,Bottom) -> s1
  (Top,_) -> Top 
  (List x,List y) -> List (lub rel x y)
  (Option x,Option y) -> Option (lub rel x y)
  (Tuple xs,Tuple ys) -> Tuple (zipWith (lub rel) xs ys)
  (Lexical,Lexical) -> Lexical
  (Numerical,Numerical) -> Numerical
  (Sort _,_)
    | subtype rel s1 s2 -> s2
    | subtype rel s2 s1 -> s1
  (_,Sort _)
    | subtype rel s1 s2 -> s2
    | subtype rel s2 s1 -> s1
  _ -> Top

glb :: SubtypeRelation -> Sort -> Sort -> Sort
glb rel s1 s2 = case (s1,s2) of
  (Bottom,_) -> Bottom
  (_,Bottom) -> Bottom
  (Top,_) -> s2
  (_,Top) -> s1
  (List x,List y) -> List (glb rel x y)
  (Option x,Option y) -> Option (glb rel x y)
  (Tuple xs,Tuple ys) -> Tuple (zipWith (glb rel) xs ys)
  (Lexical,Lexical) -> Lexical
  (Numerical,Numerical) -> Numerical
  (Sort _,_)
    | subtype rel s1 s2 -> s1
    | subtype rel s2 s1 -> s2
  (_,Sort _)
    | subtype rel s1 s2 -> s1
    | subtype rel s2 s1 -> s2
  _ -> Bottom

lower :: SubtypeRelation -> Sort -> [Sort]
lower rel@(SubtypeRelation _ _ gr) s = case s of
  Bottom -> [Bottom]
  Top -> error "lower set of top is unsupported"
  Lexical -> return Lexical
  Numerical -> return Numerical
  List x -> List <$> lower rel x
  Option x -> Option <$> lower rel x
  Tuple xs -> Tuple <$> permutations (lower rel <$> xs)
  Sort _ -> fromMaybe (return s) $ do
    n <- lookupSort rel s
    xs <- traverse (G.lab gr) (G.pre gr n)
    return (s:xs)

