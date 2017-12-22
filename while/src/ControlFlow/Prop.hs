{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
module ControlFlow.Prop where

import WhileLanguage
import qualified ConcreteSemantics as Concrete

import Data.Text (Text)
import Data.GaloisConnection
import Data.Order
import Data.Powerset
import Data.Hashable

import Data.Map (Map)
import qualified Data.Map as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Set (Set)
import qualified Data.Set as Set

import GHC.Generics (Generic)


---------------
-- concrete
---------------

data TraceElem v = TrAssign Text v | TrIf v
  deriving (Show,Eq,Generic,Functor)

instance Hashable v => Hashable (TraceElem v)

instance PreOrd v => PreOrd (TraceElem v) where
  (TrAssign t1 v1) ⊑ (TrAssign t2 v2) = t1==t2 && v1 ⊑ v2
  (TrIf v1) ⊑ (TrIf v2) = v1 ⊑ v2
  _ ⊑ _ = False

type CProp = [TraceElem Concrete.Val]
initCProp :: CProp
initCProp = []

type LiftedCProp = Pow [TraceElem (Pow Concrete.Val)]
liftCProp :: CProp -> LiftedCProp
liftCProp = singleton . map (fmap singleton)


---------------
-- abstract
---------------

data CFGNode v = CFGAssign Text v | CFGIf v
  deriving (Show,Eq)

type CFGNodes v = IntMap (CFGNode v)

data CFG v = CFG {entry :: Set Int, exit :: Set Int, nodes :: CFGNodes v, edges :: Set (Int,Int)}
  deriving (Show,Eq)

instance PreOrd v => PreOrd (CFGNode v) where
  (CFGAssign t1 v1) ⊑ (CFGAssign t2 v2) = t1==t2 && v1⊑v2
  (CFGIf v1) ⊑ (CFGIf v2) = v1⊑v2
  _ ⊑ _ = False

  (CFGAssign t1 v1) ≈ (CFGAssign t2 v2) = t1==t2 && v1≈v2
  (CFGIf v1) ≈ (CFGIf v2) = v1≈v2
  _ ≈ _ = False

instance PreOrd v => PreOrd (CFG v) where
  -- TODO: implement proper subgraph check
  (CFG en1 ex1 no1 ed1) ⊑ (CFG en2 ex2 no2 ed2) =
    en1 ⊑ en2 &&
    ex1 ⊑ ex2 &&
    no1 ⊑ no2 &&
    ed1 ⊑ ed2

instance Complete v => Complete (CFG v) where
  (CFG en1 ex1 no1 ed1) ⊔ (CFG en2 ex2 no2 ed2) =
      CFG (en1 `Set.union` en2) (ex1 `Set.union` ex2) (IM.unionWith partialJoin no1 no2) (ed1 `mappend` ed2)
    where (CFGAssign t1 v1) `partialJoin` (CFGAssign t2 v2) | t1 == t2 = CFGAssign t1 $ v1 ⊔ v2
          (CFGIf v1) `partialJoin` (CFGIf v2) = CFGIf $ v1 ⊔ v2

instance PreOrd v => LowerBounded (CFG v) where
  bottom = CFG Set.empty Set.empty IM.empty Set.empty

instance (Galois (Pow Concrete.Val) v,Complete v,LowerBounded v) => Galois (Pow [TraceElem (Pow Concrete.Val)]) (CFG v) where
  alpha = lifted lift
    where lift elems = CFG (Set.singleton 1) (Set.singleton $ length elems) (nodes elems) (edges elems)
          nodes elems = foldl (\m (elem,i) -> IM.insert i (mkNode elem) m) IM.empty (zip elems [1..])
          edges elems = foldr (\n -> Set.insert (n,n+1)) Set.empty [1..(length elems - 1)]
          mkNode (TrAssign x v) = CFGAssign x $ alpha v
          mkNode (TrIf v) = CFGIf $ alpha v

  gamma  (CFG en ex nodes edges) = foldr (union . walk []) empty en
    where walk :: [TraceElem (Pow Concrete.Val)] -> Int -> Pow [TraceElem (Pow Concrete.Val)]
          walk path n =
            let end = if Set.member n ex then singleton $ reverse path else empty
                outs = Set.map snd $ Set.filter (\(m,_) -> m == n) edges
            in end `union` foldr (union . walk (mkTraceElem n : path)) empty outs

          mkTraceElem n = case nodes IM.! n of
            CFGAssign t v -> TrAssign t $ gamma v
            CFGIf v -> TrIf $ gamma v


-- findBackwards :: (CFGNode -> Bool) -> CFG -> Maybe CFGNode
-- findBackwards f (CFG exits nodes edges) = fmap fromJust $ find isJust $ (map (fst . go Set.empty) exits)
--   where go :: Set Int -> Int -> (Maybe CFGNode, Set Int)
--         go seen current | f node = (Just node,seen)
--                         | Set.member current seen = (Nothing,seen)
--                         | otherwise = foldl findNext (Nothing,Set.insert current seen) predecessors
--           where node = nodes IM.! current
--                 predecessors = map fst $ filter (\(from,to) -> to == current) edges
--                 findNext (Just n,seen) pred = (Just n,seen)
--                 findNext (Nothing,seen) pred = go seen pred

-- subgraph :: CFG -> -> CFG -> Bool
-- subgraph (CFG ex1 nodes1 edges1) start1 (CFG ex2 nodes2 edges2) start2 =
--   go (IM.singleton start1 start2) Set.empty
--   where go :: IntMap Int -> Set Int -> Int -> Bool
--         go alias seen current
--           | Set.member current seen = true
--           | otherwise = fromMaybe false $ do
--             other <- IM.lookup current alias
--             currentNode <- M.lookup current nodes1
--             otherNode <- M.lookup other nodes2
--             if currentNode /= otherNode
--               then return false
--               else do
--                 let newseen = Set.insert current seen
--                 currentOut = filter () edges1


-- newtype CFGCache = CFGCache (Map Statement Int)
-- data CFGBuilder v = CFGBuilder { buildCFG :: Int -> CFG v }
--
-- instance PreOrd CFGCache where
--   (CFGCache c1) ⊑ (CFGCache c2) =
--     M.keysSet c1 `Set.isSubsetOf` M.keysSet c2
--       && all (\k -> (c1 M.! k) ⊑ (c2 M.! k)) (M.keys c1)
--
-- instance Complete CFGCache where
--   (CFGCache c1) ⊔ (CFGCache c2) =
--     CFGCache $ M.unionWith (\a b -> if a == b then a else error "cannot join differing caches") c1 c2
--
-- fresh :: Int -> (CFGNodes v) -> Int
-- fresh i no
--   | IM.null no = i
--   | otherwise = succ (fst (IM.findMax no))
--
-- instance Eq v => Eq (CFGBuilder v) where
--   b1 == b2 = buildCFG b1 1 == buildCFG b2 1
--
-- instance Eq v => Monoid (CFGBuilder v) where
--   mempty = CFGBuilder $ \_ -> CFG Set.empty Set.empty IM.empty Set.empty
--   mappend b1 b2
--     | b1 == mempty = b2
--     | b2 == mempty = b1
--     | otherwise = CFGBuilder $ \i ->
--       let CFG en1 ex1 no1 ed1 = buildCFG b1 i
--           CFG en2 ex2 no2 ed2 = buildCFG b2 (fresh i no1)
--       in CFG en1 ex2 (IM.union no1 no2) $
--         foldr Set.insert (ed1 `mappend` ed2) [(ex,en) | ex <- Set.toList ex1, en <- Set.toList en2]
--
-- instance PreOrd v => PreOrd (CFGBuilder v) where
--   b1 ⊑ b2 = (buildCFG b1 1) ⊑ (buildCFG b2 1)
--   (≈) = (==)
--
-- instance Complete (CFGBuilder v) where
--   b1 ⊔ b2 = CFGBuilder $ \i ->
--     let CFG ex1 no1 ed1 = buildCFG b1 i
--         CFG ex2 no2 ed2 = buildCFG b2 i
--     in CFG (ex1 `mappend` ex2) (IM.union no1 no2) (ed1 `mappend` ed2)
--
--
--
-- type AProp v = (CFGBuilder v,CFGCache)
-- initAProp :: AProp v
-- initAProp = (mempty, CFGCache M.empty)
