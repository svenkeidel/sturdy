{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
module Props.ControlFlow.Prop where

import WhileLanguage
import qualified Vals.Concrete.Val as Concrete

import Data.GaloisConnection
import Data.Order
import Data.Powerset
import Data.Hashable

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as Set

import GHC.Generics (Generic)


---------------
-- concrete
---------------

data TraceElem v = TrAssign Label v | TrIf Label v
  deriving (Show,Eq,Generic,Functor)

trLabel :: TraceElem v -> Label
trLabel (TrAssign l _) = l
trLabel (TrIf l _) = l

instance Hashable v => Hashable (TraceElem v)

instance PreOrd v => PreOrd (TraceElem v) where
  (TrAssign l1 v1) ⊑ (TrAssign l2 v2) = l1==l2 && v1 ⊑ v2
  (TrIf l1 v1) ⊑ (TrIf l2 v2) = l1==l2 && v1 ⊑ v2
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

data CFGNode v = CFGAssign Label v | CFGIf Label v
  deriving (Show,Eq)

labelCFG :: CFGNode v -> Label
labelCFG (CFGAssign l _) = l
labelCFG (CFGIf l _) = l

type CFGNodes v = Map Label (CFGNode v)

data CFG v = CFG {entry :: Set Label, exit :: Set Label, nodes :: CFGNodes v, edges :: Set (Label,Label)}
  deriving (Show,Eq)

singletonCFG :: CFGNode v -> CFG v
singletonCFG n = CFG (Set.singleton l) (Set.singleton l) (M.singleton l n) Set.empty
  where l = labelCFG n

instance PreOrd v => PreOrd (CFGNode v) where
  (CFGAssign l1 v1) ⊑ (CFGAssign l2 v2) = l1==l2 && v1⊑v2
  (CFGIf l1 v1) ⊑ (CFGIf l2 v2) = l1==l2 && v1⊑v2
  _ ⊑ _ = False

  (CFGAssign l1 v1) ≈ (CFGAssign l2 v2) = l1==l2 && v1≈v2
  (CFGIf l1 v1) ≈ (CFGIf l2 v2) = l1==l2 && v1≈v2
  _ ≈ _ = False

instance PreOrd v => PreOrd (CFG v) where
  (CFG en1 ex1 no1 ed1) ⊑ (CFG en2 ex2 no2 ed2) =
    en1 ⊑ en2 &&
    ex1 ⊑ ex2 &&
    no1 ⊑ no2 &&
    all (\e1 -> Set.member e1 ed2) ed1

partialJoin :: Complete v => CFGNode v -> CFGNode v -> CFGNode v
(CFGAssign l1 v1) `partialJoin` (CFGAssign l2 v2) | l1 == l2 = CFGAssign l1 $ v1 ⊔ v2
(CFGIf l1 v1) `partialJoin` (CFGIf l2 v2) | l1 == l2 = CFGIf l1 $ v1 ⊔ v2

instance Complete v => Complete (CFG v) where
  (CFG en1 ex1 no1 ed1) ⊔ (CFG en2 ex2 no2 ed2) =
      CFG (en1 `Set.union` en2) (ex1 `Set.union` ex2) (M.unionWith partialJoin no1 no2) (ed1 `Set.union` ed2)

instance Complete v => Monoid (CFG v) where
  mempty = CFG Set.empty Set.empty M.empty Set.empty

  mappend g1 g2 | M.null (nodes g1) = g2
  mappend g1 g2 | M.null (nodes g2) = g1
  mappend (CFG en1 ex1 no1 ed1) (CFG en2 ex2 no2 ed2) =
    CFG en1 ex2 (M.unionWith partialJoin no1 no2) (ed1 `Set.union` ed2 `Set.union` connects)
    where
      connects :: Set (Label,Label)
      connects = Set.fromList [(ex,en) | ex <- Set.toList ex1, en <- Set.toList en2]

instance PreOrd v => LowerBounded (CFG v) where
  bottom = CFG Set.empty Set.empty M.empty Set.empty

type AProp v = CFG v
initAProp :: AProp v
initAProp = CFG Set.empty Set.empty M.empty Set.empty

pushNode :: Complete v => CFGNode v -> AProp v -> AProp v
pushNode node p = p `mappend` singletonCFG node


---------------
-- Galois
---------------

instance (Galois (Pow Concrete.Val) v,Complete v,LowerBounded v) => Galois (Pow [TraceElem (Pow Concrete.Val)]) (CFG v) where
  alpha = lifted lift
    where lift elems = CFG (entry elems) (exit elems) (nodes elems) (edges elems)

          entry [] = Set.empty
          entry (e:_) = Set.singleton $ trLabel e

          exit [] = Set.empty
          exit es = Set.singleton $ trLabel $ last es

          nodes elems = M.fromListWith partialJoin $ map mkNode elems

          edges [] = Set.empty
          edges [e] = Set.empty
          edges (e1:e2:es) = Set.insert (trLabel e1,trLabel e2) $ edges (e2:es)

          mkNode (TrAssign l v) = (l,CFGAssign l $ alpha v)
          mkNode (TrIf l v) = (l, CFGIf l $ alpha v)

  gamma  (CFG en ex nodes edges) = foldr (union . walk []) empty en
    where walk :: [TraceElem (Pow Concrete.Val)] -> Label -> Pow [TraceElem (Pow Concrete.Val)]
          walk path n =
            let end = if Set.member n ex then singleton $ reverse path else empty
                outs = Set.map snd $ Set.filter (\(m,_) -> m == n) edges
            in end `union` foldr (union . walk (mkTraceElem n : path)) empty outs

          mkTraceElem :: Label -> TraceElem (Pow Concrete.Val)
          mkTraceElem n = case nodes M.! n of
            CFGAssign l v -> TrAssign l $ gamma v
            CFGIf l v -> TrIf l $ gamma v

