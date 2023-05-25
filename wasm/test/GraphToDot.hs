module GraphToDot where

import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow (CFG(..))
import           Data.Graph.Inductive.Graph (labNodes,labEdges)
--import           Data.Graph.Inductive.PatriciaTree

graphToDot :: (stmt -> String) -> CFG stmt -> String
graphToDot f (CFG graph) =
    let nodes = labNodes graph in
    let edges = labEdges graph in
    "digraph { " ++ concatMap toNode nodes ++ concatMap toEdge edges ++ "}"

    where
        toNode (n,inst) = (show n) ++"[label=\"" ++ (f inst) ++ "\"]; "
        toEdge (from,to,_) = (show from) ++ " -> " ++ (show to) ++ "; "
