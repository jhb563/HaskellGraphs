module ComplexGraphs where

import Data.Graph.Inductive.Graph (LNode, LEdge, mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.MaxFlow (maxFlow)
import Data.Set

data NodeLabel a b = 
  LeftNode a |
  RightNode b |
  SourceNode |
  SinkNode

findMaxMatching :: (a -> b -> Bool) -> Set a -> Set b -> Int
findMaxMatching predicate as bs = maxFlow graph source sink
  where
    (graph, source, sink) = totalGraph predicate as bs

totalGraph :: (a -> b -> Bool) -> Set a -> Set b 
  -> (Gr (NodeLabel a b) Int, Int, Int)
totalGraph predicate as bs = (mkGraph allNodes allEdges, sourceIndex, sinkIndex)
  where
    sz_a = size as
    sz_b = size bs
    (leftNodes, rightNodes, middleEdges) = createInnerGraph predicate as bs
    sourceIndex = sz_a + sz_b + 1
    sourceNode = (sourceIndex, SourceNode)
    sinkIndex = (sz_a + sz_b + 2)
    sinkNode = (sinkIndex, SinkNode)
    sourceEdges = [(sourceIndex, lIndex, 1) | lIndex <- fst <$> leftNodes]
    sinkEdges = [(rIndex, sinkIndex, 1) | rIndex <- fst <$> rightNodes]
    allNodes = sourceNode : sinkNode : (leftNodes ++ rightNodes)
    allEdges = sourceEdges ++ middleEdges ++ sinkEdges

createInnerGraph 
  :: (a -> b -> Bool) 
  -> Set a 
  -> Set b 
  -> ([LNode (NodeLabel a b)], [LNode (NodeLabel a b)], [LEdge Int])
createInnerGraph predicate as bs = (aNodes, bNodes, edges)
  where
    sz_a = size as
    sz_b = size bs
    indexedAs = zip [1..sz_a] (elems as)
    aNodes = zip [1..sz_a] (LeftNode <$> (elems as))
    indexedBs = zip [(sz_a + 1)..(sz_a + sz_b)] (elems bs)
    bNodes = zip [(sz_a + 1)..(sz_a + sz_b)] (RightNode <$> (elems bs))
    nodesAreConnected (_, aItem) (_, bItem) = predicate aItem bItem
    edges = [(fst aN, fst bN, 1) | aN <- indexedAs, bN <- indexedBs, nodesAreConnected aN bN]
