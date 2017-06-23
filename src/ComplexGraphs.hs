module ComplexGraphs where

import Data.Graph.Inductive.Graph (LNode, LEdge, mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.MaxFlow (maxFlow)

data NodeLabel a b = 
  LeftNode a |
  RightNode b |
  SourceNode |
  SinkNode

findMaxMatching :: (a -> b -> Bool) -> [a] -> [b] -> Int
findMaxMatching predicate as bs = maxFlow graph source sink
  where
    (graph, source, sink) = totalGraph predicate as bs

totalGraph :: (a -> b -> Bool) -> [a] -> [b] 
  -> (Gr (NodeLabel a b) Int, Int, Int)
totalGraph predicate as bs = (mkGraph allNodes allEdges, sourceIndex, sinkIndex)
  where
    sz_a = length as
    sz_b = length bs
    (leftNodes, rightNodes, middleEdges) = createInnerGraph predicate as bs
    sourceIndex = sz_a + sz_b + 1
    sinkIndex = (sz_a + sz_b + 2)
    sourceNode = (sourceIndex, SourceNode)
    sinkNode = (sinkIndex, SinkNode)
    sourceEdges = [(sourceIndex, lIndex, 1) | lIndex <- fst <$> leftNodes]
    sinkEdges = [(rIndex, sinkIndex, 1) | rIndex <- fst <$> rightNodes]
    allNodes = sourceNode : sinkNode : (leftNodes ++ rightNodes)
    allEdges = sourceEdges ++ middleEdges ++ sinkEdges

createInnerGraph 
  :: (a -> b -> Bool) 
  -> [a]
  -> [b]
  -> ([LNode (NodeLabel a b)], [LNode (NodeLabel a b)], [LEdge Int])
createInnerGraph predicate as bs = (aNodes, bNodes, edges)
  where
    sz_a = length as
    sz_b = length bs
    aNodes = zip [1..sz_a] (LeftNode <$> as)
    bNodes = zip [(sz_a + 1)..(sz_a + sz_b)] (RightNode <$> bs)
    indexedAs = zip [1..sz_a] as
    indexedBs = zip [(sz_a + 1)..(sz_a + sz_b)] bs
    nodesAreConnected (_, aItem) (_, bItem) = predicate aItem bItem
    edges = [(fst aN, fst bN, 1) | aN <- indexedAs, bN <- indexedBs, nodesAreConnected aN bN]
