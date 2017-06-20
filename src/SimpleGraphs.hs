module SimpleGraphs where

import Control.Monad (replicateM)
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (sp, spLength)
import Data.Graph.Inductive.Query.MST (msTree)
import System.IO (Handle, hGetLine)

data EdgeSpec = EdgeSpec
  { fromNode :: Int
  , toNode :: Int
  , distance :: Int
  }

newtype NodeLabel = NodeLabel Int 
type Distance = Int

solveSP :: Handle -> IO ()
solveSP handle = do
  inputs <- readInputs handle
  start <- read <$> hGetLine handle
  end <- read <$> hGetLine handle
  let gr = genGraph inputs
  print $ sp start end gr
  print $ spLength start end gr

solveMST :: Handle -> IO ()
solveMST handle = do
  inputs <- readInputs handle
  let gr = genUndirectedGraph inputs
  print $ msTree gr

readInputs :: Handle -> IO (Int, [EdgeSpec])
readInputs handle = do
  numNodes <- read <$> hGetLine handle
  numEdges <- (read <$> hGetLine handle)
  edges <- replicateM numEdges (readEdge handle)
  return (numNodes, edges)

readEdge :: Handle -> IO EdgeSpec
readEdge handle = do
  input <- hGetLine handle
  let [f_s, t_s, d_s] = words input
  return $ EdgeSpec (read f_s) (read t_s) (read d_s)

genGraph :: (Int, [EdgeSpec]) -> Gr NodeLabel Distance
genGraph (numNodes, edgeSpecs) = mkGraph nodes edges
  where
    nodes = (\i -> (i, NodeLabel i)) 
      <$> [1..numNodes]
    edges = (\es -> (fromNode es, toNode es, distance es))
      <$> edgeSpecs

genUndirectedGraph :: (Int, [EdgeSpec]) -> Gr NodeLabel Distance
genUndirectedGraph (numNodes, edgeSpecs) = mkGraph nodes edges
  where
    nodes = (\i -> (i, NodeLabel i)) 
      <$> [1..numNodes]
    edges = concatMap (\es -> 
      [(fromNode es, toNode es, distance es), (toNode es, fromNode es, distance es)])
      edgeSpecs
