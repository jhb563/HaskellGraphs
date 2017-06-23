module Tools where

import           Control.Monad (replicateM)
import           Data.List (delete)
import           Data.List.Split (splitOn)
import           System.IO (hGetLine, Handle)

import           ComplexGraphs (findMaxMatching)

readInput :: Handle -> IO ([String], [(String, [String])])
readInput handle = do
  numTools <- read <$> hGetLine handle
  numPeople <- read <$> hGetLine handle
  tools <- replicateM numTools (hGetLine handle)
  people <- replicateM numPeople (readPersonLine handle)
  return (tools, people)
 
readPersonLine :: Handle -> IO (String, [String]) 
readPersonLine handle = do
  line <- hGetLine handle
  let components = splitOn ", " line
  return (head components, tail components)

solveToolsGreedy :: Handle -> IO Int
solveToolsGreedy handle = do
  (tools, personMap) <- readInput handle
  return $ findMaxMatchingGreedy tools personMap
 
findMaxMatchingGreedy :: [String] -> [(String, [String])] -> Int 
findMaxMatchingGreedy [] _ = 0 -- No more tools to match
findMaxMatchingGreedy (tool : rest) personMap = case break (containsTool tool) personMap of
  (allPeople, []) -> findMaxMatchingGreedy rest personMap -- Can't match this tool
  (somePeople, (_ : otherPeople)) -> 1 + findMaxMatchingGreedy rest (somePeople ++ otherPeople)

containsTool :: String -> (String, [String]) -> Bool
containsTool tool pair = tool `elem` (snd pair)

solveToolsSlow :: Handle -> IO Int
solveToolsSlow handle = do
  (tools, personMap) <- readInput handle
  return $ findMaxMatchingSlow tools personMap

findMaxMatchingSlow :: [String] -> [(String, [String])] -> Int
findMaxMatchingSlow [] _ = 0
findMaxMatchingSlow allTools@(tool : rest) personMap = 
  case break (containsTool tool) personMap of
    (allPeople, []) -> findMaxMatchingGreedy rest personMap -- Can't match this tool
    (somePeople, (chosen : otherPeople)) -> max useIt loseIt
      where
        useIt = 1 + findMaxMatchingSlow rest (somePeople ++ otherPeople)
        loseIt = findMaxMatchingSlow allTools newList
        newList = somePeople ++ (modifiedChosen : otherPeople)
        modifiedChosen = dropTool tool chosen

dropTool :: String -> (String, [String]) -> (String, [String])
dropTool tool (name, validTools) = (name, delete tool validTools)

solveToolsBest :: Handle -> IO Int
solveToolsBest handle = do
  (tools, personMap) <- readInput handle
  return $ findMaxMatchingBest tools personMap

findMaxMatchingBest :: [String] -> [(String, [String])] -> Int
findMaxMatchingBest tools personMap = findMaxMatching containsTool tools personMap
