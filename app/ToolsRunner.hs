module Main where

import System.IO (stdin)

import Tools (solveToolsBest)

main :: IO ()
main = do
  res <- solveToolsBest stdin
  print res
