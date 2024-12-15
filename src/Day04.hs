module Day04 (solveDay04) where

import Utils (readFileLines)

solveDay04 :: FilePath -> IO (Int, Int)
solveDay04 filePath = do
  input <- readFileLines filePath
  let resultP1 = solveDay04p1 input
      resultP2 = solveDay04p2 input
  return (resultP1, resultP2)

solveDay04p1 :: [String] -> Int
solveDay04p1 _ = 0

solveDay04p2 :: [String] -> Int
solveDay04p2 _ = 0
