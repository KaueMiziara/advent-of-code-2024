module Day02 (solveDay02) where

import Utils (readFileLines, isOrdered)

solveDay02 :: FilePath -> IO (Int, Int)
solveDay02 filePath = do
  input <- parseInput filePath
  let resultP1 = solveDay02p1 input
      resultP2 = solveDay02p2 input
  return (resultP1, resultP2)


solveDay02p1 :: [[Int]] -> Int
solveDay02p1 = length . filter (\report -> isOrdered report && isSafe report)

solveDay02p2 :: [[Int]] -> Int
solveDay02p2 _ = 0


isSafe :: [Int] -> Bool
isSafe [] = True
isSafe [_] = True
isSafe (x:y:xs) = 
  let diff = abs (y - x)
  in diff >= 1 && diff <= 3 && isSafe (y:xs)

parseInput :: FilePath -> IO [[Int]]
parseInput filePath = do
  inputLines <- readFileLines filePath
  return $ map parseLine inputLines

parseLine :: String -> [Int]
parseLine line = map read $ words line
