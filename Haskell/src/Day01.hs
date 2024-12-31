module Day01 (solveDay01) where

import Data.List (sort)
import Utils (readFileLines)

solveDay01 :: FilePath -> IO (Int, Int)
solveDay01 filePath = do
  input <- parseInput filePath
  let resultP1 = solveDay01p1 input
      resultP2 = solveDay01p2 input
  return (resultP1, resultP2)


type Locations = (Int, Int)


solveDay01p1 :: [Locations] -> Int
solveDay01p1 locs =
  let (leftList, rightList) = unzip locs
      sortedFirst = sort leftList
      sortedSecond = sort rightList
      paired = zip sortedFirst sortedSecond
   in sum $ map (\(x, y) -> abs (x - y)) paired

solveDay01p2 :: [Locations] -> Int
solveDay01p2 locs =
  let (leftList, rightList) = unzip locs
    in sum $ map (\x -> x * length (filter (== x) rightList)) leftList


parseInput :: FilePath -> IO [Locations]
parseInput filePath = do
  inputLines <- readFileLines filePath
  return $ map parseLine inputLines

parseLine :: String -> Locations
parseLine line =
  let [id1, id2] = words line
   in (read id1, read id2)
