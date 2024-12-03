module Utils
  ( readFileLines,
    parseInput,
    isOrdered,
  )
where

readFileLines :: FilePath -> IO [String]
readFileLines filePath = do
  contents <- readFile filePath
  return $ lines contents

parseInput :: FilePath -> IO [[Int]]
parseInput filePath = do
  inputLines <- readFileLines filePath
  return $ map parseLine inputLines

parseLine :: String -> [Int]
parseLine line = map read $ words line

isOrdered :: [Int] -> Bool
isOrdered [] = True
isOrdered [_] = True
isOrdered (x : y : xs)
  | y > x = all (\(a, b) -> b > a) (zip (x : y : xs) (y : xs))
  | y < x = all (\(a, b) -> b < a) (zip (x : y : xs) (y : xs))
  | otherwise = False
